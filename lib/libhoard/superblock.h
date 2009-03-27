// -*- C++ -*-

#ifndef _SUPERBLOCK_H_
#define _SUPERBLOCK_H_

#include "sassert.h"
#include "check.h"
#include "superblockheader.h"
#include "hldefines.h"


/**
 * @class Superblock
 * @brief A superblock (group of pages) for memory management.
 */

template <class H, int SuperblockSize>

class Superblock : public SuperblockHeader<H> {

public:

  typedef Superblock<H, SuperblockSize> SuperblockType;
  typedef SuperblockHeader<H> Header;

  inline Superblock (size_t sz)
    : Header (sz, BufferSize)
  {
    assert (sz <= BufferSize);

    // Verify that the header is double-aligned (so that _buf will be too).
    HL::sassert<(sizeof(H) % sizeof(double) == 0)> verifyHeaderAligned;

    // Sanity check: ensure that this superblock is the correct size!
    HL::sassert<(sizeof(Superblock) <= SuperblockSize)> verifySize;

#if !defined(HOARD_NO_REAPS) || !(HOARD_NO_REAPS)
    Header::_position = reinterpret_cast<char *>(_buf);
#else
    // Add every block to the freelist (note: in reverse order).
    Header::_position = NULL;
    Header::_objectsFree = 0;
    char * ptr = reinterpret_cast<char *>(_buf);
    char * p = ptr + Header::getObjectSize() + sz;
    while (reinterpret_cast<size_t>(p) <=
	   reinterpret_cast<size_t>(&_buf[BufferSize])) {
      Header::free (ptr);
      ptr = p;
      p = ptr + Header::getObjectSize() + sz;
    }
#endif

    assert (sizeof(*this) == SuperblockSize);
    assert (Header::getSize(0) == sz);
    ////////    assert (Header::isValidSuperblock());
    // FIX ME^^^ disabled for now

    // Ensure that _buf is double-word aligned.
    assert (((size_t) _buf & (sizeof(double) - 1)) == 0);
  }

  MALLOC_FUNCTION INLINE void * malloc (size_t sz) {
    Check<Superblock, MyChecker> check (this);
    assert (sz <= Header::getObjectSize());
#if !defined(HOARD_NO_REAPS) || !(HOARD_NO_REAPS)
    if (Header::_position) {
      char * ptr = Header::_position;
      Header::_position = ptr + Header::getObjectSize();
      if (reinterpret_cast<size_t>(Header::_position) >
	  reinterpret_cast<size_t>(&_buf[BufferSize])) {
	// We're out of space.
	Header::_position = NULL;
      } else {
	Header::decObjectsFree();
	return ptr;
      }
    }
#endif
    // Check the freelist.
    void * ptr = Header::malloc (sz);
    assert (ptr || (Header::getObjectsFree() == 0));
    return ptr;
  }

  INLINE void free (void * ptr) {
    Check<Superblock, MyChecker> check (this);
#ifndef NDEBUG
    if (!checkInRange (ptr)) {
      return;
    }
#endif
    Header::free (ptr);

#if !defined(HOARD_NO_CLEAR_SUPERBLOCK) || !(HOARD_NO_CLEAR_SUPERBLOCK)
    // Once this puppy is completely empty, switch back to bump mode.
    if (Header::getObjectsFree() == Header::getTotalObjects()) {
      clear();
    }
#endif
  }

  inline void clear (void) {
    Header::_position = reinterpret_cast<char *>(_buf);
    Header::clear ();
  }

private:

  Superblock (const Superblock&);

  inline bool checkInRange (void * ptr) const {
    // True if this object was really ours.
    bool greaterOrEqual = (reinterpret_cast<size_t>(ptr) >= reinterpret_cast<size_t>(_buf));
    bool lessOrEqual = (reinterpret_cast<size_t>(ptr) <= reinterpret_cast<size_t>(this + 1));
    return (greaterOrEqual && lessOrEqual);
  }

  class MyChecker;
  friend class MyChecker;

  /// Precondition and postcondition checking.
  class MyChecker {
  public:
    static void precondition (Superblock * e) {
      e->sanityCheck();
    }
    static void postcondition (Superblock * e) {
      e->sanityCheck();
    }
  };

  void sanityCheck (void) {
    assert (Header::isValidSuperblock());
    Header::sanityCheck();
  }

  /// @note Update this if any other fields are added to this class!
  enum { BufferSize = SuperblockSize - sizeof(Header)  };

  /// The buffer for allocating memory.
  char _buf[BufferSize];

};


#endif
