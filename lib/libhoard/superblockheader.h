// -*- C++ -*-

#ifndef _SUPERBLOCKHEADER_H_
#define _SUPERBLOCKHEADER_H_

#include <stdlib.h>
#include <stdio.h>

#include "sassert.h"
#include "check.h"
#include "freesllist.h"
#include "hldefines.h"

/**
 * @class SuperblockHeader
 * @brief The base header for any superblock.
 * @note  Assumes all objects are the same size.
 */

template <class Super>
class SuperblockHeader : public Super {
private:

  enum { SUPERBLOCK_MAGIC_NUMBER = 0xCafeBabe };

public:

  typedef Super Header;

  inline SuperblockHeader (size_t sz, size_t available)
    : Super (sz),
      _magic (SUPERBLOCK_MAGIC_NUMBER),
      _objectSize (sz),
      _totalObjects (available / sz),
      _objectsFree (_totalObjects),
      _magic2 (SUPERBLOCK_MAGIC_NUMBER),
      _junk (0)
  {
    assert (sz <= available);
  }

  void dumpStats (void) {
#ifndef NDEBUG
    fprintf (stderr, "Superblock %x: size = %d, total = %d, free = %d\n",
	     this, _objectSize, _totalObjects, _objectsFree);
#endif
  }

  /// @return The size of the given object.
  /// @note   All objects from this superblock are the same size, so the argument is ignored.
  INLINE size_t getSize (void *) const {
    return _objectSize;
  }

  MALLOC_FUNCTION INLINE void * malloc (size_t sz) {
    assert (isValidSuperblock());
    assert (sz <= _objectSize);
    char * p = reinterpret_cast<char *>(_freeList.get());
    if (p) {
      assert (_objectsFree >= 1);
      _objectsFree--;
    }
    assert (isValidSuperblock());
    return p;
  }

  INLINE void free (void * ptr) {
    assert (isValidSuperblock());
    _freeList.insert (reinterpret_cast<FreeSLList::Entry *>(ptr));
    _objectsFree++;
    assert (isValidSuperblock());
  }

  inline bool isValidSuperblock (void) const {
#if 0
    if (!Super::isValidSuperblock()) {
      fprintf (stderr, "Error - invalid header!: %s!\n", __FILE__);
      abort();
    }
    if (_magic != SUPERBLOCK_MAGIC_NUMBER) {
      fprintf (stderr, "Error - invalid superblock number: %s!\n", __FILE__);
      abort();
    }
    if (_objectsFree > _totalObjects) {
      fprintf (stderr, "Error - invalid number of objects: %s!\n", __FILE__);
      abort();
    }
    if ((size_t) this != (((size_t) this + SUPERBLOCK_SIZE-1) & ~(SUPERBLOCK_SIZE-1))) {
      fprintf (stderr, "Error - unaligned superblock: %s!\n", __FILE__);
      abort();
    }
#endif
    return ((_magic == (unsigned long) SUPERBLOCK_MAGIC_NUMBER) &&
	    (_magic2 == (unsigned long) SUPERBLOCK_MAGIC_NUMBER) &&
	    (_objectsFree <= _totalObjects) &&
	    ((size_t) this == (((size_t) this + (SUPERBLOCK_SIZE-1)) & ~(SUPERBLOCK_SIZE-1))));
  }

  void clear (void) {
    assert (isValidSuperblock());
    _freeList.clear();
    _objectsFree = _totalObjects;
    assert (isValidSuperblock());
  }

  INLINE int getTotalObjects (void) const {
    return _totalObjects;
  }

  /// Return the number of free objects in this superblock.
  INLINE int getObjectsFree (void) const {
    assert (_objectsFree >= 0);
    assert (_objectsFree <= _totalObjects);
    return _objectsFree;
  }

  /// Return the size of objects in this superblock.
  INLINE size_t getObjectSize (void) const {
    return _objectSize;
  }

  INLINE void decObjectsFree (void) {
    assert (_objectsFree >= 1);
    assert (isValidSuperblock());
    _objectsFree--;
    assert (isValidSuperblock());
  }

  void sanityCheck (void) const {
    assert (isValidSuperblock());
  }

private:

  /// A magic number to verify that this is a valid superblock.
  const unsigned long _magic;

  /// The object size.
  const size_t _objectSize;

  /// Total objects in the superblock.
  const size_t _totalObjects;

  /// The list of freed objects.
  FreeSLList _freeList;

protected:
  /// The number of objects available for (re)use.
  size_t _objectsFree;

private:

  const unsigned long _magic2;
  const unsigned long _junk; // for alignment.

protected:

  /// The cursor into the buffer following the header.
  char * _position;

  // For 64-bit alignment!
  double _dummyD1;
  double _dummyD2;

  /// No copy constructor.
  SuperblockHeader (const SuperblockHeader&);

};

#endif
