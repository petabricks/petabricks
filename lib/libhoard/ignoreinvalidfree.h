// -*- C++ -*-

#ifndef _IGNOREINVALIDFREE_H_
#define _IGNOREINVALIDFREE_H_

#include "hldefines.h"

// A class that checks to see if the object to be freed is inside a
// valid superblock. If not, it drops the object on the floor. We do
// this in the name of robustness (turning a segfault or data
// corruption into a potential memory leak) and because on some
// systems, it's impossible to catch the first few allocated objects.

template <class S>
class IgnoreInvalidFree : public S {
public:
  INLINE void free (void * ptr) {
    typename S::SuperblockType * s = S::getSuperblock (ptr);
    if (!s || (!s->isValidSuperblock())) {
      // We encountered an invalid free, so we drop it.
      return;
    }
    size_t v = *((size_t *) ptr);
#if 0
    // Don't try to free this object if it's filled with the special
    // already-freed value. Prevents double frees, but could lead to
    // memory leaks if people are actually using this value in their
    // allocated data.
    if (v != ALREADY_FREED_VALUE) {
      *((size_t *) ptr) = ALREADY_FREED_VALUE;
      S::free (ptr);
    }
#else
    S::free (ptr);
#endif
  }

  INLINE size_t getSize (void * ptr) {
    typename S::SuperblockType * s = S::getSuperblock (ptr);
    if (!s || (!s->isValidSuperblock()))
      return 0;
    return S::getSize (ptr);
  }

private:

  // A special value that is placed into deallocated objects.
  enum { ALREADY_FREED_VALUE = 0xDeadBeef };

};

#endif
