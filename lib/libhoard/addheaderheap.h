// -*- C++ -*-

#ifndef _ADDHEADERHEAP_H_
#define _ADDHEADERHEAP_H_

#include "sassert.h"
#include "hldefines.h"

/**
 * @class AddHeaderHeap
 */

template <class SuperblockType,
	  size_t SuperblockSize,
	  class SuperHeap>
class AddHeaderHeap : public SuperHeap {
private:

  HL::sassert<(((int) SuperHeap::Alignment) % SuperblockSize == 0)> verifySize1;
  HL::sassert<(((int) SuperHeap::Alignment) >= SuperblockSize)> verifySize2;

public:

  enum { Alignment = 0 };

  MALLOC_FUNCTION INLINE void * malloc (size_t sz) {
    // Allocate extra space for the header,
    // put it at the front of the object,
    // and return a pointer to just past it.
    void * ptr = SuperHeap::malloc (sz + sizeof(typename SuperblockType::Header));
    if (ptr == NULL) {
      fprintf (stderr, "AddHeaderHeap::malloc - no memory.\n");
      return NULL;
    }
    typename SuperblockType::Header * p;
    p = new (reinterpret_cast<char *>(ptr)) typename SuperblockType::Header (sz, sz);
    return reinterpret_cast<void *>(p + 1);
  }

  INLINE size_t getSize (void * ptr) {
    // Find the header (just before the pointer) and return the size
    // value stored there.
    typename SuperblockType::Header * p;
    p = reinterpret_cast<typename SuperblockType::Header *>(ptr);
    return (p - 1)->getSize (ptr);
  }

  INLINE void free (void * ptr) {
    // Find the header (just before the pointer) and free the whole object.
    typename SuperblockType::Header * p;
    p = reinterpret_cast<typename SuperblockType::Header *>(ptr);
    SuperHeap::free (reinterpret_cast<void *>(p - 1));
  }
};

#endif
