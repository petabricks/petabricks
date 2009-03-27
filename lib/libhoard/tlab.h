// -*- C++ -*-

/**
 *
 * @class  ThreadLocalAllocationBuffer
 * @author Emery Berger <http://www.cs.umass.edu/~emery>
 * @brief  An allocator, meant to be used for thread-local allocation.
 */

#include "dllist.h"
#include "array.h"

template <int NumBins,
	  int (*getSizeClass) (size_t),
	  size_t (*getClassSize) (const int),
	  int LargestObject,
	  int LocalHeapThreshold,
	  class SuperblockType,
	  int SuperblockSize,
	  class ParentHeap>

class ThreadLocalAllocationBuffer {

public:

  ThreadLocalAllocationBuffer (ParentHeap * parent)
    : _parentHeap (parent),
      _localHeapBytes (0)
  {
  }

  ~ThreadLocalAllocationBuffer (void) {
    clear();
  }

  size_t getSize (void * ptr) {
    return _parentHeap->getSize (ptr);
  }

  inline void * malloc (size_t sz) {
    // Get memory from the local heap,
    // and deduct that amount from the local heap bytes counter.
    if (sz > LargestObject) {
      return _parentHeap->malloc (sz);
    }

    if (sz < 2 * sizeof(size_t)) {
      sz = 2 * sizeof(size_t);
    }

    int c = getSizeClass (sz);
    void * ptr = localHeap(c).get();
    if (ptr) {
      _localHeapBytes -= getClassSize(c);
      return ptr;
    }

    // No more local memory (for this size, at least).
    // Now get the memory from our parent.
    ptr = _parentHeap->malloc (sz);

    const SuperblockType * s = getSuperblock (ptr);
    assert (s != NULL);
    assert (s->isValidSuperblock());
    const void * owner = (const void *) (s->getOwner());
    assert (owner != NULL);

    return ptr;
  }

  inline void free (void * ptr) {

    if (ptr == NULL) {
      return;
    }

    //
    // Get all the size and ownership info about this object.
    //

    const SuperblockType * s = getSuperblock (ptr);
    
    // If this isn't a valid superblock, just return.

    if (!s || !s->isValidSuperblock()) {
      return;
    }

    assert (s != NULL);
    assert (s->isValidSuperblock());

    const size_t sz = s->getObjectSize();

    // Free big objects to the parent.
    if (sz > LargestObject) {
      _parentHeap->free (ptr);
      return;
    }

    const int c = getSizeClass (sz);
    assert (getClassSize(c) >= sz);

    // Check to see if this pointer is 'remote' or local.

    const void * owner = (const void *) (s->getOwner());
    if ((owner == _parentHeap) || (owner == NULL)) {
      // It's local. What happens local stays local :).
      localHeap(c).insert ((HL::DLList::Entry *) ptr);
      _localHeapBytes += sz;
   
    } else {
      // It's remote. Free it directly to the parent heap.
      _parentHeap->free (ptr);
    }
    
    // If we have too much memory stored locally,
    // give it all back.
    
    if (_localHeapBytes > LocalHeapThreshold) {
      clear();
      //clearHalf();
    }
  }

  void clear (void) {
    // Free every object to the 'parent' heap.
    for (int i = 0; i < NumBins; i++) {
      HL::DLList::Entry * e = localHeap(i).get();
      while (e) {
	_parentHeap->free (e);
	e = localHeap(i).get();
      }
    }
    _localHeapBytes = 0;
  }

private:

  void clearHalf (void) {
    // Free objects to the 'parent' heap
    // until we've given up half of the threshold amount of memory.
    // We start with the bigger objects first, and work our way down.
    for (int i = NumBins - 1; i >= 0; i--) {
      size_t sz = getClassSize (i);
      while (!localHeap(i).isEmpty()) {
	HL::DLList::Entry * e = localHeap(i).get();
	_parentHeap->free (e);
	_localHeapBytes -= sz;
	if (_localHeapBytes <= (LocalHeapThreshold / 2)) {
	  return;
	}
      }
    }
  }

  // Disable assignment and copying.

  ThreadLocalAllocationBuffer (const ThreadLocalAllocationBuffer&);
  ThreadLocalAllocationBuffer& operator=(const ThreadLocalAllocationBuffer&);

  /// Use bit masking to find the start of the containing superblock.
  static inline SuperblockType * getSuperblock (void * ptr) {
    SuperblockType * s
      = reinterpret_cast<SuperblockType *>(reinterpret_cast<size_t>(ptr) & ~(SuperblockSize-1));
    return s;
  }

  /// This heap's 'parent' (where to go for more memory).
  ParentHeap * _parentHeap;

  /// The number of bytes we currently have on this thread.
  int _localHeapBytes;

  /// The local heap itself.
  HL::Array<NumBins, HL::DLList> localHeap;

};
