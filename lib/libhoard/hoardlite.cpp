/* -*- C++ -*- */

/*

  "Hoard-Lite", a variant of the Hoard Multiprocessor Memory Allocator
  www.hoard.org

  Author: Emery Berger, http://www.cs.umass.edu/~emery

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/

/*
 * This class layers a per-thread thresholding allocator on top of the
 * GNU libc allocator. It improves thread-level scalability and even
 * uniprocessor performance, since most allocations avoid
 * locking.
 * 
 * Note that this is not the be-all, end-all of scalable memory
 * allocation.  Because it is built on top of the Lea allocator, it
 * has per-object headers, which can consume extra space and cause
 * cache pollution. It also can suffer from absolutely catastrophic
 * "allocator-induced false sharing", which destroys performance on
 * multiprocessors. See my paper on Hoard for a discussion of this
 * phenomenon, and how Hoard avoids it. But for many purposes, this
 * allocator is good enough, and it's definitely much faster than the
 * default allocator.
 *
 * A bonus: because every malloc'ed object is a "real" malloc'ed
 * object, you might even be able to use your favorite memory debugger
 * or leak detector without any problems. Unfortunately, due to a
 * limitation in the GNU libc's API, I have to make what most memory
 * debuggers would consider 'illegal' calls in order to determine
 * object sizes. Hopefully, this will eventually change.
 *
 * Copyright (C) 2005 Emery Berger, University of Massachusetts Amherst
 *
 */


#include <new>
#include <assert.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#include <utility> // STL

#include "ansiwrapper.h"

using namespace HL;

extern "C" void * dlmalloc (size_t);
extern "C" void   dlfree (void *);
extern "C" size_t dlmalloc_usable_size (void *);

/*
#define USE_DL_PREFIX 1
#define USE_MALLOC_LOCK 1
*/

#include "dlmalloc.c"

class OldMalloc {
public:
  inline void * malloc (size_t sz) const {
    void * ptr = ::dlmalloc (sz);
    return ptr;
  }

  inline void free (void * ptr) const {
    ::dlfree (ptr);
  }

  inline size_t getSize (void * obj) const {
    return ::dlmalloc_usable_size (obj);
  }

};


/**
 * ThresholdHeap
 * keeps no more than a threshold amount of free memory on the heap.
 */

template <int THRESHOLD, int MAX_SIZE, class super>
class ThresholdHeap : public super {
public:

  ThresholdHeap (void)
    : currentBytes (0)
  {
    // Initialize all the free lists to NULL (empty).
    for (int i = 0; i < BIGGEST_OBJECT_INDEX; i++) {
      bin[i] = NULL;
    }
  }

  inline void * malloc (size_t sz) {
    // Try a local bin unless the size is too large.
    if (sz < MAX_SIZE) {

      // Convert the size into a bin index.
      const int index = sz >> SIZE_SHIFT;
      Object *& head = bin[index];
      if (head) {

	// Pop off an object.
	Object * obj = head;
	head = head->next;
	currentBytes -= (index << SIZE_SHIFT); // super::getSize (obj); // (index << SIZE_SHIFT);
	assert (super::getSize(obj) >= sz);
	return obj;
      }
    }
    // Otherwise, ask for memory from the parent.
    void * ptr = super::malloc (sz);
    return ptr;
  }

  inline void free (void * ptr) {

    // Get the object size, and convert it into a bin (free list) index.
    const size_t sz = super::getSize (ptr);
    const int index = sz >> SIZE_SHIFT;

    // Free the object onto the local heap if we haven't exceeded our
    // threshold or it's not too big.

    if ((sz < MAX_SIZE) && (currentBytes + sz <= THRESHOLD)) {

      // Push the object onto the appropriate free list.
      Object * obj = (Object *) ptr;
      Object *& head = bin[index];
      obj->next = head;
      head = obj;

      // Update the amount of space held on free lists.
      currentBytes += (index << SIZE_SHIFT); // sz;

    } else {

      // Either the object is too big or we already have too much on
      // our local heap, so free the object to the parent heap.
      super::free (ptr);

      if (sz < MAX_SIZE) {
	// We just had too much stuff on the heap.
	// Clear the current bin!
	// clearBin (sz >> SIZE_SHIFT);
      }
    }
  }

  inline size_t getSize (void * ptr) {
    return super::getSize (ptr);
  }

  void clear (void) {
    for (int i = 0; i < BIGGEST_OBJECT_INDEX; i++) {
      clearBin (i);
    }
    currentBytes = 0;
  }

  void clearBin (int i) {
    Object * obj = bin[i];
    while (obj) {
      Object * next = obj->next;
      currentBytes -= super::getSize (obj);
      super::free (obj);
      obj = next;
    }
    bin[i] = NULL;
  }

private:

  enum { SIZE_SHIFT = 3 };
  enum { BIGGEST_OBJECT_INDEX = MAX_SIZE >> SIZE_SHIFT };

  class Object {
  public:
    /// The next object in the free list.
    Object * next;
  };

  /// The current number of bytes on this heap.
  size_t currentBytes;

  /// Freelist pointers for each possible size.
  Object * bin[BIGGEST_OBJECT_INDEX];

};


// [ ][------ 128 bytes ------] from Lea
// [ ][HtidHobjHobj....Hobj.xx]


class TheCustomHeapType :
  public ANSIWrapper<ThresholdHeap<65536, 2048, OldMalloc> > {};

/*
  Because we can't initialize C++ objects in thread-local variables,
  we need this work-around: we create a thread-local array to hold the
  class, and a pointer that will point to it (initially NULL).
*/

typedef struct {
  // Use a double here to ensure double-word alignment.
  double buf[sizeof(TheCustomHeapType) / sizeof(double) + 1];
} Buffer;

// The buffer that will eventually hold the TLAB.
__thread Buffer tlabBuffer;

// The TLAB pointer.
__thread TheCustomHeapType * theTLAB;

// The thread id.
__thread unsigned int tid;

inline TheCustomHeapType * getCustomHeap (void) {
  // The pointer to the TLAB itself.
  if (!theTLAB) {
    new ((char *) &tlabBuffer) TheCustomHeapType;
    theTLAB = (TheCustomHeapType *) &tlabBuffer;  
    // Linux-specific...
    tid = (unsigned int) pthread_self() >> 10;
  }

  return theTLAB;
}

#include "wrapper.cpp"


//
// Everything below here is to force the clearing of TLABs when the owning thread goes away.
//

// Ensure that all pthread_* calls get strong linkage.
// Otherwise, our versions here won't replace them!

#undef __GXX_WEAK__ 

extern "C" {

typedef void * (*threadFunctionType) (void *);

typedef  
int (*pthread_create_function) (pthread_t *thread,
				const pthread_attr_t *attr,
				threadFunctionType start_routine,
				void *arg);
}

#include <dlfcn.h>

extern "C" void * threadProxy (void * a)
{
  std::pair<threadFunctionType, void *> * z
    = (std::pair<threadFunctionType, void *> *) a;

  threadFunctionType f = z->first;
  void * arg = z->second;
  void * result = (*f)(arg);

  // The thread is done: clear the local heap.
  getCustomHeap()->clear();
  return result;
}


extern "C" int pthread_create (pthread_t *thread,
			       const pthread_attr_t *attr,
			       void * (*start_routine) (void *),
			       void * arg) throw ()
{
  // A pointer to the library version of pthread_create.
  static pthread_create_function real_pthread_create = NULL;

  // Force initialization of the TLAB before our first thread is created.
  volatile static TheCustomHeapType * t = getCustomHeap();

#if defined(linux)
  char fname[] = "pthread_create";
#else
  char fname[] = "_pthread_create";
#endif
 
  // Instantiate the pointer to pthread_create, if it hasn't been
  // instantiated yet.

  if (real_pthread_create == NULL) {
    real_pthread_create = (pthread_create_function) dlsym (RTLD_NEXT, fname);
    if (real_pthread_create == NULL) {
      fprintf (stderr, "Could not find the pthread_create function!\n");
      fprintf (stderr, "Please report this problem to emery@cs.umass.edu.\n");
      abort();
    }
  }

  std::pair<threadFunctionType, void *> * args
    = new std::pair<threadFunctionType, void *> (start_routine, arg);

  int result = (*real_pthread_create)(thread, attr, threadProxy, args);
  
  return result;
}
