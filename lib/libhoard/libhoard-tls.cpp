/* -*- C++ -*- */

// The Hoard Multiprocessor Memory Allocator
// www.hoard.org
//
// Author: Emery Berger, http://www.cs.umass.edu/~emery
//
// Copyright (c) 1998-2003, The University of Texas at Austin.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Library General Public License as
// published by the Free Software Foundation, http://www.fsf.org.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Library General Public License for more details.
//
//////////////////////////////////////////////////////////////////////////////

/*
 * @file   libhoard.cpp
 * @brief  This file replaces malloc etc. in your application.
 * @author Emery Berger <http://www.cs.umass.edu/~emery>
 */

#include <stdlib.h>

// #define USE_HOARD_TLS 1

#if defined(_WIN32)
#define __thread __declspec(thread)
#endif

#include "cpuinfo.h"
#include "hoard.h"
#include "VERSION.h"

typedef bins<MySuperblockType<SmallHeap>::Header, SUPERBLOCK_SIZE> MyBinType;
enum { num_bins = MyBinType::NUM_BINS };

__thread int initialized = 0;
__thread ProcessHeap::SuperblockType * local_superblock[num_bins];
__thread int hoard_tid;

volatile int anyThreadCreated = 0;

const int MaxThreads = 128;
const int NumHeaps = 16;

template <class SuperHeap>
class PerThreadSuperblockHeap : public SuperHeap {
public:
  inline void * malloc (size_t sz) {
      fprintf (stderr, "DODODODOOD\n");
      return SuperHeap::malloc (sz);


    int c = MyBinType::getSizeClass (sz);
    if ((c < 0) || (c >= num_bins)) {
      //      abort();
      return NULL;
    }

    // Fast path.
    if (local_superblock[c]) {
      void * ptr = local_superblock[c]->malloc (sz);
      if (ptr) {
	return ptr;
      }
    }

    return mallocSlowPath (c, sz);
  }

  void * mallocSlowPath (int c, size_t sz) {
    while (1) {
      if (local_superblock[c] == NULL) {
	// Get a superblock.
	local_superblock[c] = reinterpret_cast<typename ProcessHeap::SuperblockType *> (SuperHeap::get (sz, NULL));
      }
      if (local_superblock[c] == NULL) {
	local_superblock[c] = reinterpret_cast<typename ProcessHeap::SuperblockType *> (getAnotherSuperblock (sz));
      }
      if (local_superblock[c] == NULL) {
	fprintf (stderr, "HOLY CRAP %s!\n", __FILE__);
      	abort();
	return SuperHeap::malloc (sz);
      }
      void * ptr = local_superblock[c]->malloc (sz);
      if (ptr) {
	return ptr;
      }
      // The superblock has been entirely consumed. Get rid of it.
      put (reinterpret_cast<typename SuperHeap::SuperblockType *>(local_superblock[c]), sz);
      local_superblock[c] = NULL;
      printf ("iterate...\n");
    }
  }

  inline void free (void * ptr) {
    // If the free is to this superblock, free it directly
    // without a lock.

    // FIX ME!!
     SuperHeap::free (ptr);
     return;

    // FIX ME!!

    int c = MyBinType::getSizeClass (getSize(ptr));
    if ((local_superblock[c] != NULL) && (ptr >= local_superblock[c]) && (ptr <= local_superblock[c] + 1)) {
      local_superblock[c]->free (ptr);
    } else {
      SuperHeap::free (ptr);
    }
  }
};

class TheCustomHeapType ;

class TheCustomHeapType : public HL::ANSIWrapper<HL::HybridHeap<bins<MySuperblockType<SmallHeap>::Header, SUPERBLOCK_SIZE>::BIG_OBJECT, PerThreadSuperblockHeap<ThreadPoolHeap<MaxThreads, NumHeaps, HoardHeapX> >, BigHeap> > {};

// class TheCustomHeapType : public HL::ANSIWrapper<HL::HybridHeap<bins<MySuperblockType<SmallHeap>::Header, SUPERBLOCK_SIZE>::BIG_OBJECT, ThreadPoolHeap<MaxThreads, NumHeaps, HoardHeapX>, BigHeap> > {};

inline static TheCustomHeapType * getCustomHeap (void) {
  static char thBuf[sizeof(TheCustomHeapType)];
  static TheCustomHeapType * th = new (thBuf) TheCustomHeapType;
  return th;
}

#if defined(_WIN32)
#pragma warning(disable:4273)
#endif

#include "wrapper.cpp"

static void * handle;

static void initializeMaps (void);

static TheLockType heapLock;

static void acquireHeapLock (void) {
  heapLock.lock();
}

static void releaseHeapLock (void) {
  heapLock.unlock();
}


class InitializeMe {
public:
  InitializeMe (void) {
#if defined(RTLD_NEXT)
    handle = RTLD_NEXT; // dlopen ("libpthread.so", RTLD_NOW);
#endif
    initializeMaps();
  }
};

void _init (void) {
  static InitializeMe please;
}

static void initializeMaps (void) {
  int i;
#if 0
  for (i = 0; i < MaxThreads; i++) {
    getCustomHeap()->setTidMap(i, 0);
  }
  for (i = 0; i < MaxHeaps; i++) {
    getCustomHeap()->setInusemap (i, 0);
  }
#endif
}

static void findUnusedHeap (void) {
  // Find an unused heap.
  acquireHeapLock();
  
#if 0
  int tid = CPUInfo::getThreadId() % TheCustomHeapType::MaxThreads;

  int i = 0;
  while ((i < TheCustomHeapType::MaxHeaps) && (getCustomHeap()->getInusemap(i)))
    i++;
  if (i >= TheCustomHeapType::MaxHeaps) {
    //    printf ("DOH! all in use!\n");
    // Every heap is in use: pick a random victim.
    i = (int) ( TheCustomHeapType::MaxHeaps * ((double) rand() / (double) RAND_MAX));
  }

  //  printf ("thread %d acquiring heap %d\n", tid, i);

  getCustomHeap()->setInusemap (i, 1);
  getCustomHeap()->setTidMap (tid, i);
  releaseHeapLock();
#endif
}

static void releaseHeap (void) {
#if 0
  // Decrement the ref-count on the current heap.

  acquireHeapLock();
  enum { VerifyPowerOfTwo = 1 / ((TheCustomHeapType::MaxThreads & ~(TheCustomHeapType::MaxThreads-1))) };

  int tid = CPUInfo::getThreadId() & (TheCustomHeapType::MaxThreads - 1);
  int heapIndex = getCustomHeap()->getTidMap (tid);

  //  printf ("thread %d releasing heap %d\n", tid, heapIndex);
 
  getCustomHeap()->setInusemap (heapIndex, 0);

  // Prevent underruns (defensive programming).

  if (getCustomHeap()->getInusemap (heapIndex) < 0) {
    getCustomHeap()->setInusemap (heapIndex, 0);
  }
  releaseHeapLock();
#endif
}

#if !defined(_WIN32)

// NOTE: Relies on libpthread being a shared library.

#include <pthread.h>
#include <dlfcn.h>

template <class T1, class T2>
class MyPair {
public:
  T1 first;
  T2 second;
};


extern "C" {

  typedef void * (*threadFunctionType) (void *);

  typedef  
  int (*pthread_create_function) (pthread_t *thread,
				  const  pthread_attr_t *attr,
				  threadFunctionType start_routine,
				  void *arg);
  
  typedef
  void (*pthread_exit_function) (void *);

  static void exitRoutine (void) {
    releaseHeap();
    // Dispose of any local superblocks.
    for (int i = 0; i < num_bins; i++) {
      if (superblock[i]) {
	getCustomHeap()->put (reinterpret_cast<TheCustomHeapType::SuperblockType *>(superblock[i]), MyBinType::getClassSize(i));
	superblock[i] = NULL;
      }
    }
  }


  void * startMeUp (void * a)
  {
    anyThreadCreated = 1;

    findUnusedHeap();

    MyPair<threadFunctionType, void *> * z
      = (MyPair<threadFunctionType, void *> *) a;
    threadFunctionType f = z->first;
    void * arg = z->second;
    delete z;
    void * result = (*f)(arg);
    exitRoutine();
    return result;
  }

  int pthread_create (pthread_t *thread,
		      const  pthread_attr_t *attr,
		      threadFunctionType start_routine,
		      void * arg)
  {
    //    static InitializeMe init = startRoutine();
    static pthread_create_function f = NULL;
    initialized = 0;
    hoard_tid = CPUInfo::getThreadId();
#if defined(linux)
    char fname[] = "pthread_create";
#else
    char fname[] = "_pthread_create";
#endif

    if (f == NULL) {
      f = (pthread_create_function) dlsym (RTLD_NEXT, fname);
    }

    MyPair<threadFunctionType, void *> * newarg
      = new MyPair<threadFunctionType, void *>();

    newarg->first  = start_routine;
    newarg->second = arg;

    int result = (*f)(thread, attr, startMeUp, newarg);
    
    return result;
  }

  void pthread_exit (void * arg)
  {
#if defined(linux)
    static pthread_exit_function f
      = (pthread_exit_function) dlsym (RTLD_NEXT, "pthread_exit");
#else
    static pthread_exit_function f
      = (pthread_exit_function) dlsym (RTLD_NEXT, "_pthread_exit");
#endif
    exitRoutine();
    (*f)(arg);
  }
}

#elif defined(_WIN32)

#include <stdio.h>

extern "C"
BOOL WINAPI DllMain(HANDLE hinstDLL, DWORD fdwReason, LPVOID lpreserved)
{
  int i;
  int tid;
  static int np = CPUInfo::computeNumProcessors();
  switch (fdwReason) {
  case DLL_PROCESS_ATTACH:
    fprintf (stderr, "This software uses the Hoard scalable memory allocator (version " HOARD_VERSION_STRING ").\nCopyright (C) 2005 Emery Berger, The University of Texas at Austin,\nand University of Massachusetts, Amherst.\nFor more information, see http://www.hoard.org\n");
    initializeMaps();
    break;
  case DLL_THREAD_ATTACH:
    hoard_tid = CPUInfo::getThreadId();
    for (int i = 0; i < num_bins; i++) {
      local_superblock[i] = NULL;
    }
    if (np != 1) {
      // This relies on atomic stores in spinlock.h.
      anyThreadCreated = 1;
    }
    if (np == 1) {
      // Assign the thread to heap 0.
      // FIX ME
      // getCustomHeap()->setTidMap ((int) CPUInfo::getThreadId() % TheCustomHeapType::MaxThreads, 0);
    } else {
      findUnusedHeap();
    }
    break;
  case DLL_THREAD_DETACH:
    // Do we want to reset anyThreadCreated?
    for (int i = 0; i < num_bins; i++) {
      // Dispose of local_superblocks here.
      local_superblock[i] = NULL;
    }
    if (np != 1) {
      releaseHeap();
    }
    break;
  default:
    return TRUE;
  }
  return TRUE;
}

#endif
