/**
 * @file userealtls.cpp
 * @author Emery Berger <http://www.cs.umass.edu/~emery>
 *
 * This file leverages compiler support for thread-local variables for
 * access to thread-local heaps. It also intercepts thread completions
 * to flush these local heaps, returning any unused memory to the
 * global Hoard heap. On Windows, this happens in DllMain. On Unix
 * platforms, we interpose our own versions of pthread_create and
 * pthread_exit.
*/

#if !defined(_WIN32) && !defined(USE_THREAD_KEYWORD)
#error "This file is only for targets with compiler support for thread-specific data."
#endif

#include "VERSION.h"

#if defined(_WIN32)
#define THREAD_LOCAL __declspec(thread)
#else
#define THREAD_LOCAL __thread
#endif

#include <new>

static THREAD_LOCAL double tlabBuffer[sizeof(TheCustomHeapType) / sizeof(double) + 1];
static THREAD_LOCAL TheCustomHeapType * theTLAB = NULL;

/// Return the custom heap.

static void initializeCustomHeap (void) {
  new ((char *) &tlabBuffer) TheCustomHeapType (getMainHoardHeap());
  theTLAB = (TheCustomHeapType *) &tlabBuffer;  
}

inline TheCustomHeapType * getCustomHeap (void) {
  // The pointer to the TLAB itself.
  if (!theTLAB) {
    initializeCustomHeap();
  }
  return theTLAB;
}

//
// Intercept thread creation and destruction to flush the TLABs.
//

#if defined(_WIN32)

#ifndef HOARD_PRE_ACTION
#define HOARD_PRE_ACTION
#endif

#ifndef HOARD_POST_ACTION
#define HOARD_POST_ACTION
#endif

#include <stdio.h>

#ifndef CUSTOM_DLLNAME
#define CUSTOM_DLLNAME DllMain
#endif

extern "C" {

BOOL WINAPI CUSTOM_DLLNAME (HANDLE hinstDLL, DWORD fdwReason, LPVOID lpreserved)
{
  int i;
  int tid;
  static int np = HL::CPUInfo::computeNumProcessors();

  switch (fdwReason) {

  case DLL_PROCESS_ATTACH:
    HOARD_PRE_ACTION;
    fprintf (stderr, "This software uses the Hoard scalable memory allocator (version " HOARD_VERSION_STRING ", libhoard).\nCopyright (C) 2005 Emery Berger, The University of Texas at Austin,\nand the University of Massachusetts Amherst.\nFor more information, see http://www.hoard.org\n");
    break;

  case DLL_PROCESS_DETACH:
    HOARD_POST_ACTION;
    break;

  case DLL_THREAD_ATTACH:
    if (np == 1) {
      // We have exactly one processor - just assign the thread to
      // heap 0.
      getMainHoardHeap()->chooseZero();
    } else {
      getMainHoardHeap()->findUnusedHeap();
    }
    break;

  case DLL_THREAD_DETACH:
    // Dump the memory from the TLAB.
    getCustomHeap()->clear();
    if (np != 1) {
      // If we're on a multiprocessor box, relinquish the heap
      // assigned to this thread.
      getMainHoardHeap()->releaseHeap();
    }
    break;

  default:
    return TRUE;
  }

  return TRUE;
}
}

#else

extern "C" {

typedef void * (*threadFunctionType) (void *);

typedef  
int (*pthread_create_function) (pthread_t *thread,
				const pthread_attr_t *attr,
				threadFunctionType start_routine,
				void *arg);
}

#include <dlfcn.h>

// A special routine we call on thread exits to free up some resources.
static void exitRoutine (void) {
  // Clear the TLAB's buffer.
  getCustomHeap()->clear();

  // Relinquish the assigned heap.
  getMainHoardHeap()->releaseHeap();
}


extern "C" {

static void * startMeUp (void * a)
{
  initializeCustomHeap();
  getMainHoardHeap()->findUnusedHeap();

  pair<threadFunctionType, void *> * z
    = (pair<threadFunctionType, void *> *) a;

  threadFunctionType f = z->first;
  void * arg = z->second;
  void * result = (*f)(arg);
  exitRoutine();
  return result;
}

}


extern "C" int pthread_create (pthread_t *thread,
			       const pthread_attr_t *attr,
			       void * (*start_routine) (void *),
			       void * arg)
#if !defined(__SUNPRO_CC)
  throw ()
#endif
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

  anyThreadCreated = 1;
 
  pair<threadFunctionType, void *> * args
    = new pair<threadFunctionType, void *> (start_routine, arg);

  int result = (*real_pthread_create)(thread, attr, startMeUp, args);
  
  return result;
}

#endif
