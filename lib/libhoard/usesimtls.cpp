/**
 * @file usesimtls.cpp
 * @author Emery Berger <http://www.cs.umass.edu/~emery>
 * @sa userealtls.cpp
 *
 * Emulate thread-local data by using aligned stacks for each thread,
 * and storing thread-local data at the start of these stacks. Each
 * thread can then locate its thread-local data by bitmasking the
 * address of any stack variable. This file also intercepts thread
 * completions to flush these local heaps, returning any unused memory
 * to the global Hoard heap. To do this, we interpose our own versions
 * of pthread_create and pthread_exit.
*/


#if defined(_WIN32)
#error "This file is not intended for usage with Microsoft Windows; use 'userealtls.cpp' instead."
#endif

// Ensure that all pthread_* calls get strong linkage.
// Otherwise, our versions here won't replace them!

#undef __GXX_WEAK__ 

/// The address of the main stack.
static void * mainThreadStackLocation;

/// The address of the main TLAB.
static TLAB * mainTLABptr;

#include "computethreadstacksize.h"

class InitializeMainTLAB {
public:
  InitializeMainTLAB (void) {
    // Force the 'main' TLAB to be initialized; that is, the TLAB for
    // the main thread, then set it.
    int dummy;
    static TLAB mainTLAB (getMainHoardHeap());
    mainThreadStackLocation = (void *) (((size_t) &dummy) & ComputeThreadStackSize::MASK);
    mainTLABptr = &mainTLAB;
  }
};

inline TLAB * getCustomHeap (void) {
  static InitializeMainTLAB initializeMe;
  int dummy;

  // Find our TLAB by bitmasking the address of a stack variable (this
  // depends on aligned thread stacks!).
  // Locate the TLAB at the beginning of the stack.
  TLAB * tlab = (TLAB *)
    (((size_t) &dummy) & ComputeThreadStackSize::MASK);

  if (tlab == mainThreadStackLocation) {
    return mainTLABptr;
  } else {
    return tlab;
  }
}


/****************/
/***** UNIX *****/
/****************/

/* Here is where we hijack pthread_create and company.  NOTE: This
   relies on libpthread being a shared library. */

#include <pthread.h>
#include <dlfcn.h>

#include <utility> // STL

typedef char * getcwdFunction (char *, size_t);

#if 0
extern "C" char * getcwd (char * buf, size_t size)
{
  static getcwdFunction * real_getcwd
    = (getcwdFunction *) dlsym (RTLD_NEXT, "getcwd");
  
  if (!buf) {
    if (size == 0) {
      size = PATH_MAX;
    }
    buf = (char *) malloc (size);
  }
  return (real_getcwd)(buf, size);
}
#endif

extern "C" {
typedef void * (*threadFunctionType) (void *);
}

typedef
int (*pthread_attr_setstackaddr_function) (pthread_attr_t * attr,
					   void * stack);

typedef  
int (*pthread_create_function) (pthread_t *thread,
				const pthread_attr_t *attr,
				threadFunctionType start_routine,
				void *arg);

typedef
void (*pthread_exit_function) (void *);

// A special routine we call on thread exits to free up some resources.
static void exitRoutine (void) {

  // Clear the TLAB's buffer.
  getCustomHeap()->clear();

  // Relinquish the assigned heap.
  getMainHoardHeap()->releaseHeap();
}


extern "C" void * startMeUp (void * a)
{
  getMainHoardHeap()->findUnusedHeap();

  // Instantiate the thread-local allocation buffer here.
  void * buf = (void *) getCustomHeap();
  
  new (buf) TLAB (getMainHoardHeap());

#if 0
  char b[255];
  sprintf (b, "%s: tlab at %x\n", __FILE__, buf);
  fprintf (stderr, b);
  fflush (stderr);
#endif

  pair<threadFunctionType, void *> * z
    = (pair<threadFunctionType, void *> *) a;
  threadFunctionType f = z->first;
  void * arg = z->second;
  void * result = (*f)(arg);
  exitRoutine();
  return result;
}

#if 0
pthread_attr_setstackaddr_function getReal_pthread_attr_setstackaddr (void) {
  static pthread_attr_setstackaddr_function f = NULL;
  if (f == NULL) {
    f = (pthread_attr_setstackaddr_function)
      dlsym (RTLD_NEXT, "pthread_attr_setstackaddr");
    if (f == NULL) {
      abort();
    }
  }
  return f;
}
#endif

/* NPTL internal pthread_attr stuff, for an annoying work-around. */

typedef struct 
{
  struct sched_param schedparam;
  int schedpolicy;
  int flags;
  size_t guardsize;
  void *stackaddr;
  size_t stacksize;
  void *cpuset;
  size_t cpusetsize;
} my_pthread_attr;

#if defined(linux)
// From Jakub Jelinek: detects if we are using NPTL, Linux's new
// thread library.
int isUsingNPTL (void) {
  size_t n = confstr (_CS_GNU_LIBPTHREAD_VERSION, NULL, 0);
  if (n > 0)
    {
      char buf[n];// = (char *) alloca (n);
      confstr (_CS_GNU_LIBPTHREAD_VERSION, buf, n);
      if (strstr (buf, "NPTL"))
        return 1;
    }
  return 0;
}
#endif

extern "C" {

int pthread_create (pthread_t *thread,
		    const pthread_attr_t *attr,
		    void * (*start_routine) (void *),
		    void * arg)
#ifndef __SVR4
                    throw()
#endif
{
  int stackSize = ComputeThreadStackSize::VALUE;
  char * stackBase = NULL;
  pair<threadFunctionType, void *> * args = NULL;

  // A pointer to the library version of pthread_create.
  static pthread_create_function real_pthread_create = NULL;

  // Force initialization of the TLAB before our first thread is created.
  volatile static TLAB * t = getCustomHeap();

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
      fprintf (stderr, "Could not find the pthread_create function.\n");
      fprintf (stderr, "Please report this problem to emery@cs.umass.edu.\n");
      abort();
    }
  }

  anyThreadCreated = 1;
 
  // Use an aligned thread stack.
  // This allows us to compute thread IDs very cheaply: we take
  // address of a stack variable and mask it. Again, see
  // heaplayers/cpuinfo.h.
  
  // NOTE: The default permissions here make the thread stack *non* executable.
  // This may provide some measure of safety from stack smashing attacks.
  // It may also interfere with JIT compilation.
  
#if defined(__SVR4) && defined(MAP_ALIGN)
  stackBase = (char *) mmap ((char *) stackSize, stackSize, HOARD_MMAP_PROTECTION_MASK, MAP_PRIVATE | MAP_ALIGN | MAP_ANON, -1, 0);

#else
  // We have to align the buffer ourselves.
  // Get a big chunk from mmap,
  // then unmap the non-aligned extra parts.

  char * ptr = (char *) mmap ((char *) NULL, stackSize * 2, HOARD_MMAP_PROTECTION_MASK, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  stackBase = (char *) (((size_t) ptr + stackSize - 1) & ~(stackSize - 1));

  size_t prolog = (size_t) stackBase - (size_t) ptr;
  
  if (prolog > 0) {

#if 0
    // FIX ME
    char buf[255];
    sprintf (buf, "%s: munmap %x, sz = %d\n", __FILE__, ptr, prolog);
    fprintf (stderr, buf);
    fflush (stderr);
#endif
    
    munmap (ptr, prolog);
  }


  void * eob = ptr + 2 * stackSize - 1;
  void * originalStart = stackBase + stackSize;
  void * newStart = (void *) (((size_t) originalStart + stackSize - 1) & ~(stackSize - 1));
  
  if ((size_t) newStart <= (size_t) eob) {
    size_t epilog = (size_t) eob - (size_t) newStart;
    if (epilog > 0) {

#if 0
      // FIX ME
      char buf[255];
      sprintf (buf, "%s: munmap %x, sz = %d\n", __FILE__, newStart, epilog);
      fprintf (stderr, buf);
      sprintf (buf, "%s: stackBase = %x, sz = %d\n", __FILE__, stackBase, stackSize);
      fprintf (stderr, buf);
      fflush (stderr);
#endif

      munmap (newStart, epilog);
    }
  }

            
#endif
      
  // Round up the TLAB size to the next page-size unit (typically
  // 4K) for some picky operating systems (Mac OS X). Then subtract
  // this from the stack size, and set the thread stack accordingly.
  // Reserve some space for the thread attribute & arguments to 'startMeUp'.
      
  const size_t variableSpaceNeeded =
    sizeof(TLAB) +
    sizeof(pthread_attr_t) +
    sizeof(pair<threadFunctionType, void *>);

  size_t roundUpSize = (variableSpaceNeeded + 8191) & ~8191;
  char * originalStackBase = stackBase;
  stackBase = originalStackBase + roundUpSize;
  stackSize -= roundUpSize;

  // Set the stack base and size appropriately.

  pthread_attr_t * backupAttr =
    new (originalStackBase + sizeof(TLAB)) pthread_attr_t;
  pthread_attr_init (backupAttr);

#if defined(linux)
  static bool nptl = isUsingNPTL();
  if (nptl) {
    // The following code is a work-around for the NPTL (new thread
    // library in Linux).  For some reason, directly setting the
    // attribute fields works fine, but calling pthread_attr_setstack
    // or the (deprecated) setstackaddr/size calls does not.
    my_pthread_attr * a = (my_pthread_attr *) backupAttr;
    a->stackaddr = stackBase;
    a->stacksize = stackSize;
  } else {
    pthread_attr_setstackaddr (backupAttr, stackBase);
    pthread_attr_setstacksize (backupAttr, stackSize);
  }   
#else
  pthread_attr_setstackaddr (backupAttr, stackBase);
  pthread_attr_setstacksize (backupAttr, stackSize);
#endif

  args = new (originalStackBase + sizeof(TLAB) + sizeof(pthread_attr_t))
    pair<threadFunctionType, void *> (start_routine, arg);

  int result = (*real_pthread_create)(thread, backupAttr, startMeUp, args);
  
  return result;
}

}


extern "C" void pthread_exit (void * arg)
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

#if defined(__SVR4) // Solaris

typedef
int (*thr_create_function) (void * stack_base,
			   size_t stack_size,
			   void * (*start_func) (void *),
			   void * arg,
			   long flags,
			   thread_t *new_thread_ID);


extern "C" int thr_create (void * stack_base,
			   size_t stack_size,
			   void * (*start_func) (void *),
			   void * arg,
			   long flags,
			   thread_t *new_thread_ID)
{
  static thr_create_function f = NULL;
  char fname[] = "thr_create";

  if (f == NULL) {
    f = (thr_create_function) dlsym (RTLD_NEXT, fname);
    if (f == NULL) {
      abort();
    }
  }

  anyThreadCreated = 1;
  
  pair<threadFunctionType, void *> * newarg
    = new pair<threadFunctionType, void *>(start_func, arg);
  
  if (stack_base != NULL) {
    anyThreadStackCreated = true;
  }

  int result = (*f)(stack_base, stack_size, startMeUp, newarg, flags, new_thread_ID);
  
  return result;
}

extern "C" void thr_exit (void * arg)
{
  static pthread_exit_function f
    = (pthread_exit_function) dlsym (RTLD_NEXT, "thr_exit");
  exitRoutine();
  (*f)(arg);
}
#endif

