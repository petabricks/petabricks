/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/

#include "jalloc.h"
#include "jasm.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif

//#undef JALIB_ALLOCATOR

//#define PROFILE_ALLOCATOR
#ifdef PROFILE_ALLOCATOR
#include <math.h>
jalib::AtomicT a_counts[ sizeof(size_t)*8 ] = {0};
jalib::AtomicT b_counts[ sizeof(size_t)*8 ] = {0};
jalib::AtomicT c_counts[ sizeof(size_t)*8 ] = {0};
static void _profile_a( size_t s ){
  int bin = (int) ( log(s*1.99) / log(2.0) );
  jalib::atomicIncrement(a_counts+bin);
}
static void _profile_b( size_t bin ){
  jalib::atomicIncrement(b_counts+bin);
}
static void _profile_c( size_t bin ){
  jalib::atomicIncrement(c_counts+bin);
}
void jalloc_profile_print(){
  for(int i=0; i<10; ++i)
    printf("%10d ", i);
  printf("\n");
  for(int i=0; i<10; ++i)
    printf("%10d ", (int)a_counts[i]);
  printf("\n");
  for(int i=0; i<10; ++i)
    printf("%10d ", (int)b_counts[i]);
  printf("\n");
  for(int i=0; i<10; ++i)
    printf("%10d ", (int)c_counts[i]);
  printf("\n");
}
#else
#define _profile_a(x)
#define _profile_b(x)
#define _profile_c(x)
#endif

template<typename T>
INLINE void* _default_reallocate(void* ptr, size_t oldn, size_t newn){
  if(oldn==newn) return ptr;
  void* newptr = T::allocate(newn);
  memcpy(newptr, ptr, oldn);
  T::deallocate(ptr, oldn);
  return newptr;
}

void* jalib::JAllocRaw::allocate(size_t n) {
#if defined(JALIB_USE_MALLOC) || !defined(HAVE_MMAP) || !defined(JALIB_ALLOCATOR)
  void* p = malloc(n);
  if(p==0)
    perror("malloc: ");
#else
  void* p = mmap(NULL, n, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if(p==MAP_FAILED)
    perror("_alloc_raw: ");
#endif
  return p;
}

void jalib::JAllocRaw::deallocate(void* ptr, size_t n) {
#if defined(JALIB_USE_MALLOC) || !defined(HAVE_MMAP) || !defined(JALIB_ALLOCATOR)
  free(ptr);
  (void)n;//make n "used"
#else
  if(ptr==0 || n==0) return;
  int rv = munmap(ptr, n);
  if(rv!=0)
    perror("_dealloc_raw: ");
#endif
}

void* jalib::JAllocRaw::reallocate(void* ptr, size_t oldn, size_t newn){
#if defined(JALIB_USE_MALLOC) || !defined(HAVE_MMAP) || !defined(JALIB_ALLOCATOR)
  if(oldn==newn) return ptr;
  return realloc(ptr, newn);
#else
  return _default_reallocate<JAllocRaw>(ptr,oldn,newn);
#endif
}

#ifdef JALIB_ALLOCATOR


namespace jalib {

template < size_t _N, size_t BLOCKSIZE, int pidx>
class JFixedAllocStack {
public:
  enum { N=_N };
  JFixedAllocStack() : _root(NULL) {}

  //allocate a chunk of size N
  void* allocate() {
    _profile_b(pidx);
    if(_root == NULL) expand();
    FreeItem* item = _root;
    _root = item->next;
    item->next = NULL;
    return item;
  }

  //deallocate a chunk of size N
  void deallocate(void* ptr) {
    FreeItem* item = static_cast<FreeItem*>(ptr);
    item->next = _root;
    _root = item;
  }
protected:
  //allocate more raw memory when stack is empty
  void expand() {
    _profile_c(pidx);
    FreeItem* bufs = static_cast<FreeItem*>(JAllocRaw::allocate(BLOCKSIZE));
    int count=BLOCKSIZE / sizeof(FreeItem);
    for(int i=0; i<count-1; ++i){
      bufs[i].next=bufs+i+1;
    }
    bufs[count-1].next = _root;
    _root=bufs; 
  }
protected:
  struct FreeItem {
    union {
      FreeItem* next;
      char buf[N];
    };
  };
private:
  FreeItem* _root;
  PADDING(64 - sizeof(FreeItem*));
};

template < typename Alloc >
class JGlobalAlloc {
public:
  enum { N = Alloc::N };

  static void* allocate(){
    if(pthread_mutex_lock(theMutex()) != 0)
      perror("JGlobalAlloc::allocate");
   
    void* ptr = theAlloc().allocate();

    if(pthread_mutex_unlock(theMutex()) != 0)
      perror("JGlobalAlloc::allocate");

    return ptr;
  }

  //deallocate a chunk of size N
  static void deallocate(void* ptr){
    if(pthread_mutex_lock(theMutex()) != 0)
      perror("JGlobalAlloc::allocate");
   
    theAlloc().deallocate(ptr);

    if(pthread_mutex_unlock(theMutex()) != 0)
      perror("JGlobalAlloc::allocate");
  }
private:
  static pthread_mutex_t* theMutex() {
    static pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
    return &m;
  }
  static Alloc& theAlloc() {
    static Alloc a;
    return a;
  }
};

template < typename Alloc >
class JThreadAlloc {
public:
  enum { N = Alloc::N };

  static void* allocate(){
    void* ptr = myAlloc().allocate();
    return ptr;
  }

  static void deallocate(void* ptr){
    myAlloc().deallocate(ptr);
  }
private:
  static Alloc& myAlloc() {
    static __thread Alloc* a = NULL;
    static __thread char buf[sizeof(Alloc)];
    if(a==NULL){
      a = (Alloc*) buf;
      *a = Alloc();
    }
    return *a;
  }
};
                                                                                                    
typedef JThreadAlloc< JFixedAllocStack<64   , 1024*16 , 0> > lvl0;
typedef JThreadAlloc< JFixedAllocStack<128  , 1024*16 , 1> > lvl1;
typedef JThreadAlloc< JFixedAllocStack<192  , 1024*64 , 2> > lvl2;
//typedef JThreadAlloc< JFixedAllocStack<384  , 1024*16 , 3> > lvl3;
//typedef JThreadAlloc< JFixedAllocStack<512  , 1024*8  , 4> > lvl4;
//typedef JThreadAlloc< JFixedAllocStack<1024 , 1024*8  , 5> > lvl5;

}

void* jalib::JAlloc::allocate(size_t n) {
  _profile_a(n);
  if(n <= lvl0::N) return lvl0::allocate(); else
  if(n <= lvl1::N) return lvl1::allocate(); else
  if(n <= lvl2::N) return lvl2::allocate(); else
//if(n <= lvl3::N) return lvl3::allocate(); else
//if(n <= lvl4::N) return lvl4::allocate(); else
//if(n <= lvl5::N) return lvl5::allocate(); else
  //return _alloc_raw(n);
  return malloc(n);
}
void jalib::JAlloc::deallocate(void* ptr, size_t n){
  if(n <= lvl0::N) lvl0::deallocate(ptr); else
  if(n <= lvl1::N) lvl1::deallocate(ptr); else
  if(n <= lvl2::N) lvl2::deallocate(ptr); else
//if(n <= lvl3::N) lvl3::deallocate(ptr); else
//if(n <= lvl4::N) lvl4::deallocate(ptr); else
//if(n <= lvl5::N) lvl5::deallocate(ptr); else
  //_dealloc_raw(ptr, n);
  free(ptr);
}
void* jalib::JAlloc::reallocate(void* ptr, size_t oldn, size_t newn){
  return _default_reallocate<JAlloc>(ptr,oldn,newn);
}


void* operator new(size_t nbytes){
  size_t* p = (size_t*) jalib::JAlloc::allocate(nbytes+sizeof(size_t));
  *p = nbytes;
  p+=1;
  return p;
}

void* operator new[](size_t nbytes){
  return operator new(nbytes);
}

void operator delete(void* _p){
  if(_p==0) return;
  size_t* p = (size_t*) _p;
  p-=1;
  jalib::JAlloc::deallocate(p, *p+sizeof(size_t));
}

void operator delete[](void* _p){
  operator delete(_p);
}

#else

#include <stdlib.h>

void* jalib::JAlloc::allocate(size_t n) {
  return JAllocRaw::allocate(n);
}
void jalib::JAlloc::deallocate(void* ptr, size_t n){
  JAllocRaw::deallocate(ptr, n);
}
void* jalib::JAlloc::reallocate(void* ptr, size_t oldn, size_t newn){
  return JAllocRaw::reallocate(ptr, oldn, newn);
}

#endif


