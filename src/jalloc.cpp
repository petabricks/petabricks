/****************************************************************************
 *   Copyright (C) 2006-2008 by Jason Ansel                                 *
 *   jansel@csail.mit.edu                                                   *
 *                                                                          *
 *   This file is part of the JALIB module of DMTCP (DMTCP:dmtcp/jalib).    *
 *                                                                          *
 *  DMTCP:dmtcp/jalib is free software: you can redistribute it and/or      *
 *  modify it under the terms of the GNU Lesser General Public License as   *
 *  published by the Free Software Foundation, either version 3 of the      *
 *  License, or (at your option) any later version.                         *
 *                                                                          *
 *  DMTCP:dmtcp/src is distributed in the hope that it will be useful,      *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 *  GNU Lesser General Public License for more details.                     *
 *                                                                          *
 *  You should have received a copy of the GNU Lesser General Public        *
 *  License along with DMTCP:dmtcp/src.  If not, see                        *
 *  <http://www.gnu.org/licenses/>.                                         *
 ****************************************************************************/

#include "jalloc.h"
#include "jasm.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


#ifdef JALIB_ALLOCATOR

#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

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

namespace jalib
{

inline void* _alloc_raw(size_t n) {
#if defined(JALIB_USE_MALLOC) || !defined(HAVE_MMAP)
  return malloc(n);
#else
  void* p = mmap(NULL, n, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if(p==MAP_FAILED)
    perror("_alloc_raw: ");
  return p;
#endif
}

inline void _dealloc_raw(void* ptr, size_t n) {
#if defined(JALIB_USE_MALLOC) || !defined(HAVE_MMAP)
  free(ptr);
#else
  if(ptr==0 || n==0) return;
  int rv = munmap(ptr, n);
  if(rv!=0)
    perror("_dealloc_raw: ");
#endif
}

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
    FreeItem* bufs = static_cast<FreeItem*>(_alloc_raw(BLOCKSIZE));
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
                                                                                                    
typedef JThreadAlloc< JFixedAllocStack<64   , 1024*64  , 0> > lvl0;
typedef JThreadAlloc< JFixedAllocStack<192  , 1024*256 , 1> > lvl1;
typedef JThreadAlloc< JFixedAllocStack<512  , 1024*8   , 2> > lvl2;
typedef JThreadAlloc< JFixedAllocStack<1024 , 1024*8   , 3> > lvl3;
typedef JThreadAlloc< JFixedAllocStack<2048 , 1024*8   , 4> > lvl4;
typedef JThreadAlloc< JFixedAllocStack<4096 , 1024*8   , 5> > lvl5;

void* JAllocDispatcher::allocate(size_t n) {
  _profile_a(n);
  if(n <= lvl0::N) return lvl0::allocate(); else
  if(n <= lvl1::N) return lvl1::allocate(); else
  if(n <= lvl2::N) return lvl2::allocate(); else
  if(n <= lvl3::N) return lvl3::allocate(); else
  if(n <= lvl4::N) return lvl4::allocate(); else
  if(n <= lvl5::N) return lvl5::allocate(); else
  return _alloc_raw(n);
}
void JAllocDispatcher::deallocate(void* ptr, size_t n){
  if(n <= lvl0::N) lvl0::deallocate(ptr); else
  if(n <= lvl1::N) lvl1::deallocate(ptr); else
  if(n <= lvl2::N) lvl2::deallocate(ptr); else
  if(n <= lvl3::N) lvl3::deallocate(ptr); else
  if(n <= lvl4::N) lvl4::deallocate(ptr); else
  if(n <= lvl5::N) lvl5::deallocate(ptr); else
  _dealloc_raw(ptr, n);
}
}

void* operator new(size_t nbytes){
  size_t* p = (size_t*) jalib::JAllocDispatcher::allocate(nbytes+sizeof(size_t));
  *p = nbytes;
  p+=1;
  return p;
}

void operator delete(void* _p){
  size_t* p = (size_t*) _p;
  p-=1;
  jalib::JAllocDispatcher::deallocate(p, *p+sizeof(size_t));
}

#else

#include <stdlib.h>

void* jalib::JAllocDispatcher::allocate(size_t n) {
  return malloc(n);
}
void jalib::JAllocDispatcher::deallocate(void* ptr, size_t){
  free(ptr);
}

#endif


