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
#ifndef JALIBJMUTEX_H
#define JALIBJMUTEX_H

#include "jassert.h"
#include "jasm.h"

#include <pthread.h>

#ifdef __APPLE__
#include <sched.h>
#define pthread_yield sched_yield
#endif //__APPLE__

#define JMUTEX_ALIGNMENT 128

namespace jalib {

class JMutexPthread;
class JMutexSpin;
typedef JMutexSpin JMutex;

class JMutexSpin{
  JMutexSpin(const JMutexSpin&); //banned
public:
  JMutexSpin() : _v(0) {}
  
  INLINE bool _trylock() const {
    return _v==0 && fetchAndStore(&_v, 1)==0;
  }

  INLINE bool trylock() const {
#ifdef DEBUG
    if(_trylock()) {
      _owner = pthread_self();
      return true;
    }
    return false;
#else
    return _trylock();
#endif
  }

  

  INLINE void unlock() const {
    staticMemFence();
#ifdef DEBUG
    JASSERT(_v==1);
    JASSERT(pthread_equal(_owner, pthread_self()));
    memset(&_owner, -1, sizeof _owner);
#endif
    _v = 0;
  }
  
  INLINE void lock() const {
#ifdef DEBUG
    JASSERT(_v==0 || !pthread_equal(_owner, pthread_self()));
#endif
    while(!trylock()) {
      staticMemFence();
    }
  }

protected:
  ///
  /// 0 for unlocked, 1 for locked
  mutable long _v;
  PADDING(CACHE_LINE_SIZE - sizeof(long));
#ifdef DEBUG
  mutable pthread_t _owner;
#endif
} __attribute__((aligned(CACHE_LINE_SIZE)));


class JMutexPthread{
  JMutexPthread(const JMutexPthread&); //banned
public:
  enum RecursiveT { RECURSIVE };

  JMutexPthread(){
    #ifdef DEBUG
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
    JASSERT(pthread_mutex_init(&_mux, &attr)==0);
    pthread_mutexattr_destroy(&attr);
    #else
    JASSERT(pthread_mutex_init(&_mux, 0)==0);
    #endif
  }

  JMutexPthread(RecursiveT){
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    JASSERT(pthread_mutex_init(&_mux, &attr)==0);
    pthread_mutexattr_destroy(&attr);
  }


  ~JMutexPthread(){
    JASSERT(pthread_mutex_destroy(&_mux)==0);
  }

  void lock() const {
    int rv = pthread_mutex_lock(&_mux);
    JASSERT(rv == 0)(rv)(EINVAL); 
  }
  bool trylock() const {return pthread_mutex_trylock(&_mux) == 0;}
  void unlock() const {JASSERT(pthread_mutex_unlock(&_mux) == 0);}
protected:
  mutable pthread_mutex_t _mux;
};

//a mutex and a condition variable
class JCondMutex : public JMutexPthread {
public:
  JCondMutex() : JMutexPthread(RECURSIVE) {
    JASSERT(pthread_cond_init(&_cond, NULL) == 0);
  }
  ~JCondMutex(){JASSERT( pthread_cond_destroy(&_cond) == 0); }

  void wait() const      {JASSERT(pthread_cond_wait(&_cond, &_mux) == 0);}
  void signal() const    {JASSERT(pthread_cond_signal(&_cond)      == 0);}
  void broadcast() const {JASSERT(pthread_cond_broadcast(&_cond)   == 0);}
protected:
  mutable pthread_cond_t _cond;
};

#if 0
class JLockScope{
  JLockScope(const JLockScope&); //banned
public:
  //support both spin and pthread versions
  JLockScope(const JMutexSpin& mux)    : _s(&mux), _p(0)     { mux.lock(); }
  JLockScope(const JMutexPthread& mux) : _s(0),    _p(&mux)  { mux.lock(); }
  ~JLockScope() {
    if(_s!=0)      _s->unlock();
    else if(_p!=0) _p->unlock();
  }
private:
  const JMutexSpin*    _s;
  const JMutexPthread* _p;
};
#else
template<typename T>
class JLockScope{
  JLockScope(const JLockScope&); //banned
public:
  //support both spin and pthread versions
  JLockScope(const T& mux) : _m(mux) { mux.lock(); }
  ~JLockScope() { _m.unlock(); }
private:
  const T& _m;
};

#endif

#define _JLOCKSCOPE_CAT(a, b) a ## b
#define JLOCKSCOPE_CAT(a, b) _JLOCKSCOPE_CAT(a, b)
#define JLOCKSCOPE(m) jalib::JLockScope<typeof(m)> JLOCKSCOPE_CAT(__scopelock , __LINE__ ) ( m )

}

#endif
