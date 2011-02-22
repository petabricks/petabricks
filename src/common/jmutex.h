/***************************************************************************
 *   Copyright (C) 2006-2009 by Jason Ansel                                *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
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
  
  bool trylock() const {
    memFence();
    return _v==0 && fetchAndStore(&_v, 1)==0;
  }

  void unlock() const {
    memFence();
    _v = 0;
    memFence();//not needed?
  }
  
  void lock() const {
    while(!trylock()) {
      staticMemFence();
    }
  }

protected:
  ///
  /// 0 for unlocked, 1 for locked
  mutable long _v;
  PADDING(CACHE_LINE_SIZE - sizeof(long));
} __attribute__((aligned(CACHE_LINE_SIZE)));


class JMutexPthread{
  JMutexPthread(const JMutexPthread&); //banned
public:
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
  ~JMutexPthread(){
    JASSERT(pthread_mutex_destroy(&_mux)==0);
  }

  void lock() const {JASSERT(pthread_mutex_lock(&_mux)   == 0);}
  bool trylock() const {return pthread_mutex_trylock(&_mux) == 0;}
  void unlock() const {JASSERT(pthread_mutex_unlock(&_mux) == 0);}
protected:
  mutable pthread_mutex_t _mux;
};

//a mutex and a condition variable
class JCondMutex : public JMutexPthread {
public:
  JCondMutex(){ JASSERT( pthread_cond_init(&_cond, NULL) == 0); }
  ~JCondMutex(){JASSERT( pthread_cond_destroy(&_cond) == 0); }

  void wait() const      {JASSERT(pthread_cond_wait(&_cond, &_mux) == 0);}
  void signal() const    {JASSERT(pthread_cond_signal(&_cond)      == 0);}
  void broadcast() const {JASSERT(pthread_cond_broadcast(&_cond)   == 0);}
protected:
  mutable pthread_cond_t _cond;
};

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

#define _JLOCKSCOPE_CAT(a, b) a ## b
#define JLOCKSCOPE_CAT(a, b) _JLOCKSCOPE_CAT(a, b)
#define JLOCKSCOPE(m) jalib::JLockScope JLOCKSCOPE_CAT(__scopeLock , __LINE__ ) ( m )

}

#endif
