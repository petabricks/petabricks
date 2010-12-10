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

#include <pthread.h>

#ifdef __APPLE__
#include <sched.h>
#define pthread_yield sched_yield
#endif //__APPLE__


namespace jalib {

class JMutex{
  JMutex(const JMutex&); //banned
public:
  JMutex(){ pthread_mutex_init(&_mux, NULL); }
  ~JMutex(){ pthread_mutex_destroy(&_mux); }

  void lock() const {JASSERT(pthread_mutex_lock(&_mux)   == 0);}
  bool trylock() const {return pthread_mutex_trylock(&_mux) == 0;}
  void unlock() const {JASSERT(pthread_mutex_unlock(&_mux) == 0);}
protected:
  mutable pthread_mutex_t _mux;
};

//a mutex and a condition variable
class JCondMutex : public JMutex {
public:
  JCondMutex(){  pthread_cond_init(&_cond, NULL); }
  ~JCondMutex(){ pthread_cond_destroy(&_cond); }

  void wait() const      {JASSERT(pthread_cond_wait(&_cond, &_mux) == 0);}
  void signal() const    {JASSERT(pthread_cond_signal(&_cond)      == 0);}
  void broadcast() const {JASSERT(pthread_cond_broadcast(&_cond)   == 0);}
protected:
  mutable pthread_cond_t _cond;
};

class JLockScope{
  JLockScope(const JLockScope&); //banned
public:
  JLockScope(const JMutex& mux) : _mux(mux) { _mux.lock(); }
  ~JLockScope() { _mux.unlock(); }
private:
  const JMutex& _mux;
};

#define _JLOCKSCOPE_CAT(a, b) a ## b
#define JLOCKSCOPE_CAT(a, b) _JLOCKSCOPE_CAT(a, b)
#define JLOCKSCOPE(m) jalib::JLockScope JLOCKSCOPE_CAT(__scopeLock , __LINE__ ) ( m )

}

#endif
