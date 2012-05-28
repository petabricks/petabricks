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
#ifndef PETABRICKSTHEDEQUE_H
#define PETABRICKSTHEDEQUE_H

#include "jalloc.h"
#include "jasm.h"
#include "jmutex.h"

#include <deque>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

/** 
 * Implementation of the Cilk THE protocol for a work stealing deque 
 * Supports a SINGLE thread at the top, and MANY threads at the bottom
 */
template < typename T >
class THEDeque {
public:
  THEDeque() {
    _t = 0;
    _h = 0;
    _size = 1000;
    _array = (T*) malloc(sizeof(T) * _size); // Use malloc so we can realloc
  }
  ~THEDeque() {
    free(_array);
  }

  ///
  /// Called by owner thread
  void push_top(const T& task) {
    if(empty() && _h>0){
      //reset the head/tail pointers
      JLOCKSCOPE(_lock);
      if(empty())
        _t=_h=0;
    }

    _array[_t] = task;
    _t++;

    if (_t >= _size - 1) {
      //resize the array
      JLOCKSCOPE(_lock);
      _size *= 3;
      _array = (T*) realloc(_array, sizeof(T) * _size);
    }
  }

  ///
  /// Called by owner thread
  T pop_top() {

    if (empty()) {
      return NULL;
    }

    _t--;

    jalib::memFence();

    if (_h > _t) {
      _t++;
      {
        JLOCKSCOPE(_lock);
        if (empty()) {
          return NULL;
        }
        _t--;
        return _array[_t];
      }
    } else {
      return _array[_t];
    }
  }

  ///
  /// Called by stealer thread
  T pop_bottom() {

    if (empty() || !_lock.trylock()) {
      return NULL;
    }

    _h++;

    jalib::memFence();

    if (_h > _t) {
      _h--;
      _lock.unlock();
      return NULL;
    }

    T task = _array[_h - 1];

    _lock.unlock();

    return task;
  }

  bool empty() const {
    return _h >= _t;
  }
  int size() const {
    return (int)(_t - _h);
  }
private:
  long _size;
  T* _array;
  PADDING(CACHE_LINE_SIZE - sizeof(long) - sizeof(T*));
  long _h; // head index
  PADDING(CACHE_LINE_SIZE - sizeof(long));
  long _t; // tail index
  PADDING(CACHE_LINE_SIZE - sizeof(long));
  jalib::JMutexSpin _lock;
} __attribute__((aligned(CACHE_LINE_SIZE)));

/**
 * A deque protected by a lock
 * Supports a MANY thread at the top, and MANY threads at the bottom
 */
template < typename T >
class LockingDeque {
public:
  void push_top(const T& task) { 
    JLOCKSCOPE(_lock);
    _deque.push_back(task);
  }
  void push_bottom(const T& task) { 
    JLOCKSCOPE(_lock);
    _deque.push_front(task);
  }
  T pop_top() {
    JLOCKSCOPE(_lock);
    if(_deque.empty()) return NULL;
    T t = _deque.back();
    _deque.pop_back();
    return t;
  }
  T pop_bottom() {
    if(_deque.empty() || !_lock.trylock()) return NULL;
    T t = NULL;
    if(!_deque.empty()){
      t = _deque.front();
      _deque.pop_front();
    }
    _lock.unlock();
    return t;
  }
  int size() const { return (int)_deque.size(); }
  bool empty() const { return _deque.empty(); }
private:
  jalib::JMutex _lock;
  std::deque<T> _deque;
};
}

#endif

