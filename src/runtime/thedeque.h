/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#ifndef PETABRICKSTHEDEQUE_H
#define PETABRICKSTHEDEQUE_H

#include "common/jasm.h"
#include "common/jmutex.h"

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
  PADDING(CACHE_LINE_SIZE);
  long _h; // head index
  PADDING(CACHE_LINE_SIZE);
  long _t; // tail index
  PADDING(CACHE_LINE_SIZE);
  jalib::JMutex _lock;
  PADDING(CACHE_LINE_SIZE);
};

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
  std::deque<T> _deque;
  jalib::JMutex _lock;
};
}

#endif

