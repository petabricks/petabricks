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

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

template < typename T >
class THEDeque {
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

  void push(const T& task) {

    _array[_t] = task;
    _t++;

    if (_t >= _size - 1) {
      JLOCKSCOPE(_lock);

      _size *= 3;
      _array = (T*) realloc(_array, sizeof(T) * _size);
    }
  }

  T pop() {

    if (isEmpty()) {
      return NULL;
    }

    JLOCKSCOPE(_lock);

    if (_h == _t) {
      return NULL;
    } else {
      _t--;
      return _array[_t];
    }
  }

  T pop_bottom() {

    if (isEmpty() || !_lock.trylock()) {
      return NULL;
    }

    T retVal = NULL;

    if (_h != _t) {
      retVal = _array[_h];
      _h++;
    }

    _lock.unlock();

    return retVal;
  }


  T pop_lock_free() {

    if (isEmpty()) {
      return NULL;
    }

    _t--;

    jalib::memFence();

    if (_h > _t) {
      _t++;
      {
        JLOCKSCOPE(_lock);
        if (_h >= _t) {
          return NULL;
        }
        _t--;
        return _array[_t];
      }
    } else {
      return _array[_t];
    }
  }

  T pop_bottom_lock_free() {

    if (isEmpty() || !_lock.trylock()) {
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

  void clear() {
    JLOCKSCOPE(_lock);

    for (int i = _h; i < _t; i++) {
      _array[i]->runWrapper(true);
    }

    _h = 0;
    _t = 0;
  }

  bool isEmpty() {
    return _h == _t;
  }

};
}

#endif

