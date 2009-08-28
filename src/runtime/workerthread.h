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
#ifndef PETABRICKSWORKERTHREAD_H
#define PETABRICKSWORKERTHREAD_H

#include "dynamictask.h"
#include "thedeque.h"

#include <pthread.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


namespace petabricks {

int tid();

class WorkerThread {
  typedef THEDeque<DynamicTask*> Deque;
  PADDING(CACHE_LINE_SIZE);
  Deque _deque;
  struct { int z; int w; } _randomNumState;
  PADDING(CACHE_LINE_SIZE);

 public:

  WorkerThread()
  {
    _randomNumState.z = tid() * tid() * 2;
    _randomNumState.w = tid() + 1;
  }

  void push(DynamicTask *t)
  {
    _deque.push(t);
  }

  DynamicTask *pop()
  {
    return _deque.pop_lock_free();
  }

  DynamicTask *steal()
  {
    return _deque.pop_bottom_lock_free();
  }

  int nextTaskStack(int numThreads) {
    int next = (getRandInt() % numThreads);
    return next;
  }

  void clear() {
    _deque.clear();
  }

  bool isEmpty() {
    return _deque.isEmpty();
  }

 private:

  int getRandInt() {
    _randomNumState.z = 36969 * (_randomNumState.z & 65535) + (_randomNumState.z >> 16);
    _randomNumState.w = 18000 * (_randomNumState.w & 65535) + (_randomNumState.w >> 16);
    int retVal = (_randomNumState.z << 16) + _randomNumState.w;
    if (retVal < 0) {
      retVal = -retVal;
    }
    return retVal;
  }
} __attribute__ ((aligned (64)));

}

#endif
