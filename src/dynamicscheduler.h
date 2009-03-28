/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
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
#ifndef PETABRICKSDYNAMICSCHEDULER_H
#define PETABRICKSDYNAMICSCHEDULER_H

#include "jmutex.h"
#include "jrefcounted.h"
#include "jblockingqueue.h"
#include "jmutex.h"
#include "jasm.h"
#include "dynamictask.h"

#include <pthread.h>
#include <list>
#include <set>
#include <deque>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

// The major work of dynamic scheduler is to maintain two queues:
// * ready queue: a queue of tasks ready to run

// forward declarsion for DynamicTaskPtr
//class DynamicTask;
//typedef jalib::JRef<DynamicTask> DynamicTaskPtr;
//typedef jalib::JBlockingQueue<DynamicTaskPtr> DynamicTaskQueue;

int tid(void);

class DynamicScheduler{

private:
  class Deque {
    private:
      long _size;
      DynamicTask **_array;
      long _h; // head index
      jalib::JMutex _lock;
      char padding[56];
      long _t; // tail index

    public:

    Deque() {
      _t = 0;
      _h = 0;
      _size = 1000;
      _array = (DynamicTask **) malloc(sizeof(DynamicTask *) * _size); // Use malloc so we can realloc
    }

    ~Deque() {
      free(_array);
    }

    void push(DynamicTask *task) {

      _array[_t] = task;
      _t++;

      if (_t >= _size - 1) {
        JLOCKSCOPE(_lock);

        _size *= 3;
        _array = (DynamicTask **) realloc(_array, sizeof(DynamicTaskPtr) * _size);
      }
    }

#if 0
    DynamicTask *pop() {

      if (_h == _t) {
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

    DynamicTask *pop_bottom() {

      if ( _h == _t || !_lock.trylock()) {
        return NULL;
      }

      DynamicTask *retVal = NULL;

      if (_h != _t) {
        retVal = _array[_h];
        _h++;
      }

      _lock.unlock();

      return retVal;
    }

#else

    DynamicTask *pop() {

      if (_h == _t) {
        return NULL;
      }

      _t--;

      jalib::loadFence();

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

    DynamicTask *pop_bottom() {

      if (_h == _t || !_lock.trylock()) {
        return NULL;
      }

      _h++;

      jalib::loadFence();

      if (_h > _t) {
        _h--;
        _lock.unlock();
        return NULL;
      }

      DynamicTask *task = _array[_h - 1];

      _lock.unlock();

      return task;
    }
#endif

    void clear() {
      JLOCKSCOPE(_lock);
      _h = 0;
      _t = 0;
    }

  };

  class TaskStack {

    char _prePadding[CACHE_LINE_SIZE];
    Deque _deque;
    Deque _cont_deque;
    struct { int z; int w; } _randomNumState;
    char _postPadding[CACHE_LINE_SIZE];

   public:

    TaskStack()
    {
      _randomNumState.z = tid();
      _randomNumState.w = tid() + 1;
    }

    void push(DynamicTask *t)
    {
      if (t->isContinuation)
        _cont_deque.push(t);
      else
        _deque.push(t);
    }

    DynamicTask *pop()
    {
      DynamicTask *retVal = _deque.pop();
      if (retVal == NULL) {
        retVal = _cont_deque.pop();
      }

      return retVal;
    }

    DynamicTask *steal()
    {
      DynamicTask *retVal = _cont_deque.pop_bottom();
      if (retVal == NULL) {
        retVal = _deque.pop_bottom();
      }

      return retVal;
    }

    int nextTaskStack(int numThreads) {

      return (getRandInt() % numThreads);
    }

    void clear() {
      _deque.clear();
      _cont_deque.clear();
    }

   private:

    int getRandInt() {
      _randomNumState.z = 36969 * (_randomNumState.z & 65535) + (_randomNumState.z >> 16);
      _randomNumState.w = 18000 * (_randomNumState.w & 65535) + (_randomNumState.w >> 16);
      return (_randomNumState.z << 16) + _randomNumState.w;
    }
  } __attribute__ ((aligned (64)));

public:
  TaskStack taskStacks[MAX_NUM_WORKERS];

  static int myThreadID();

  ///
  /// constructor
  DynamicScheduler();

  ///
  /// destructor
  ~DynamicScheduler();

  ///
  /// start worker threads
  void startWorkerThreads(int newWorkers);

  ///
  /// add a ready task into queue
  void enqueue(DynamicTask *t) {
#ifdef GRACEFUL_ABORT
    if(isAborting()){
      throw AbortException();
    }
#endif
    taskStacks[tid()].push(t);
  }


  ///
  /// blocked until get a task from queue for execution
  DynamicTask *dequeue() {
    DynamicTask *task;
    do {
      task = tryDequeue();
    } while (task == NULL);
    return task;
  }

  ///
  /// unblocked method, try to get a task from queue for execution
  DynamicTask *tryDequeue() {

    DynamicTask *task = taskStacks[tid()].pop();

    if (task) {
      JASSERT(task->state == DynamicTask::S_READY);
    }

    if (task == NULL) {
      int stealTaskStack = taskStacks[tid()].nextTaskStack(numOfWorkers + 1);
      task = taskStacks[stealTaskStack].steal();

      if (task) {
        JASSERT(task->state == DynamicTask::S_READY);
      }
    }

#ifndef GRACEFUL_ABORT
    return task;
#else
    if(task && !isAborting()) {
      return task;
    } else {
      JLOCKSCOPE(theAbortingLock);
      if(isAborting()) {
        //taskStacks[tid()].clear();
        throw AbortException();
      } else {
        return 0;
      }
    }
#endif
  }

  int workers() {
    return numOfWorkers;
  }


  void popAndRunOneTask(bool blocking);


#ifdef GRACEFUL_ABORT
  class AbortException {};
  static bool isAborting() { return theIsAborting; }
  void abortBegin();
  void abortEnd();
  void abortWait();
  void setAbortFlag();
#endif
protected:
  ///
  /// Total number of worker threads
  int numOfWorkers;

  ///
  /// worker threads for task execution
  pthread_t *workerThreads;

#ifdef GRACEFUL_ABORT
  static jalib::JCondMutex theAbortingLock;
  static bool theIsAborting;
  int numAbortedThreads;
#endif
};

}

#endif
