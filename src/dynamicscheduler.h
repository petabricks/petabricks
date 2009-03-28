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
typedef jalib::JRef<DynamicTask> DynamicTaskPtr;
typedef jalib::JBlockingQueue<DynamicTaskPtr> DynamicTaskQueue;

int tid(void);

class DynamicScheduler{

private:
  class Deque {
    private:
      long _size;
      DynamicTaskPtr *_array;
      long _h; // head index
      jalib::JMutex _lock;
      char padding[56];
      long _t; // tail index

    public:

    Deque() {
      _t = 0;
      _h = 0;
      _size = 1000000;
      _array = (DynamicTaskPtr *) malloc(sizeof(DynamicTaskPtr) * _size); // Use malloc so we can realloc
    }

    ~Deque() {
      free(_array);
    }

    void push(const DynamicTaskPtr& task) {
      _array[_t] = task;
      _t++;

      JASSERT(_t < _size - 1);

      /*
      if (_t >= _size - 1) {
        JLOCKSCOPE(_lock);

        _size *= 3;
        _array = (DynamicTaskPtr *) realloc(_array, sizeof(DynamicTaskPtr) * _size);
      }
      */
    }

#if 1
    DynamicTaskPtr pop() {

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

    DynamicTaskPtr pop_bottom() {

      if (_h == _t || !_lock.trylock()) {
        return NULL;
      }

      DynamicTaskPtr retVal;

      if (_h != _t) {
        retVal = _array[_h];
        _h++;
      }

      _lock.unlock();

      return retVal;
    }

#else

    DynamicTaskPtr pop() {

      if (_h == _t) {
        return NULL;
      }

      long t = _t - 1;
      DynamicTaskPtr retVal = _array[t];

      jalib::loadFence();

      _t = t;

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
        return retVal;
      }
    }

    DynamicTaskPtr pop_bottom() {

      if (_h == _t || !_lock.trylock()) {
        return NULL;
      }

      long h = _h;
      DynamicTaskPtr retVal = _array[h];

      jalib::loadFence();

      _h = h + 1;

      jalib::loadFence();

      if (_h > _t) {
        _h--;
        _lock.unlock();
        return NULL;
      }

      _lock.unlock();

      return retVal;
    }
#endif
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

    void push(const DynamicTaskPtr& t)
    {
      if (t->isContinuation)
        _cont_deque.push(t);
      else
        _deque.push(t);
    }

    DynamicTaskPtr pop()
    {
      DynamicTaskPtr retVal;

      retVal = _deque.pop();
      if (!retVal) {
        retVal = _cont_deque.pop();
      }

      return retVal;
    }

    DynamicTaskPtr steal()
    {
      DynamicTaskPtr retVal;

      retVal = _cont_deque.pop_bottom();
      if (!retVal) {
        retVal = _deque.pop_bottom();
      }

      return retVal;
    }

    int nextTaskStack(int numThreads) {

      return (getRandInt() % numThreads);
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
  void enqueue(const DynamicTaskPtr& t) {
#ifdef GRACEFUL_ABORT
    if(isAborting()){
      throw AbortException();
    }
#endif
    taskStacks[tid()].push(t);
  }


  ///
  /// blocked until get a task from queue for execution
  DynamicTaskPtr dequeue() {
    DynamicTaskPtr task;
    do {
      task = tryDequeue();
    } while (!task);
    return task;
  }

  ///
  /// unblocked method, try to get a task from queue for execution
  DynamicTaskPtr tryDequeue() {

    DynamicTaskPtr task = taskStacks[tid()].pop();

    if (!task) {
      int stealTaskStack = taskStacks[tid()].nextTaskStack(numOfWorkers + 1);
      task = taskStacks[stealTaskStack].steal();
    }

#ifndef GRACEFUL_ABORT
    return task;
#else
    if(task && !isAborting()) {
      return task;
    } else {
      JLOCKSCOPE(theAbortingLock);
      if(isAborting()) {
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


#ifdef QUEUE_STATISTICS
  unsigned int contention() {
    return queue.contention();
  }

  unsigned int totalEnqueue() {
    return queue.totalEnqueue();
  }

  uint64_t totalEmptyTime() {
    return queue.totalEmptyTime();
  }

  uint64_t totalFilledTime() {
    return queue.totalFilledTime();
  }
#endif // QUEUE_STATISTICS

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
  /// Blocking queue for ready tasks
  DynamicTaskQueue queue;

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
