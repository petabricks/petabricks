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

    char _prePadding[CACHE_LINE_SIZE];
    jalib::JMutex _lock;
    std::deque<DynamicTaskPtr> _deque;
    struct { int z; int w; } _randomNumState;
    char _postPadding[CACHE_LINE_SIZE];

   public:

    Deque()
    {
      _randomNumState.z = tid();
      _randomNumState.w = tid() + 1;
    }

    void push(const DynamicTaskPtr& t)
    {
      JLOCKSCOPE(_lock);

      _deque.push_back(t);

    }

    DynamicTaskPtr pop()
    {
      JLOCKSCOPE(_lock);

      DynamicTaskPtr retVal(NULL);

      if (!_deque.empty()) {
        DynamicTaskPtr back = _deque.back();
        if (back->state == DynamicTask::S_READY) {
          retVal = back;
          _deque.pop_back();
        }
      }

      return retVal;
    }

    DynamicTaskPtr steal()
    {
      DynamicTaskPtr retVal(NULL);

      if (!_lock.trylock()) {
        return retVal;
      }

      if (!_deque.empty()) {
        DynamicTaskPtr front = _deque.front();
        if (front->state == DynamicTask::S_READY) {
          retVal = front;
          _deque.pop_front();
        }
      }

      _lock.unlock();

      return retVal;
    }

    int nextDeque(int numThreads) {

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
  Deque deques[MAX_NUM_WORKERS];

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
    queue.push(t);
  }


  ///
  /// blocked until get a task from queue for execution
  DynamicTaskPtr dequeue() {
    DynamicTaskPtr task;
    do {
      task = tryDequeue();
    } while (!task);
  }

  ///
  /// unblocked method, try to get a task from queue for execution
  DynamicTaskPtr tryDequeue() {

    DynamicTaskPtr task = deques[tid()].pop();

    if (!task) {
      int stealDeque = deques[tid()].nextDeque(numOfWorkers);
      task = deques[stealDeque].steal();
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

  /*
  bool empty() {
    return queue.empty();
  }

  size_t size() {
    return queue.size();
  }
  */

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
