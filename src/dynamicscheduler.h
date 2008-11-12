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
#ifndef HECURADYNAMICSCHEDULER_H
#define HECURADYNAMICSCHEDULER_H

#include "jmutex.h"
#include "jrefcounted.h"
#include "jblockingqueue.h"

#include <pthread.h>
#include <list>
#include <set>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace hecura {

// The major work of dynamic scheduler is to maintain two queues:
// * ready queue: a queue of tasks ready to run

// forward declarsion for DynamicTaskPtr
class DynamicTask;
typedef jalib::JRef<DynamicTask> DynamicTaskPtr;


class DynamicScheduler{
public:
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
    DynamicTaskPtr task = queue.pop();
#ifndef GRACEFUL_ABORT
    return task;
#else
    if(task && !isAborting()){
      return task;
    }else{
      throw AbortException();
    }
#endif
  }


  ///
  /// unblocked method, try to get a task from queue for execution
  DynamicTaskPtr tryDequeue() {
    DynamicTaskPtr task = queue.tryPop();
#ifndef GRACEFUL_ABORT
    return task;
#else
    if(task && !isAborting()){
      return task;
    }else{
      JLOCKSCOPE(theAbortingLock);
      if(isAborting())
        throw AbortException();
      else
        return 0;
    }
#endif
  }

  bool empty() {
    return queue.empty();
  }

  size_t size() {
    return queue.size();
  }

  int workers() {
    return numOfWorkers;
  }


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
#endif
protected:
  ///
  /// Blocking queue for ready tasks
  jalib::JBlockingQueue<DynamicTaskPtr> queue;

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
