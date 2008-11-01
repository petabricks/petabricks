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

#include <pthread.h>
#include <list>


namespace hecura {

// The major work of dynamic scheduler is to maintain two queues:
// * ready queue: a queue of tasks ready to run
// * wait  queue: a queue of tasks with unmet dependencies

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
  void startWorkerThreads();
  
  /// 
  /// add a new task into scheduler
  void enqueueNewTask(DynamicTaskPtr task);

  ///
  /// add the new task into the ready queue
  void enqueueReadyQueue(DynamicTaskPtr task);

  ///
  /// remove a task from the ready queue
  /// if ready queue is empty, return NULL
  /// so it never block
  DynamicTaskPtr dequeueReadyQueueNonblocking();
  DynamicTaskPtr dequeueReadyQueueBlocking();

  ///
  /// remove the specific task from the ready queue
  void dequeueReadyQueue(DynamicTaskPtr task);

  ///
  /// add the task in to the wait queue
  void enqueueWaitQueue(DynamicTaskPtr task);

  ///
  /// remove the task from the wait queue
  void dequeueWaitQueue(DynamicTaskPtr task);

  ///
  /// clean the wait queue
  void cleanWaitQueue();

  ///
  ///
  void moveWait2Ready(DynamicTaskPtr task);

  ///
  /// lock the mutex
  void mutexLock() { 
    mutex.lock(); 
  }

  ///
  /// unlock the mutex
  void mutexUnlock() { 
    mutex.unlock(); 
  }

  ///
  /// conditional wait
  void condWait() { mutex.wait(); }

  ///
  /// signal the conditional variables
  void condSignal() { mutex.signal(); }

 protected:

  ///
  /// mutex for accessing the queue
  jalib::JCondMutex mutex;

  ///
  /// Total number of worker threads
  int numOfWorkers;

  ///
  /// worker threads for task execution
  pthread_t *workerThreads;
  
  ///
  /// queue for ready tasks  
  std::list<DynamicTaskPtr> readyQueue;

  ///
  /// queue for waiting tasks
  std::list<DynamicTaskPtr> waitQueue;
};

}

#endif
