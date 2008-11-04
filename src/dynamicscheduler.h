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
  /// add a ready task into queue
  void enqueue(const DynamicTaskPtr& t) {
    queue.push(t);
//     JTRACE("added ready task")(queue.size());
  }

  
  ///
  /// blocked until get a task from queue for execution 
  DynamicTaskPtr dequeue() {
    DynamicTaskPtr task = queue.pop();
//     JTRACE("running task")(queue.size());
    return task;
  }


  ///
  /// unblocked method, try to get a task from queue for execution
  DynamicTaskPtr tryDequeue() {
    DynamicTaskPtr task = queue.tryPop();
//     JTRACE("waiting and trying to run task")(queue.size());
    return task;
  }

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
};

}

#endif
