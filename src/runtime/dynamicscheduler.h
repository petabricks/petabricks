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

#include "workerthread.h"

#include <pthread.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

int tid(void);

class DynamicScheduler{
public:
  static DynamicScheduler& instance();

  WorkerThread taskStacks[MAX_NUM_WORKERS];

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

  int lin_delay(int delay, int step, int min, int max) {

    if (delay > min) {
      for (int i = 0; i < delay; i++) {
        pause();
      }
    }
    if (delay < max - step) {
      delay += step;
    }
    return delay;
  }

  ///
  /// blocked until get a task from queue for execution
  DynamicTask *dequeue() {
    DynamicTask *task;
    //int delay = 0;
    while (true) {
      task = tryDequeue();
      if (task != NULL) {
        return task;
      }
      //delay = lin_delay(delay, 0, 100, 1000);
    }
  }

  ///
  /// unblocked method, try to get a task from queue for execution
  DynamicTask *tryDequeue() {

    DynamicTask *task = taskStacks[tid()].pop();

    if (task == NULL) {
      int stealTaskStack = taskStacks[tid()].nextTaskStack(numOfWorkers);
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

  void clearPrivateTaskStack() {
    taskStacks[tid()].clear();
  }

  void popAndRunOneTask(bool blocking);

#ifdef GRACEFUL_ABORT
  class AbortException {};
  static bool isAborting() { return theIsAborting; }
  static bool isTerminating() { return theIsTerminating; }
  void abortBegin();
  void abortEnd();
  void abortWait();
  void setAbortFlag();
  void shutdown();
  void resetAbortFlag();
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
  static bool theIsTerminating;
  int numAbortedThreads;
#endif
};

}

#endif
