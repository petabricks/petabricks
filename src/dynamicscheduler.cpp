/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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

#include "dynamicscheduler.h"
#include "dynamictask.h"

#include "jtunable.h"

#include <pthread.h>
#include <unistd.h>
 
#define VERBOSE


#define MIN_NUM_WORKERS  0
#define MAX_NUM_WORKERS  512

JTUNABLE(tunerNumOfWorkers, 16, MIN_NUM_WORKERS, MAX_NUM_WORKERS);

namespace hecura {

extern "C" {
void *workerStartup(void *);
}


DynamicScheduler::DynamicScheduler()
{
  numOfWorkers = tunerNumOfWorkers;
#ifdef VERBOSE
  printf("thread %d, total number of workers %d\n", 
	 pthread_self(), numOfWorkers);
#endif
}


DynamicScheduler::~DynamicScheduler()
{
  // wait for all worker to exit
  //for(unsigned int i = 0; i < numOfWorkers; i++) {
  //  pthread_join(workerThreads[i], NULL);
  //}
}


void DynamicScheduler::startWorkerThreads()  
{
  // allocat and spawn a certain number of thread
  workerThreads   = new pthread_t[numOfWorkers];
  for(unsigned int i = 0; i < numOfWorkers; i++) {
    JASSERT(pthread_create(&workerThreads[i], NULL, workerStartup, (void *)this) == 0);
  }
}



void DynamicScheduler::enqueueNewTask(DynamicTaskPtr task)
{
  // first check if there is any dependent,
  // if no add task into the ready queue
  // else  add task into the  wait queue
  task->lock();
  if(task->isReady()) {
    enqueueReadyQueue(task);
  } else {
    enqueueWaitQueue(task);
  }
  task->unlock();
}


/*
 * Warning, now do not consider the data race
 * assume proper lock is applied when called
 */
void DynamicScheduler::enqueueReadyQueue(DynamicTaskPtr task)
{
#ifdef VERBOSE
  printf("thread %d: add task %p into ready queue\n", 
	 pthread_self(), task.asPtr());
#endif
  mutexLock();
  // append task to end of the ready queue
  readyQueue.push_back(task);
  condSignal();
  mutexUnlock();
}


DynamicTaskPtr DynamicScheduler::dequeueReadyQueueNonblocking()
{
  mutexLock();
  // remove task from the ready queue
  if(readyQueue.empty()){
    mutexUnlock();
    return NULL;
  }

  // simply return the first task in the ready queue
  DynamicTaskPtr task = readyQueue.front();
  readyQueue.pop_front();
  mutexUnlock();

#ifdef VERBOSE  
  printf("thread %d: remove task %p from ready queue\n", 
	 pthread_self(), task.asPtr());
#endif

  return task;
}

DynamicTaskPtr DynamicScheduler::dequeueReadyQueueBlocking()
{
  mutexLock();
  while(readyQueue.empty())
    condWait();

  // simply return the first task in the ready queue
  DynamicTaskPtr task = readyQueue.front();
  readyQueue.pop_front();
  mutexUnlock();
#ifdef VERBOSE  
  printf("thread %d: remove task %p from ready queue\n", 
	 pthread_self(), task.asPtr());
#endif
  return task;
}


void DynamicScheduler::dequeueReadyQueue(DynamicTaskPtr task)
{
  // remove the task from ready queue
  mutexLock();
  readyQueue.remove(task);
  mutexUnlock();
}



/*
 * Warning, now do not consider the data race
 * assume proper lock is applied when called
 */
void DynamicScheduler::enqueueWaitQueue(DynamicTaskPtr task)
{
  // add task into wait queue
#ifdef VERBOSE  
  printf("thread %d: add task %p into wait queue\n", 
	 pthread_self(), task.asPtr());
#endif
  mutexLock();
  waitQueue.push_back(task);
  mutexUnlock();
}


/*
 * Warning, now do not consider the data race
 * assume proper lock is applied when called
 */
void DynamicScheduler::dequeueWaitQueue(DynamicTaskPtr task)
{
  mutexLock();
  // remove task from wait queue
  waitQueue.remove(task);
  mutexUnlock();
#ifdef VERBOSE  
  printf("thread %d, remove task %p from wait queue\n", 
	 pthread_self(), task.asPtr());
#endif
}


void DynamicScheduler::moveWait2Ready(DynamicTaskPtr task)
{
  dequeueWaitQueue(task);
  enqueueReadyQueue(task);
}

void DynamicScheduler::cleanWaitQueue() 
{
  std::list<DynamicTaskPtr>::iterator it;
  DynamicTaskPtr task;
  mutexLock();
  for(it = waitQueue.begin(); it != waitQueue.end(); it++ ) {
    task = *it;
    if(!task) {
      dequeueWaitQueue(task);
      continue;
    }
    if(task->isReady()) {
      dequeueWaitQueue(task);
      enqueueReadyQueue(task);
    }
  }
  mutexUnlock();
}


void *workerStartup(void *args) 
{
  DynamicScheduler *scheduler = (DynamicScheduler *)args;

  // infinit loop to for executing tasks
  while(true) {
    DynamicTaskPtr task = NULL;
    
    // get a task for execution
    task = scheduler->dequeueReadyQueueBlocking();

    // execute the task
    DynamicTaskPtr cont = task->run();
    task->completeTask();
    // remove the dependence for the task
    if(!cont) {
      // no continuation task, so remove the dependence
      task->removeDependence();
    } else {
      // assuming all dependent will depend on cond too
      task->copyDependence(cont);
      scheduler->enqueueNewTask(cont);
    } 
  }
}


}


