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

#include <pthread.h>
 
//#define VERBOSE

namespace hecura {

extern "C" {
void *workerStartup(void *);
}


DynamicScheduler::DynamicScheduler()
{
  numOfWorkers = 8;
}


DynamicScheduler::~DynamicScheduler()
{
  // wait for all worker to exit
  for(unsigned int i = 0; i < numOfWorkers; i++) {
    pthread_join(workerThreads[i], NULL);
  }
}


void DynamicScheduler::startWorkerThreads()  
{
  // allocat and spawn a certain number of thread
  workerThreads   = new pthread_t[numOfWorkers];
  for(unsigned int i = 0; i < numOfWorkers; i++) {
    JASSERT(pthread_create(&workerThreads[i], NULL, workerStartup, (void *)this) == 0);
  }
}


/*
 * Warning, now do not consider the data race
 * assume proper lock is applied when called
 */

void DynamicScheduler::enqueueNewTask(DynamicTaskPtr task)
{
  // first check if there is any dependent,
  // if no add task into the ready queue
  // else  add task into the  wait queue
  if(task->isReady())
    enqueueReadyQueue(task);
  else
    enqueueWaitQueue(task);
}


void DynamicScheduler::enqueueReadyQueue(DynamicTaskPtr task)
{
  // append task to end of the ready queue
#ifdef VERBOSE  
  printf("add task %p into ready queue\n", task.asPtr());
#endif
  readyQueue.push_back(task);
  // signal if there is any worker is waiting
  condMutexSignal();
}


DynamicTaskPtr DynamicScheduler::dequeueReadyQueue()
{
  // remove task from the ready queue
  if(readyQueue.empty())
    return NULL;

  // simply return the first task in the ready queue
  DynamicTaskPtr task = readyQueue.front();
  readyQueue.pop_front();
#ifdef VERBOSE  
  printf("remove task %p from ready queue\n", task.asPtr());
#endif
  return task;
}


void DynamicScheduler::dequeueReadyQueue(DynamicTaskPtr task)
{
  // remove the task from ready queue
  readyQueue.remove(task);
}


void DynamicScheduler::enqueueWaitQueue(DynamicTaskPtr task)
{
  // add task into wait queue
#ifdef VERBOSE  
  printf("add task %p into wait queue\n", task.asPtr());
#endif
  waitQueue.push_back(task);
}


void DynamicScheduler::dequeueWaitQueue(DynamicTaskPtr task)
{
  // remove task from wait queue
  waitQueue.remove(task);
#ifdef VERBOSE  
  printf("remove task %p from wait queue\n", task.asPtr());
#endif
}


void *workerStartup(void *args) 
{
  DynamicScheduler *scheduler = (DynamicScheduler *)args;

  // infinit loop to for executing tasks
  while(true) {
    DynamicTaskPtr task = NULL;
    //
    // To get a task for execution
    //
    // lock 
    scheduler->condMutexLock();
    // if empty, cond wait on the ready queue.
    scheduler->mutexLock();
    task = scheduler->dequeueReadyQueue();
    scheduler->mutexUnlock();
    while (!task) {
      scheduler->condMutexWait();
      scheduler->mutexLock();
      task = scheduler->dequeueReadyQueue();
      scheduler->mutexUnlock();
    }
    scheduler->condMutexUnlock();

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


