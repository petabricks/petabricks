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
#include "dynamictask.h"
 
//#define VERBOSE

namespace hecura {

DynamicScheduler *DynamicTask::scheduler = NULL;

DynamicTask::DynamicTask()
{
  complete         = false;
  numOfPredecessor = 0;

  // allocate scheduler when the first task is created
  if(scheduler == NULL) {
    scheduler = new DynamicScheduler();
    scheduler->startWorkerThreads();
  }
}


void DynamicTask::enqueue()
{
  // put new task into scheduler's work queue
  scheduler->mutexLock();
  scheduler->enqueueNewTask(DynamicTaskPtr(this));
  scheduler->mutexUnlock();
}


void DynamicTask::dependsOn(const DynamicTaskPtr &that)
{
  // increase the numOfPredecessor as I rely on one more task
  // add this into that's dependent list
  // I am updating someone else's dependent list
  // could be more than one updating it, so need lock
  if(!that)
    return;
  that->dependentMutex.lock();
  if(!that->complete){
    jalib::atomicAdd<1>(&numOfPredecessor);
    that->dependents.push_back(DynamicTaskPtr(this));
#ifdef VERBOSE
    printf("task %p depends on task %p counter: %d\n", 
	   this, that.asPtr(), numOfPredecessor);
#endif  
  }
  that->dependentMutex.unlock();
}


bool DynamicTask::isReady()
{
  if(numOfPredecessor == 0)
    return true;
  return false;
}


void DynamicTask::removeDependence()
{
  std::vector<DynamicTaskPtr>::iterator it;
  dependentMutex.lock();
#ifdef VERBOSE
  printf("task %p finish, decrease dependents counter\n", this);
#endif  
  for(it = dependents.begin(); it != dependents.end(); ++it) {
    DynamicTaskPtr task = *it;
    // dec the counter
    jalib::atomicAdd<-1>(&task->numOfPredecessor);
#ifdef VERBOSE
    printf("\ttask %p: %d\n", task.asPtr(), task->numOfPredecessor);
#endif    
    // if no predecessor, move task from wait queue to ready queue
    if(task->numOfPredecessor == 0) {
      scheduler->mutexLock();
      scheduler->dequeueWaitQueue(task);
      scheduler->enqueueReadyQueue(task);
      scheduler->mutexUnlock();
    }
  }
  dependents.clear();
  dependentMutex.unlock();
}


void DynamicTask::copyDependence(DynamicTaskPtr dst)
{
  std::vector<DynamicTaskPtr>::iterator it;
  // assuming no one else will touch dst, so no lock for dst task's dependent
  dependentMutex.lock();
  for(it = dependents.begin(); it != dependents.end(); ++it) {
    DynamicTaskPtr task = *it;
    dst->dependents.push_back(task);
  }
  dependents.clear();
  dependentMutex.unlock();
}


void DynamicTask::completeTask()
{
  complete = true;
}


void DynamicTask::waitUntilComplete()
{
#ifdef VERBOSE
  printf("\ttask %p wait until complete\n", this);
#endif   
  while(!this->complete) {
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
