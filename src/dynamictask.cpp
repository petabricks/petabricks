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
 
#define VERBOSE
//#define PBCC_SEQUENTIAL

namespace hecura {

DynamicScheduler *DynamicTask::scheduler = NULL;

DynamicTask::DynamicTask()
{
  complete         = false;
  numOfPredecessor = 0;

#ifndef PBCC_SEQUENTIAL
  // allocate scheduler when the first task is created
  if(scheduler == NULL) {
    scheduler = new DynamicScheduler();
    scheduler->startWorkerThreads();
  }
#endif
}


#ifdef PBCC_SEQUENTIAL
void DynamicTask::enqueue() { run();}
#else
void DynamicTask::enqueue()
{
  // put new task into scheduler's work queue
  scheduler->enqueueNewTask(DynamicTaskPtr(this));
}
#endif // PBCC_SEQUENTIAL


#ifdef PBCC_SEQUENTIAL
void DynamicTask::dependsOn(const DynamicTaskPtr &that){}
#else
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
    printf("thread %d: task %p depends on task %p counter: %d\n", 
	   pthread_self(), this, that.asPtr(), numOfPredecessor);
#endif  
  }
  that->dependentMutex.unlock();
}
#endif // PBCC_SEQUENTIAL


bool DynamicTask::isReady()
{
  if(numOfPredecessor == 0)
    return true;
  return false;
}


void DynamicTask::removeDependence()
{
#ifdef VERBOSE
  printf("thread %d: task %p finish, decrease dependents counter\n", 
	 pthread_self(), this);
#endif  

  std::vector<DynamicTaskPtr>::iterator it;
  std::vector<DynamicTaskPtr> tmp;

  dependentMutex.lock();
  JASSERT(complete);
  dependents.swap(tmp);
  dependentMutex.unlock();

  for(it = tmp.begin(); it != tmp.end(); ++it) {
    DynamicTaskPtr task = *it;
#ifdef VERBOSE
    printf("\ttask %p: %d\n", task.asPtr(), task->numOfPredecessor);
#endif    
    // dec the counter
    if(jalib::atomicAdd<-1>(&task->numOfPredecessor)==0){
      // if no predecessor, move task from wait queue to ready queue
      scheduler->moveWait2Ready(task);
    }
  }
}


void DynamicTask::copyDependence(DynamicTaskPtr dst)
{
  std::vector<DynamicTaskPtr>::iterator it;
  std::vector<DynamicTaskPtr> tmp;
  // assuming no one else will touch dst, so no lock for dst task's dependent
  dependentMutex.lock();
  dependents.swap(tmp);
  dependentMutex.unlock();

  dst->dependentMutex.lock();
  for(it = tmp.begin(); it != tmp.end(); ++it) {
    DynamicTaskPtr task = *it;
    dst->dependents.push_back(task);
  }
  dst->dependentMutex.unlock();
}


void DynamicTask::completeTask()
{
  dependentMutex.lock();
  complete = true;
  dependentMutex.unlock();
}


#ifdef PBCC_SEQUENTIAL
void DynamicTask::waitUntilComplete() {}
#else
void DynamicTask::waitUntilComplete()
{
#ifdef VERBOSE
  printf("thread %d: task %p wait until complete\n", 
	 pthread_self(), this);
#endif   
  while(!this->complete) {
    // get a task for execution
    DynamicTaskPtr task = scheduler->dequeueReadyQueueNonblocking();
    if (!task) {
      //scheduler->cleanWaitQueue();
      usleep(100);
    } else {
      DynamicTaskPtr cont = task->run();
      task->completeTask();
      // remove the dependence for the task
      if(!cont) {
	// no continuation task, so remove the dependence
	task->removeDependence();
      } else {
	// assuming all dependent will depend on cont too
	task->copyDependence(cont);
	scheduler->enqueueNewTask(cont);
      }
    }
  }
}
#endif // PBCC_SEQUENTIAL

}
