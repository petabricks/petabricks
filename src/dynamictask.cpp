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
#include "dynamicscheduler.h"
#include "jasm.h"
#include "jtunable.h"

#include <pthread.h>

//#define PBCC_SEQUENTIAL
#define INLINE_NULL_TASKS

#define MIN_INLINE_TASK_SIZE  1
#define MAX_INLINE_TASK_SIZE  65536

JTUNABLE(tunerNumOfWorkers,   8, MIN_NUM_WORKERS, MAX_NUM_WORKERS);

namespace petabricks {

DynamicScheduler* DynamicTask::scheduler = NULL;
size_t            DynamicTask::firstSize = 0;
size_t            DynamicTask::maxSize   = 0;

DynamicTask::DynamicTask(bool isCont)
{
  // when this task is created, no other thread would touch it
  // so no lock for numOfPredecessor update
  state = S_NEW;
  isContinuation = isCont;

  // allocate scheduler when the first task is created
  if(scheduler == NULL) {
    scheduler = new DynamicScheduler();
    scheduler->startWorkerThreads(tunerNumOfWorkers);
  }
}

void DynamicTask::enqueue()
{
  incRefCount(); // matches with runWrapper()
  decrementPredecessors(); //remove one pred before enqueue
}

void DynamicTask::dependsOn(const DynamicTaskPtr &that)
{
  if(!that) return;
#ifdef DEBUG
  JASSERT(that!=this).Text("task cant depend on itself");
#endif

  bool isDepMet = true;
  for(;;){
    if(that->state == S_COMPLETE){
      break;
    }else if(that->state == S_CONTINUED){
      dependsOn(that->continuation);
      break;
    }else{
      JLOCKSCOPE(that->lock);
      if(that->state >= S_READY){ //test again now that we have lock
        isDepMet = false;
        that->dependents.push_back(this);
        break;
      }
    }
  }
  if(!isDepMet) jalib::atomicIncrement(&state);
}

void petabricks::DynamicTask::decrementPredecessors(){
  if(jalib::atomicDecrementReturn(&state)==S_READY){
    inlineOrEnqueueTask();
  }
}


void petabricks::DynamicTask::runWrapper(){
#ifdef DEBUG
  JASSERT(state==S_READY)(state);
#endif
  continuation = run();

  std::vector<DynamicTask*> tmp;

  {
    JLOCKSCOPE(lock);
    if(continuation) state = S_CONTINUED;
    else             state = S_COMPLETE;
    dependents.swap(tmp);
  }

  if(continuation){
    continuation->isContinuation = true;
    {
      JLOCKSCOPE(continuation->lock);
      if(continuation->dependents.empty()){
        //swap is faster than insert
        continuation->dependents.swap(tmp);
      }else{
        continuation->dependents.insert(continuation->dependents.end(), tmp.begin(), tmp.end());
      }
    }
    continuation->enqueue();
  }else{
    std::vector<DynamicTask*>::iterator it;
    for(it = tmp.begin(); it != tmp.end(); ++it) {
      (*it)->decrementPredecessors();
    }
  }
  decRefCount(); //matches with enqueue();
}


void DynamicTask::waitUntilComplete()
{
  lock.lock();
  while(state != S_COMPLETE && state!= S_CONTINUED) {
    lock.unlock();
    // get a task for execution
    scheduler->popAndRunOneTask(false);
    lock.lock();
  }
  lock.unlock();
  if(state == S_CONTINUED)
    continuation->waitUntilComplete();
}

void DynamicTask::inlineOrEnqueueTask()
{
#ifdef INLINE_NULL_TASKS
  if(isNullTask())
    runWrapper(); //dont bother enqueuing just run it
  else
#endif
  {
    JASSERT(this->state == S_READY);
    scheduler->enqueue(this);
  }
}

}
