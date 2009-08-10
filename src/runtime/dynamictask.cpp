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
#include "petabricksruntime.h"
#include "dynamicscheduler.h"
#include "dynamictask.h"

#include "common/jasm.h"
#include "common/jtunable.h"

#include <pthread.h>

//#define PBCC_SEQUENTIAL
#define INLINE_NULL_TASKS

#define MIN_INLINE_TASK_SIZE  1
#define MAX_INLINE_TASK_SIZE  65536


namespace petabricks {

size_t            DynamicTask::firstSize = 0;
size_t            DynamicTask::maxSize   = 0;

DynamicTask::DynamicTask(bool isCont)
{
  // when this task is created, no other thread would touch it
  // so no lock for numOfPredecessor update
  state = S_NEW;
  numOfPredecessor = 0;
  isContinuation = isCont;
  continuation = NULL;

}


#ifdef PBCC_SEQUENTIAL
void DynamicTask::enqueue() { run();}
#else
void DynamicTask::enqueue()
{
  incRefCount(); // matches with runWrapper()
  int preds;
  {
    JLOCKSCOPE(lock);
    preds=numOfPredecessor;
    if(preds==0)
      state=S_READY;
    else
      state=S_PENDING;
  }
  if(preds==0) { // || (isContinuation && !isNullTask())) {
    inlineOrEnqueueTask();
  }
  //inlineOrEnqueueTask();
}
#endif // PBCC_SEQUENTIAL


#ifdef PBCC_SEQUENTIAL
void DynamicTask::dependsOn(const DynamicTaskPtr &that){}
#else
void DynamicTask::dependsOn(const DynamicTaskPtr &that)
{
  if(!that) return;
  JASSERT(that!=this).Text("task cant depend on itself");
  JASSERT(state==S_NEW)(state).Text(".dependsOn must be called before enqueue()");
  that->lock.lock();
  if(that->state == S_CONTINUED){
    that->lock.unlock();
    dependsOn(that->continuation);
  }else if(that->state != S_COMPLETE){
    that->dependents.push_back(this);
    {
      JLOCKSCOPE(lock);
      numOfPredecessor++;
    }
    that->lock.unlock();
  }else{
    that->lock.unlock();
  }
#ifdef VERBOSE
    printf("thread %d: task %p depends on task %p counter: %d\n", pthread_self(), this, that.asPtr(), numOfPredecessor);
#endif
}
#endif // PBCC_SEQUENTIAL

void petabricks::DynamicTask::decrementPredecessors(bool isAborting){
  bool shouldEnqueue = false;
  {
    JLOCKSCOPE(lock);
    if(--numOfPredecessor==0 && state==S_PENDING){
      state = S_READY;
      shouldEnqueue = true;
    }
  }
  if (shouldEnqueue) {
    if (isAborting) {
      runWrapper(true);
    } else {
      inlineOrEnqueueTask();
    }
  }
}


void petabricks::DynamicTask::runWrapper(bool isAborting){
  JASSERT(state==S_READY && numOfPredecessor==0)(state)(numOfPredecessor);

  if (!isAborting) {
    continuation = run();
  } else {
    continuation = NULL;
  }

  std::vector<DynamicTask*> tmp;

  {
    JLOCKSCOPE(lock);
    dependents.swap(tmp);
    if(continuation) state = S_CONTINUED;
    else             state = S_COMPLETE;
  }

  if(continuation){
    continuation->isContinuation = true;
#ifdef VERBOSE
    JTRACE("task complete, continued")(tmp.size());
#endif
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
    #ifdef VERBOSE
    if(!isNullTask()) JTRACE("task complete")(tmp.size());
    #endif
    std::vector<DynamicTask*>::iterator it;
    for(it = tmp.begin(); it != tmp.end(); ++it) {
      (*it)->decrementPredecessors(isAborting);
    }
  }
  decRefCount(); //matches with enqueue();
}


#ifdef PBCC_SEQUENTIAL
void DynamicTask::waitUntilComplete() {}
#else
void DynamicTask::waitUntilComplete()
{
  lock.lock();
  while(state != S_COMPLETE && state!= S_CONTINUED) {
    lock.unlock();
    // get a task for execution
    DynamicScheduler::instance().popAndRunOneTask(false);
    lock.lock();
  }
  lock.unlock();
  if(state == S_CONTINUED)
    continuation->waitUntilComplete();
}
#endif // PBCC_SEQUENTIAL

void DynamicTask::inlineOrEnqueueTask()
{
#ifdef INLINE_NULL_TASKS
  if(isNullTask())
    runWrapper(); //dont bother enqueuing just run it
  else
#endif
  {
    DynamicScheduler::instance().enqueue(this);
  }
}

}
