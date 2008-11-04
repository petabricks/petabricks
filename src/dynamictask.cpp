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
#include "jasm.h"
#include <pthread.h>

//#define PBCC_SEQUENTIAL
#define INLINE_NULL_TASKS

namespace hecura {

DynamicScheduler *DynamicTask::scheduler = NULL;

DynamicTask::DynamicTask()
{
  // when this task is created, no other thread would touch it
  // so no lock for numOfPredecessor update
  state = S_NEW;
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
  int preds;
  {
    JLOCKSCOPE(lock);
    preds=numOfPredecessor;
    if(preds==0)
      state=S_READY;
    else
      state=S_PENDING;
  }
  if(preds==0){
#ifdef INLINE_NULL_TASKS
    if(isNullTask()){
      runWrapper(); //dont bother enqueuing null tasks
    }else
#endif
    {
      scheduler->enqueue(this);
    }
  }
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
    jalib::atomicAdd<1> (&numOfPredecessor);
    that->lock.unlock();
  }else{
    that->lock.unlock();
  }
#ifdef VERBOSE
    printf("thread %d: task %p depends on task %p counter: %d\n", pthread_self(), this, that.asPtr(), numOfPredecessor);
#endif  
}
#endif // PBCC_SEQUENTIAL

void hecura::DynamicTask::decrementPredecessors(){
  bool shouldEnqueue = false;
  {
    JLOCKSCOPE(lock);
    if((jalib::atomicAdd<-1> (&numOfPredecessor))==0 && state==S_PENDING){
      state = S_READY;
      shouldEnqueue = true;
    }
  }
  if(shouldEnqueue){
#ifdef INLINE_NULL_TASKS
    if(isNullTask()){
      runWrapper(); //dont bother enqueuing null tasks
    }else
#endif
    {
      scheduler->enqueue(this);
    }
  }
}


void hecura::DynamicTask::runWrapper(){
  JASSERT(state==S_READY && numOfPredecessor==0)(state)(numOfPredecessor);
  continuation = run();

  std::vector<DynamicTaskPtr> tmp;

  {
    JLOCKSCOPE(lock);
    dependents.swap(tmp);
    if(continuation) state = S_CONTINUED;
    else             state = S_COMPLETE;
  }

  if(continuation){
    JTRACE("task complete, continued")(tmp.size());
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
    #ifdef DEBUG
    if(!isNullTask()) JTRACE("task complete")(tmp.size());
    #endif
    std::vector<DynamicTaskPtr>::iterator it;
    for(it = tmp.begin(); it != tmp.end(); ++it) {
      (*it)->decrementPredecessors();
    }
  }
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
    DynamicTaskPtr task = scheduler->tryDequeue();
    if (!task) {
      pthread_yield();
    } else {
      task->runWrapper();
    }
    lock.lock();
  }
  lock.unlock();
  if(state == S_CONTINUED)
    continuation->waitUntilComplete();
}
#endif // PBCC_SEQUENTIAL

}
