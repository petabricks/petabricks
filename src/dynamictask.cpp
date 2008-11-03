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

//#define PBCC_SEQUENTIAL

namespace hecura {

DynamicScheduler *DynamicTask::scheduler = NULL;

DynamicTask::DynamicTask()
{
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
  if(preds==0)
    scheduler->enqueue(this);
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
    numOfPredecessor++;
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
  JLOCKSCOPE(lock);
  numOfPredecessor--;
  if(numOfPredecessor==0 && state==S_PENDING){
    state=S_READY;
    DynamicTaskPtr self = this;
    scheduler->enqueue(self);
  }
}


void hecura::DynamicTask::runWrapper(){
  JASSERT(state==S_READY && numOfPredecessor==0)(state)(numOfPredecessor);
  JTRACE("running task");
  continuation = run();

  std::vector<DynamicTaskPtr>::iterator it;
  std::vector<DynamicTaskPtr> tmp;

  lock.lock();
  dependents.swap(tmp);
  if(continuation) state = S_CONTINUED;
  else             state = S_COMPLETE;
  lock.unlock();

  if(continuation){
    {
      JLOCKSCOPE(continuation->lock);
      for(it = tmp.begin(); it != tmp.end(); ++it) {
        continuation->dependents.push_back(*it);
      }
    }
    continuation->enqueue();
  }else{
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
      usleep(100);
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
