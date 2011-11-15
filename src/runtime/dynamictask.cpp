/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/
#include "petabricksruntime.h"

#include "dynamicscheduler.h"
#include "dynamictask.h"
#include "workerthread.h"

#include "common/jasm.h"
#include "common/jtunable.h"

#include <pthread.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

DynamicTask::DynamicTask(TaskType t)
 :_continuation(NULL), _state(S_NEW), _numPredecessors(0), _type(t)
{}


#ifdef PBCC_SEQUENTIAL
void DynamicTask::enqueue() { run();}
#else
void DynamicTask::enqueue()
{
  incRefCount(); // matches with runWrapper()
  int preds;
  {
    JLOCKSCOPE(_lock);
    preds=_numPredecessors;
    if(_state==S_NEW) {
      if(preds==0)
        _state=S_READY;
      else
        _state=S_PENDING;
    }else if(_state==S_REMOTE_NEW){
      if(preds==0)
        _state=S_REMOTE_READY;
      else
        _state=S_REMOTE_PENDING;
    }else{
      JASSERT(false)(_state);
    }
  }
  if(preds==0) {
    if(_state == S_READY){
      inlineOrEnqueueTask();
    }else if(_state == S_REMOTE_READY){
      remoteScheduleTask();
    }else{
      JASSERT(false)(_state);
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
  JASSERT(_state==S_NEW || _state==S_REMOTE_NEW)(_state).Text(".dependsOn must be called before enqueue()");
  that->_lock.lock();
  if(that->_state == S_CONTINUED){
    that->_lock.unlock();
    dependsOn(that->_continuation);
  }else if(that->_state != S_COMPLETE){
    that->_dependents.push_back(this);
#ifdef DEBUG
    JASSERT(that->_dependents.back()!=NULL);
#endif
    {
      JLOCKSCOPE(_lock);
      _numPredecessors++;
    }
    that->_lock.unlock();
  }else{
    that->_lock.unlock();
  }
#ifdef VERBOSE
    printf("thread %d: task %p depends on task %p counter: %d\n", pthread_self(), this, that.asPtr(), _numPredecessors);
#endif
}
#endif // PBCC_SEQUENTIAL

void petabricks::DynamicTask::decrementPredecessors(bool isAborting){
  bool shouldEnqueue = false;
  {
    JLOCKSCOPE(_lock);
    --_numPredecessors;
    if(_numPredecessors==0 && _state==S_PENDING){
      _state = S_READY;
      shouldEnqueue = true;
    }
    if(_numPredecessors==0 && _state==S_REMOTE_PENDING){
      _state = S_REMOTE_READY;
      shouldEnqueue = true;
    }
  }
  if (shouldEnqueue) {
    if (isAborting) {
      runWrapper(true);
    } else if (_state==S_READY) {
      inlineOrEnqueueTask();
    } else if (_state==S_REMOTE_READY) {
      remoteScheduleTask();
    } else {
      JASSERT(false);
    }
  }
}

void petabricks::DynamicTask::runWrapper(bool isAborting){
  JASSERT(((_state==S_READY && _type==TYPE_CPU) || (_state==S_REMOTE_READY && _type==TYPE_OPENCL)) && _numPredecessors==0)(_state)(_numPredecessors);

  if (!isAborting) {
#ifdef DISTRIBUTED_CACHE
    if(!isNullTask()) {
      WorkerThread::self()->cache()->invalidate();
    }
#endif
    _continuation = run();
  } else {
    _continuation = NULL;
  }

  completeTaskDeps(isAborting);
}

void petabricks::DynamicTask::completeTaskDeps(bool isAborting){
  std::vector<DynamicTask*> tmp;

  {
    JLOCKSCOPE(_lock);
    _dependents.swap(tmp);
    if(_continuation) _state = S_CONTINUED;
    else              _state = S_COMPLETE;
  }

  if(_continuation){
#ifdef VERBOSE
    JTRACE("task complete, continued")(tmp.size());
#endif
    {
      JLOCKSCOPE(_continuation->_lock);
      if(_continuation->_dependents.empty()){
        //swap is faster than insert
        _continuation->_dependents.swap(tmp);
      }else if(!tmp.empty()){
        _continuation->_dependents.insert(_continuation->_dependents.end(), tmp.begin(), tmp.end());
      }
    }
    _continuation->enqueue();
  }else{
    #ifdef VERBOSE
    if(!isNullTask()) JTRACE("task complete")(tmp.size());
    #endif
    std::vector<DynamicTask*>::iterator it;
    for(it = tmp.begin(); it != tmp.end(); ++it) {
#ifdef DEBUG
      JASSERT(*it != 0)(tmp.size());
#endif
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
  WorkerThread* self = WorkerThread::self();
  JASSERT(self!=NULL);
  _lock.lock();
  while(_state != S_COMPLETE && _state!= S_CONTINUED) {
    _lock.unlock();
    self->popAndRunOneTask(STEAL_ATTEMPTS_WAITING);
    _lock.lock();
  }
  _lock.unlock();
  if(_state == S_CONTINUED)
    _continuation->waitUntilComplete();
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
    WorkerThread* self = WorkerThread::self();
    if(self!=NULL) {
      self->pushLocal(this);
    }else{
      DynamicScheduler::cpuScheduler().injectWork(this);
    }
  }
}

}
