/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
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

//#define PBCC_SEQUENTIAL
#define INLINE_NULL_TASKS

#define MIN_INLINE_TASK_SIZE  1
#define MAX_INLINE_TASK_SIZE  65536


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
    if(preds==0)
      _state=S_READY;
    else
      _state=S_PENDING;
  }
  if(preds==0) {
    inlineOrEnqueueTask();
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
  JASSERT(_state==S_NEW)(_state).Text(".dependsOn must be called before enqueue()");
  that->_lock.lock();
  if(that->_state == S_CONTINUED){
    that->_lock.unlock();
    dependsOn(that->_continuation);
  }else if(that->_state != S_COMPLETE){
    that->_dependents.push_back(this);
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
    if(--_numPredecessors==0 && _state==S_PENDING){
      _state = S_READY;
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
  JASSERT(_state==S_READY && _numPredecessors==0)(_state)(_numPredecessors);

  if (!isAborting) {
    _continuation = run();
  } else {
    _continuation = NULL;
  }

  std::vector<DynamicTask*> tmp;

  {
    JLOCKSCOPE(_lock);
    _dependents.swap(tmp);
    if(_continuation) _state = S_CONTINUED;
    else             _state = S_COMPLETE;
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
      }else{
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
#ifdef DEBUG
    JASSERT(self!=NULL);
#endif
    self->pushLocal(this);
  }
}

}
