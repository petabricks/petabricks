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
#include "workerthread.h"

#include "dynamicscheduler.h"
#include "common/jasm.h"

#include <pthread.h>
#include <signal.h>
#include <unistd.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef DEBUG
#  define DEBUGONLY(args...) args
#else
#  define DEBUGONLY(args...) (void)0
#endif


//singleton main thread
static petabricks::WorkerThread theMainWorkerThread(petabricks::DynamicScheduler::cpuScheduler());

#ifdef HAVE_THREADLOCAL
//this is the simple/fast version
__thread petabricks::WorkerThread* mySelfPtr = NULL;
petabricks::WorkerThread* petabricks::WorkerThread::self(){
  return mySelfPtr;
}
void setSelf(petabricks::WorkerThread* v){
  mySelfPtr = v;
}
#else
//this is a backwards compatible version for archs that dont support __thread
namespace { //file local
  static pthread_key_t  worker_key;
  static pthread_once_t worker_key_once = PTHREAD_ONCE_INIT;
  static void worker_destroy(int* buf){ delete buf; }
  static void worker_key_alloc(){  pthread_key_create(&worker_key, (void (*)(void*)) worker_destroy); }
  static void worker_alloc(){
    pthread_once(&worker_key_once, worker_key_alloc);
    pthread_setspecific(worker_key, new petabricks::WorkerThread*);
  }
}
petabricks::WorkerThread* petabricks::WorkerThread::self(){
  petabricks::WorkerThread** t = (petabricks::WorkerThread**)pthread_getspecific(worker_key);
  return t!=NULL ? *t : NULL;
}
void setSelf(petabricks::WorkerThread* v){
  petabricks::WorkerThread** t = (petabricks::WorkerThread**)pthread_getspecific(worker_key);
  if(t==NULL){
    worker_alloc();
    t = (petabricks::WorkerThread**)pthread_getspecific(worker_key);
    JASSERT(t!=NULL);
  }
  *t = v;
}
#endif

petabricks::WorkerThread::WorkerThread(DynamicScheduler& ds)
  : _pool(ds.pool())
{
  static jalib::AtomicT lastId = -1;
  _id = (int)jalib::atomicIncrementReturn(&lastId);
  _randomNumState.z = _id * _id * 2;
  _randomNumState.w = _id + 1;
  setSelf(this);
  _pool.insert(this);
#ifdef WORKERTHREAD_ONDECK
  _ondeck = NULL;
#endif
#ifdef DISTRIBUTED_CACHE
  _cache = new WorkerThreadCache();
#endif
  DEBUGONLY(_isWorking=false);
}
petabricks::WorkerThread::~WorkerThread(){
  _pool.remove(this);
}


void petabricks::WorkerThread::markUtilityThread() {
  setSelf(NULL);
}

int petabricks::WorkerThread::threadRandInt() const {
  _randomNumState.z = 36969 * (_randomNumState.z & 65535) + (_randomNumState.z >> 16);
  _randomNumState.w = 18000 * (_randomNumState.w & 65535) + (_randomNumState.w >> 16);
  int retVal = (_randomNumState.z << 16) + _randomNumState.w;
  if (retVal < 0) {
    retVal = -retVal;
  }
  return retVal;
}

void petabricks::WorkerThread::popAndRunOneTask(int stealLimit)
{
  DynamicTask *task;

  //try from the local deque
  task = popLocal();

  //try stealing a bunch of times
  while(task == NULL && stealLimit-->0){
    WorkerThread* victim = _pool.getRandom(this);
    if (victim != NULL) {
      task = victim->steal();
    }
  }

  //if we got something, run it
  if (task != NULL) {
    DEBUGONLY(_isWorking=true);
    task->runWrapper();
    DEBUGONLY(_isWorking=false);
  } else {
    sched_yield();
  }
}

void petabricks::WorkerThread::mainLoop(){
  for(;;){
    try {
      for(;;) popAndRunOneTask(STEAL_ATTEMPTS_MAINLOOP);
    }catch(DynamicScheduler::AbortException e){}
  }
}

petabricks::WorkerThreadPool::WorkerThreadPool(){
  memset(_pool, 0, sizeof _pool);
}

void petabricks::WorkerThreadPool::insert(WorkerThread* thread){
  int i = _count; // the slot we wish to insert into
  for(;;++i){
    JASSERT(i<MAX_NUM_WORKERS)(i)(MAX_NUM_WORKERS).Text("Too many workers!");
    if(jalib::compareAndSwap<WorkerThread*>(_pool+i, NULL, thread))
      break;
    // the slot wasn't null when we tried to put it in... someone else took our slot
    // try again on the next slot
  }
  //ensure _count>i
  while(! jalib::compareAndSwap(&_count, _count, std::max(_count, i+1)) ) ;
}

void petabricks::WorkerThreadPool::remove(WorkerThread* thread){
  for(int i=0; i<_count; ++i){
    if(_pool[i] == thread)
      _pool[i] = NULL;//dont need a CAS here
  }
  // reduce _count if possible
  for(int t; _pool[t=_count-1] == NULL; ){
    jalib::compareAndSwap(&_count, t+1, t);//tryDecrement
  }
}

petabricks::WorkerThread* petabricks::WorkerThreadPool::getRandom(const WorkerThread* caller){
#ifdef DEBUG
  JASSERT(caller!=NULL);
#endif
  WorkerThread* rv;
  do {
    rv=_pool[caller->threadRandInt()%_count];
    if(_count==1)
      return NULL;
    jalib::staticMemFence();//don't optimize _count
  } while(rv==caller || rv == NULL);
  return rv;
}

petabricks::WorkerThread* petabricks::WorkerThreadPool::getFixed(int i/*=0*/){
  WorkerThread* rv;
  do {
    rv=_pool[i++%_count];
  } while(rv == NULL);
  return rv;
}

petabricks::AbortTask::AbortTask(int totalThreads, bool shouldExit) {
  _numLive = totalThreads;
  _numAborting = totalThreads;
  _shutdown = shouldExit;
  _state = S_READY;
}
petabricks::DynamicTaskPtr petabricks::AbortTask::run(){
  DynamicTaskPtr abortTask = this; // ensure this task isn't deleted
  WorkerThread* self = WorkerThread::self();
  JASSERT(self!=NULL);
  WorkerThreadPool& pool = self->pool();
  DynamicTask* t=NULL;

  jalib::atomicDecrement(&_numLive);

  //cancel all our pending tasks
  while((t=self->popLocal())!=NULL){
    t->cancel();
  }

  //until all threads are aborted, steal and cancel tasks
  while(_numLive>0){
    jalib::staticMemFence();
    //make sure there is something to be stolen from us, 5 and 15 are arbitrary
    if(self->workCount()<5)
      for(int i=0; i<15; ++i)
        self->pushLocal(this);
    //pick a victim to steal work from
    WorkerThread* victim = pool.getRandom(self);
    if(victim!=NULL){
      //try to steal all of the victim's work
      for(int c=victim->workCount(); c>0; --c){
        t = victim->steal();
        if(t!=NULL && t!=this) t->cancel();
        else break;
      }
    }
  }

  //drain off all our abort tasks
  while((t=self->popLocal())!=NULL){
    JASSERT(t==this);
  }

  //wait until all threads reach this point
  jalib::atomicDecrement(&_numAborting);
  while(_numAborting>0) {
    jalib::staticMemFence();
    sched_yield();
  }
 //_lock.lock();
 //if(--_numAborting==0)
 //  _lock.broadcast();
 //else
 //  _lock.wait();
 //_lock.unlock();

  //either exit or throw
  if(_shutdown && self!=&theMainWorkerThread){
    //pthread_exit(0);
    throw DynamicScheduler::CleanExitException();
  }else{
    throw DynamicScheduler::AbortException();
  }
}

void petabricks::WorkerThreadPool::debugPrint() const {
  std::cerr << "thread status: " << std::endl;

  for(int i=0; i<_count; ++i){
    const WorkerThread* t = _pool[i];
    if(t == 0){
      std::cerr << "  * empty slot " << std::endl;
    }else{
      std::cerr << "  * thread " << t->id() << " workCount:" << t->workCount();
      DEBUGONLY(std::cerr << " isWorking:" << t->isWorking());
      if(t==WorkerThread::self())
        std::cerr << " (current)";
      std::cerr << std::endl;
    }
  }
}

void petabricks::WorkerThreadPool::debugPrint(jalib::JAssert& ) const {
#if 0
  if(!o.IsFatal())
    return;
  o.Prefix();
  o << "WorkerThread status:";
  o.EndLine();

  for(int i=0; i<_count; ++i){
    const WorkerThread* t = _pool[i];
    o.Prefix();
    if(t == 0){
      o << "  - empty slot";
    }else{
      o << "  - thread " << t->id() << " workCount:" << t->workCount();
      DEBUGONLY(o << " isWorking:" << t->isWorking());
      if(t==WorkerThread::self())
        o << " (current)";
    }
    o.EndLine();
  }
  o.Prefix();
  o.EndLine();
#endif
}


extern "C" int threadstatus() {
  if(petabricks::WorkerThread::self()!=0) {
    petabricks::WorkerThread::self()->pool().debugPrint();
    return petabricks::DynamicScheduler::cpuScheduler().numThreads();
  }else{
    return 0;
  }
}

namespace{
  void onJassert(jalib::JAssert& o) {
    if(petabricks::WorkerThread::self()!=0) {
      petabricks::WorkerThread::self()->pool().debugPrint(o);
    }
  }
}
//int _ignored = jalib::JAssert::onBegin(&onJassert);


