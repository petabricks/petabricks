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
#include "dynamicscheduler.h"
#include "gpumanager.h"

#include <pthread.h>
#include <signal.h>
#include <unistd.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

petabricks::DynamicScheduler& petabricks::DynamicScheduler::cpuScheduler(){
  static DynamicScheduler t;
  if(t._rawThreadsLen==0) {
    // add the main thread
    t._rawThreads[0] = pthread_self();
    t._rawThreadsLen=1;
  }
  return t;
}
petabricks::DynamicScheduler& petabricks::DynamicScheduler::lookupScheduler(DynamicTask::TaskType t){
  static DynamicScheduler extraSchedulers[DynamicTask::TYPE_COUNT-1];
  //TODO: once we get more than 2 or 3 task types we should convert this to a table
  switch(t){
    case DynamicTask::TYPE_CPU:    return cpuScheduler();  
    case DynamicTask::TYPE_OPENCL: return extraSchedulers[0];  
    default:
    {
      UNIMPLEMENTED();
      return *(DynamicScheduler*)NULL;
    }
  }
}

extern "C" void *workerStartup(void *arg) {
  try {
    JASSERT(arg!=0);
    petabricks::WorkerThread worker(*(petabricks::DynamicScheduler*)arg);
    worker.mainLoop();
  }catch(petabricks::DynamicScheduler::CleanExitException e){}
  return NULL;
}

void petabricks::DynamicScheduler::startWorkerThreads(int total)
{
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, 0);
  JASSERT(numThreads() <= total)(numThreads())(total);
  while(numThreads() < total){
    JASSERT(pthread_create(_rawThreads+_rawThreadsLen, &attr, workerStartup, this) == 0);
    _rawThreadsLen++;
    JASSERT(_rawThreadsLen < MAX_NUM_WORKERS);
  }
  pthread_attr_destroy(&attr);
}

void petabricks::DynamicScheduler::abort(){
  static jalib::JMutex lock;
  if(lock.trylock()){
    DynamicTaskPtr t = new AbortTask(numThreads(), false);
    try{
      t->run();
    }catch(...){
      lock.unlock();
      throw;
    }
  }else{
    //another thread beat us to the abort()
    WorkerThread* self = WorkerThread::self();
    JASSERT(self!=NULL);
    //cancel all our pending tasks
    DynamicTask* t;
    while((t=self->popLocal())!=NULL)
      t->cancel();
    //wait until the other thread aborts us
    for(;;) self->popAndRunOneTask(STEAL_ATTEMPTS_MAINLOOP);
  }
  JASSERT(false).Text("unreachable");
}

void petabricks::DynamicScheduler::shutdown(){
  if(numThreads()==1)
    return;
  try {
    DynamicTaskPtr t = new AbortTask(numThreads(), true);
    t->run();
  }catch(AbortException e){}

  for(int i=1; i<_rawThreadsLen; ++i) {
    int rv = pthread_join(_rawThreads[i], NULL);
    JWARNING(rv==0)(rv).Text("pthread_join failed");
  }
  if(pthread_equal(_rawThreads[0], pthread_self())){
    _rawThreadsLen=1;
  }else{
    int rv = pthread_join(_rawThreads[0], NULL);
    JWARNING(rv==0)(rv).Text("pthread_join failed");
    _rawThreadsLen=0;
  }
}
  
void petabricks::DynamicScheduler::injectWork(DynamicTask* task){
  static jalib::AtomicT i=0;
  pool().getFixed((int)jalib::atomicIncrementReturn(&i))->inject(task);

  if(i > (1<<28)) {
    i=0;
  }
}




