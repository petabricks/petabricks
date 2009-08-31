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
#include "dynamicscheduler.h"

#include <pthread.h>
#include <signal.h>
#include <unistd.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

petabricks::DynamicScheduler& petabricks::DynamicScheduler::cpuScheduler(){
  static DynamicScheduler t;
  if(t._rawThreads.empty()){
    // add the main thread
    t._rawThreads.push_back(pthread_self());
  }
  return t;
}
petabricks::DynamicScheduler& petabricks::DynamicScheduler::lookupScheduler(DynamicTask::TaskType t){
  static DynamicScheduler extraSchedulers[DynamicTask::TYPE_COUNT-1];
  //TODO: once we get more than 2 or 3 task types we should convert this to a table
  switch(t){
    case DynamicTask::TYPE_CPU:    return cpuScheduler();  
    case DynamicTask::TYPE_OPENCL: return extraSchedulers[0];  
    default: UNIMPLEMENTED();
  }
}

extern "C" void *workerStartup(void *arg) {
  JASSERT(arg!=0);
  petabricks::WorkerThread worker(*(petabricks::DynamicScheduler*)arg);
  worker.mainLoop();
}

void petabricks::DynamicScheduler::startWorkerThreads(int total)
{
  JASSERT(numThreads() <= total)(numThreads())(total);
  while(numThreads() < total){
    pthread_t tmp;
    _rawThreads.push_back(tmp);
    JASSERT(pthread_create(&_rawThreads.back(), NULL, workerStartup, this) == 0);
  }
}

void petabricks::DynamicScheduler::abort(){
  static DynamicTaskPtr t;
  static jalib::JMutex lock;
  if(lock.trylock()){
    t = new AbortTask(numThreads(), false);
    try{
      t->run();
    }catch(...){
      lock.unlock();
      throw;
    }
    JASSERT(false);
  }else{
    //another thread beat us to the abort()
    WorkerThread* self = WorkerThread::self();
    //cancel all our pending tasks
    while(self->hasWork()){
      DynamicTask* t = self->steal();
      if(t!=NULL) t->cancel();
    }
    //wait until the other thread aborts us
    for(;;) self->popAndRunOneTask(STEAL_ATTEMPTS_MAINLOOP);
  }
}

void petabricks::DynamicScheduler::shutdown(){
  try {
    static DynamicTaskPtr t;
    t = new AbortTask(numThreads(), true);
    t->run();
  }catch(AbortException e){}
  pthread_t self = pthread_self();
  while(!_rawThreads.empty()){
    if(_rawThreads.front()!=self){
      JASSERT(pthread_join(_rawThreads.front(), 0)==0);
    }
    _rawThreads.pop_front();
  }
  _rawThreads.push_back(self);
}
  

