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
#include "dynamictask.h"

#include <pthread.h>
#include <unistd.h>
 
// #define VERBOSE


#define MIN_NUM_WORKERS  0
#define MAX_NUM_WORKERS  512


namespace hecura {

#ifdef GRACEFUL_ABORT
bool DynamicScheduler::theIsAborting=false;
jalib::JCondMutex DynamicScheduler::theAbortingLock;
#endif

extern "C" {
void *workerStartup(void *);
}


DynamicScheduler::DynamicScheduler()
{
  numOfWorkers  = 0;
  workerThreads = new pthread_t[MAX_NUM_WORKERS];
#ifdef GRACEFUL_ABORT
  numAbortedThreads = 0;
#endif
}


DynamicScheduler::~DynamicScheduler()
{
  // nothing to do so far
}


void DynamicScheduler::startWorkerThreads(int newWorkers)  
{
  // allocat and spawn a certain number of thread
  for(int i = 0; i < newWorkers; i++) {
    JASSERT(pthread_create(&workerThreads[numOfWorkers + i], NULL, workerStartup, (void *)this) == 0);
  }
  numOfWorkers += newWorkers;
  JTRACE("start worker threads")(numOfWorkers);
}

__thread std::list<DynamicTaskPtr>* q = 0;
std::list<DynamicTaskPtr>& DynamicScheduler::myThreadLocalQueue(){
  if(q==0) q = new std::list<DynamicTaskPtr>();
  return *q;
}



void DynamicScheduler::popAndRunOneTask(bool blocking){
  std::list<DynamicTaskPtr>& myQ = DynamicScheduler::myThreadLocalQueue();

#ifdef GRACEFUL_ABORT
    try{
#endif
      DynamicTaskPtr task;

      if(!myQ.empty()){
        //try a thread local task
        task = myQ.front();
        myQ.pop_front();
      }else{
        //otherwise a global task
        if(blocking)
          task = dequeue();
        else{
          task = tryDequeue();
        }
      }

#ifdef GRACEFUL_ABORT
      if(DynamicScheduler::isAborting())
        throw DynamicScheduler::AbortException();
#endif

      if(task) task->runWrapper();

#ifdef GRACEFUL_ABORT
    }catch(DynamicScheduler::AbortException e){
      if(blocking)
        abortWait();
      else
        throw;
    }
#endif
}

void *workerStartup(void *args) 
{
  DynamicScheduler *scheduler = (DynamicScheduler *)args;

  // infinit loop to for executing tasks
  while(true) {
    scheduler->popAndRunOneTask(true);
  }
}

#ifdef GRACEFUL_ABORT
void DynamicScheduler::abortBegin() {
  JLOCKSCOPE(theAbortingLock);
  if(!theIsAborting){
    theIsAborting=true; 
    queue.clear();
    for(int i=0; i<numOfWorkers+1; ++i)
      queue.push(0);
//    JTRACE("Aborting!")(numOfWorkers);
  }
  throw AbortException();
}
void DynamicScheduler::abortEnd() {
  JLOCKSCOPE(theAbortingLock);
  while(numAbortedThreads != numOfWorkers) theAbortingLock.wait();
  theIsAborting=false;
  queue.clear();
  theAbortingLock.broadcast();
}
void DynamicScheduler::abortWait() {
  JLOCKSCOPE(theAbortingLock);
  numAbortedThreads++;
  theAbortingLock.broadcast();
  while(isAborting()) theAbortingLock.wait();
  numAbortedThreads--;
}
#endif

}


