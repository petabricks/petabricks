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
#include <signal.h>
#include <jasm.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif


static jalib::AtomicT theTidGen = 0;

#ifdef HAVE_THREADLOCAL
int petabricks::tid() {
  static __thread int _tid = -1;
  if (_tid == -1) {
    _tid = (int)jalib::atomicIncrementReturn(&theTidGen)-1;
  }
  return _tid;
}
#else
//this is a backwards compatible version for archs that dont support __thread
namespace { //file local
  static pthread_key_t  tid_key;
  static pthread_once_t tid_key_once = PTHREAD_ONCE_INIT;
  static void tid_destroy(int* buf){ delete buf; }
  static void tid_key_alloc(){  pthread_key_create(&tid_key, (void (*)(void*)) tid_destroy); }
  static void tid_alloc(){
    pthread_once(&tid_key_once, tid_key_alloc);
    pthread_setspecific(tid_key, new int);
  }
}
int petabricks::tid()
{
  int* t = (int*)pthread_getspecific(tid_key);
  if(t==NULL){
    tid_alloc();
    t = (int*)pthread_getspecific(tid_key);
    JASSERT(t!=NULL);
    *t = (int)jalib::atomicIncrementReturn(&theTidGen)-1;
  }
  return *t;
}
#endif

// #define VERBOSE


petabricks::DynamicScheduler& petabricks::DynamicScheduler::instance(){
  static DynamicScheduler t;
  return t;
}

namespace petabricks {

static void sigAlarmHandler(int signal) {
  printf("  TIMED OUT!\n");
  fflush(stdout);
  DynamicScheduler::instance().setAbortFlag();
}

#ifdef GRACEFUL_ABORT
bool DynamicScheduler::theIsAborting=false;
bool DynamicScheduler::theIsTerminating=false;
jalib::JCondMutex DynamicScheduler::theAbortingLock;
#endif

extern "C" {
void *workerStartup(void *);
}


DynamicScheduler::DynamicScheduler()
{
  // Register abort handler
  signal(SIGALRM, sigAlarmHandler);

  numOfWorkers  = 1; //main thread counts as a worker
  workerThreads = new pthread_t[MAX_NUM_WORKERS];
#ifdef GRACEFUL_ABORT
  numAbortedThreads = 0;
#endif
}


DynamicScheduler::~DynamicScheduler()
{
}


void DynamicScheduler::startWorkerThreads(int total)
{
  JASSERT(numOfWorkers <= total)(numOfWorkers)(total);
  while(numOfWorkers < total)
    JASSERT(pthread_create(&workerThreads[numOfWorkers++ - 1], NULL, workerStartup, (void *)this) == 0);
}
  
void DynamicScheduler::popAndRunOneTask(bool blocking)
{
#ifdef GRACEFUL_ABORT
    try {
#endif
      DynamicTask *task;

      if (blocking) {
        task = dequeue();
      } else {
        task = tryDequeue();
      }

      if (task != NULL) {
        task->runWrapper();
      }

#ifdef GRACEFUL_ABORT
    } catch (DynamicScheduler::AbortException e) {
      if (blocking)
        abortWait();
      else
        throw;
    }
#endif
}

void *workerStartup(void *args)
{
  DynamicScheduler *scheduler = (DynamicScheduler *)args;

  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

  // infinite loop to for executing tasks
  try {
    while (true) {
      scheduler->popAndRunOneTask(true);
    }
  } catch(...) {
    //JTRACE("Thread aborted");
    throw;
  }
}

#ifdef GRACEFUL_ABORT

void DynamicScheduler::setAbortFlag() {
  JLOCKSCOPE(theAbortingLock);
  if(!theIsAborting){
    theIsAborting=true;
  }
}

void DynamicScheduler::resetAbortFlag() {
  JLOCKSCOPE(theAbortingLock);
  theIsAborting=false;
}

void DynamicScheduler::abortBegin() {
  setAbortFlag();
  throw AbortException();
}
void DynamicScheduler::abortEnd() {
  JLOCKSCOPE(theAbortingLock);
  while(numAbortedThreads != numOfWorkers - 1) {
    theAbortingLock.wait();
  }
  theIsAborting=false;

  for (int i = 0; i < numOfWorkers; i++) {
    taskStacks[i].clear();
    JASSERT(taskStacks[i].isEmpty());
  }

  theAbortingLock.broadcast();
}
void DynamicScheduler::abortWait() {
  JLOCKSCOPE(theAbortingLock);
  numAbortedThreads++;
  theAbortingLock.broadcast();
  while(isAborting()) theAbortingLock.wait();
  numAbortedThreads--;
  if(isTerminating()) pthread_exit(NULL);
}
void DynamicScheduler::shutdown() {
  theIsTerminating = true;
  setAbortFlag();
  abortEnd();
  for(int i=0; i<numOfWorkers-1; ++i)
    JASSERT(pthread_join(workerThreads[i], NULL) == 0)(i);
  numOfWorkers=1;
}
#endif

}


