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

#include "jtunable.h"

#include <pthread.h>
#include <unistd.h>
 
// #define VERBOSE


#define MIN_NUM_WORKERS  0
#define MAX_NUM_WORKERS  512

JTUNABLE(tunerNumOfWorkers, 16, MIN_NUM_WORKERS, MAX_NUM_WORKERS);

namespace hecura {

extern "C" {
void *workerStartup(void *);
}


DynamicScheduler::DynamicScheduler()
{
  numOfWorkers = tunerNumOfWorkers;
#ifdef VERBOSE
  printf("thread %d, total number of workers %d\n", 
	 pthread_self(), numOfWorkers);
#endif
}


DynamicScheduler::~DynamicScheduler()
{
  // wait for all worker to exit
  //for(unsigned int i = 0; i < numOfWorkers; i++) {
  //  pthread_join(workerThreads[i], NULL);
  //}
}


void DynamicScheduler::startWorkerThreads()  
{
  // allocat and spawn a certain number of thread
  workerThreads   = new pthread_t[numOfWorkers];
  for(unsigned int i = 0; i < numOfWorkers; i++) {
    JASSERT(pthread_create(&workerThreads[i], NULL, workerStartup, (void *)this) == 0);
  }
}


DynamicTaskPtr DynamicScheduler::dequeueReadyQueueNonblocking()
{
  mutexLock();
  // remove task from the ready queue
  if(readyQueue.empty()){
    mutexUnlock();
    return NULL;
  }

  // simply return the first task in the ready queue
  DynamicTaskPtr task = readyQueue.front();
  readyQueue.pop_front();
  mutexUnlock();

#ifdef VERBOSE  
  printf("thread %d: remove task %p from ready queue\n", 
	 pthread_self(), task.asPtr());
#endif

  return task;
}

DynamicTaskPtr DynamicScheduler::dequeueReadyQueueBlocking()
{
  mutexLock();
  while(readyQueue.empty())
    condWait();

  // simply return the first task in the ready queue
  DynamicTaskPtr task = readyQueue.front();
  readyQueue.pop_front();
  mutexUnlock();
#ifdef VERBOSE  
  printf("thread %d: remove task %p from ready queue\n", 
	 pthread_self(), task.asPtr());
#endif
  return task;
}

void *workerStartup(void *args) 
{
  DynamicScheduler *scheduler = (DynamicScheduler *)args;

  // infinit loop to for executing tasks
  while(true) {
    scheduler->dequeueReadyQueueBlocking()->runWrapper();
  }
}


}


