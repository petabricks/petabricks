/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#ifndef PETABRICKSWORKERTHREAD_H
#define PETABRICKSWORKERTHREAD_H

#include "dynamictask.h"
#include "thedeque.h"

#include <set>
#include <pthread.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

class WorkerThreadPool;
class DynamicScheduler;

class WorkerThread {
public:
  static WorkerThread* self();

  WorkerThread(DynamicScheduler& ds);
  ~WorkerThread();

  DynamicTask *steal(){
    return _deque.pop_bottom();
  }
  
  void push(DynamicTask* t){
    return _deque.push_top(t);
  }

  int threadRandInt() const;

  void popAndRunOneTask(int stealLimit);
  void mainLoop();

  int id() const { return _id; }

  bool hasWork() const { return !_deque.empty(); }
  int workCount() const { return (int)_deque.size(); }

  WorkerThreadPool& pool() { return _pool; }
  const WorkerThreadPool& pool() const { return _pool; }
private:
  int _id;
  THEDeque<DynamicTask*> _deque;
  mutable struct { int z; int w; } _randomNumState;
  WorkerThreadPool& _pool;
  pthread_t _thread;
} __attribute__ ((aligned (64)));

/**
 * A pool of worker threads
 * This data structure becomes inefficient when/after removals occur
 */
class WorkerThreadPool {
public:
  //
  // constructor 
  WorkerThreadPool();

  //
  // insert a new thread into the pool in a lock-free way
  void insert(WorkerThread* thread);
  
  //
  // remove a thread from the pool in a lock-free way
  void remove(WorkerThread* thread);

  //
  // pick a random thread from the pool
  WorkerThread* getRandom(const WorkerThread* caller);
  
  //
  // pick a thread from the pool, using i as a hint of the location
  WorkerThread* getFixed(int i=0);

private:
  WorkerThread* _pool[MAX_NUM_WORKERS];
  int _count;
};


/**
 * A task that aborts all threads in the pool
 */
class AbortTask : public DynamicTask {
public:
  AbortTask(int totalThreads, bool shouldExit = false);
  DynamicTaskPtr run();
private:
  jalib::AtomicT    _numLive;
  jalib::AtomicT    _numAborting;
  jalib::JCondMutex _lock;
  bool _shutdown;
};

}

#endif
