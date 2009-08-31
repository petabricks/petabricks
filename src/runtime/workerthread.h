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

#include <pthread.h>
#include <set>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

class DynamicScheduler;
class WorkerThreadPool;

class WorkerThread {
public:
  static WorkerThread* self();

  WorkerThread(DynamicScheduler& ds);
  ~WorkerThread();

  ///
  /// called from a remote thread, taking work
  DynamicTask* steal(){
    DynamicTask* t = NULL;
#ifdef WORKERTHREAD_INJECT
    if(!_injectQueue.empty())
      t=_injectQueue.pop_bottom();
#endif
    if(t==NULL)
      t = _deque.pop_bottom();
    return t;
  }
  
  ///
  /// called from a remote thread, giving work
  void inject(DynamicTask* t){
#ifdef WORKERTHREAD_INJECT
    _injectQueue.push_bottom(t);
#else
    JASSERT(false).Text("support for WorkerThread::inject() not compiled in");
#endif
  }
  
  ///
  /// called on WorkerThread::self(), taking work
  DynamicTask* popLocal(){
    DynamicTask* t = NULL;
#ifdef WORKERTHREAD_ONDECK
    if(_ondeck != NULL){
      t = _ondeck;
      _ondeck=NULL;
      return t;
    }
#endif
    t = _deque.pop_top();
#ifdef WORKERTHREAD_INJECT
    if(t==NULL && !_injectQueue.empty())
      t=_injectQueue.pop_top();
#endif
    return t;
  }
  
  ///
  /// called on WorkerThread::self(), giving work
  void pushLocal(DynamicTask* t){
#ifdef WORKERTHREAD_ONDECK
    if(_ondeck == NULL){
      _ondeck = t;
      return;
    }
#endif
    _deque.push_top(t);
  }
  
  ///
  /// Racy count of the number of items of work left
  int workCount() const {
    return (int)_deque.size();
  }
  
  ///
  /// thread local random number generator
  int threadRandInt() const;

  ///
  /// A single iteration of the main loop
  void popAndRunOneTask(int stealLimit);

  ///
  /// Main loop for worker threads
  void mainLoop();

  
  bool hasWork() const { return workCount()>0; }
  int id() const { return _id; }
  WorkerThreadPool& pool() { return _pool; }
  const WorkerThreadPool& pool() const { return _pool; }
private:
  int _id;
#ifdef WORKERTHREAD_ONDECK
  DynamicTask* _ondeck; // a special queue for the *first* task pushed (improves locality)
#endif
  THEDeque<DynamicTask*> _deque;
#ifdef WORKERTHREAD_INJECT
  LockingDeque<DynamicTask*> _injectQueue;
#endif
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
  WorkerThread* getRandom(const WorkerThread* caller = WorkerThread::self());
  
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
