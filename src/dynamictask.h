/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
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
#ifndef HECURADYNAMICTASK_H
#define HECURADYNAMICTASK_H

#include "jrefcounted.h"
#include "jmutex.h"
#include "dynamicscheduler.h"

#include <vector>


namespace hecura {

// forward declarsion for DynamicTaskPtr
class DynamicTask;
typedef jalib::JRef<DynamicTask> DynamicTaskPtr;


class DynamicTask : public jalib::JRefCounted {
public:
  ///
  /// Perform this task, return a continuation task that must be completed
  /// before this task is marked done, otherwise return NULL
  virtual DynamicTaskPtr run() = 0;

  /// constructor
  DynamicTask();

  ///
  /// Mark that this may not start before that
  void dependsOn(const DynamicTaskPtr& that);

  ///
  /// Enqueue this task to be run (once all dependencies are met)
  /// this method indicates that all dependencies have been registered
  void enqueue();

  ///
  /// decrease the dependent task's count 
  void removeDependence();

  ///
  /// copy the dependent tasks into dst task
  void copyDependence(DynamicTaskPtr dst);

  ///
  /// check if the task is ready to run
  bool isReady();

  ///
  /// Size in bytes the area of this task
  virtual size_t size() const  { return 0; }

  ///
  /// True if this can be split into smaller independant taks
  virtual bool canSplit() const { return false; }

  ///
  /// Split this task into smaller tasks and return a replacement
  virtual DynamicTaskPtr split(){ JASSERT(false); return 0; }


  ///
  /// Block until this task has completed
  void waitUntilComplete();

  ///
  /// set task as complete
  void completeTask();


  void lock(){ dependentMutex.lock(); }
  void unlock(){ dependentMutex.unlock(); }

 protected:
  ///
  /// a list of tasks that depends on me
  mutable std::vector<DynamicTaskPtr> dependents;

  ///
  /// a mutex lock for accessing dependent
  mutable jalib::JMutex  dependentMutex;

  ///
  /// a counter of how many tasks I depends on
  volatile long numOfPredecessor;

  /// 
  /// Scheduler for scheduling the dynamic tasks
  static DynamicScheduler   *scheduler;

  /// 
  /// indicate if the task is executed or not
  bool complete;
};


class NullDynamicTask : public DynamicTask {
public:
  DynamicTaskPtr run(){ return 0; }
};

}

#endif
