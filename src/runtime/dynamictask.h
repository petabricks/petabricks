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
#ifndef PETABRICKSDYNAMICTASK_H
#define PETABRICKSDYNAMICTASK_H

#include "common/jmutex.h"
#include "common/jrefcounted.h"

#include <algorithm>
#include <vector>

namespace petabricks {

class DynamicTask;
typedef jalib::JRef<DynamicTask> DynamicTaskPtr;


class DynamicTask : public jalib::JRefCounted {
public:
  enum TaskType {
    TYPE_CPU    = 1<<0,
    TYPE_OPENCL = 1<<1,

    TYPE_ANY    = TYPE_CPU | TYPE_OPENCL,
    TYPE_COUNT  = 2
  };

  ///
  /// Perform this task, return a continuation task that must be completed
  /// before this task is marked done, otherwise return NULL
  virtual DynamicTaskPtr run() = 0;

  /// constructor
  DynamicTask(TaskType t = TYPE_CPU);

  ///
  /// Mark that this may not start before that
  void dependsOn(const DynamicTaskPtr& that);

  ///
  /// Enqueue this task to be run (once all dependencies are met)
  /// this method indicates that all dependencies have been registered
  void enqueue();

  ///
  /// Size in bytes the area of this task
  virtual size_t size() const  { return 0; }

  ///
  /// Block until this task has completed
  void waitUntilComplete();

  ///
  /// Wrapper around run that changes state and handles dependencies
  void runWrapper(bool isAborting = false);

  ///
  /// Pretend this task was run successfully, recursively cancel tasks that depend on this
  void cancel(){ runWrapper(true); }

  ///
  /// add a type of processor this can run on
  void addType(TaskType t) { _type |= t; }
  
  ///
  /// remove a type of processor this can run on
  void removeType(TaskType t) { _type &= ~t; }

  ///
  /// test if this can run on a given processor type
  bool hasType(TaskType t) const { return (_type&t)!=0; }
 protected:
  ///
  /// either enqueue or inline the task
  void inlineOrEnqueueTask();

  virtual bool isNullTask() const { return false; }

  ///
  /// mark that a task that we dependOn has completed
  void decrementPredecessors(bool isAborting = false);

  ///
  /// a list of tasks that depends on me
  std::vector<DynamicTask*> _dependents;

  ///
  /// a mutex lock for manipulating task status and dependents list
  jalib::JMutex  _lock;

  ///
  /// Pointer to the continuation task
  DynamicTaskPtr _continuation;

  enum TaskState {
    S_NEW,       //after creation
    S_PENDING,   //after enqueue()
    S_READY,     //after all dependencies met
    S_COMPLETE,  //after run()==NULL
    S_CONTINUED  //after run()!=NULL
  };

  ///
  /// indicate if the task is executed or not
  TaskState _state;

  ///
  /// a counter of how many incomplete tasks I still depends on
  long _numPredecessors;
  
  ///
  /// cpu types this task can run on
  int _type;
};

}

#endif
