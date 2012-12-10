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
#ifndef PETABRICKSGPUMANAGER_H
#define PETABRICKSGPUMANAGER_H

#include "common/jmutex.h"
#include "common/openclutil.h"

#include "gpudynamictask.h"

#include <pthread.h>
#include <list>
#include <queue>
#include <map>


namespace petabricks {

class GpuManager /*: public jalib::JRefCounted*/  {
public:
  ///
  /// start gpu manager thread
  static void start();

  ///
  /// shut down gpu manager thread
  static void shutdown();

  ///
  /// gpu manager's main loop
  static void mainLoop();

  ///
  /// enqueue a task
  static void addTask(GpuDynamicTaskPtr task);

  ///
  /// a pointer to the current task infomation gpu manager is running
  static GpuTaskInfoPtr _currenttaskinfo;

  ///
  /// an opencl kernel that gpu manager is currently using
  static cl_kernel _kernel;

  ///
  /// an opencl command queue that gpu manager is currently using
  static cl_command_queue _queue;

private:
  /** Class is a singleton. **/
  GpuManager() {}

  ///
  /// run PREPARE task
  static void prepare(GpuDynamicTaskPtr task);

  ///
  /// run COPYIN task
  static void copyin(GpuDynamicTaskPtr task);

  ///
  /// run RUN task
  static void run(GpuDynamicTaskPtr task);

  ///
  /// run COPYOUT task
  static bool copyout(GpuDynamicTaskPtr task);

  ///
  /// mark if gpu manager is shut down
  static bool _shutdown;

  ///
  /// a thread that gpu manager is running on
  static pthread_t _thread;

  ///
  /// task queue
  static std::queue<GpuDynamicTaskPtr> _readytasks;

  ///
  /// lock for push and pop queue
  static jalib::JMutex  _lock;

  ///
  /// an opencl context that gpu manager is currently using
  static cl_context _context;

};

}

#endif

