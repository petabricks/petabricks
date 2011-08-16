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
#ifdef HAVE_OPENCL
#ifndef PETABRICKSGPUMANAGER_H
#define PETABRICKSGPUMANAGER_H

#include "common/jmutex.h"

#include "openclutil.h"
#include "gpudynamictask.h"
#include "deviceinfo.h"
#include "matrixregion.h"

#include <oclUtils.h>
#include <pthread.h>
#include <list>
#include <queue>
#include <map>


namespace petabricks {

class GpuManager : public jalib::JRefCounted  {
public:

  static void start();
  static void shutdown();
  static void mainLoop();
  static void addTask(GpuDynamicTaskPtr task);
  static void dummy() {}

  static GpuTaskInfoPtr _currenttaskinfo;
  static cl_kernel _kernel;
  static cl_command_queue _queue;

private:
  /** Class is a singleton. */
  GpuManager() {}

  static void prepare(GpuDynamicTaskPtr task);
  static void copyin(GpuDynamicTaskPtr task);
  static void run(GpuDynamicTaskPtr task);
  static bool copyout(GpuDynamicTaskPtr task);

  static bool _shutdown;
  static pthread_t _thread;
  static std::queue<GpuDynamicTaskPtr> _readytasks;
  static jalib::JMutex  _lock;

  static cl_context _context;

};

}

#endif
#endif
