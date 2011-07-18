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

#ifndef PETABRICKSGPUDYNAMICTASK_H
#define PETABRICKSGPUDYNAMICTASK_H

#include "gputaskinfo.h"

namespace petabricks {

class GpuDynamicTask;
typedef jalib::JRef<GpuDynamicTask> GpuDynamicTaskPtr;
typedef std::vector<GpuDynamicTaskPtr> GpuDynamicTaskList;

class GpuDynamicTask : public DynamicTask {
public:

  /// constructor
  GpuDynamicTask() : DynamicTask(TYPE_OPENCL) {
  }

  virtual DynamicTaskPtr run() = 0;

  void runWrapper(bool isAborting){
    JASSERT(_state==S_READY && _numPredecessors==0)(_state)(_numPredecessors);

    if (!isAborting) {
      _continuation = run();
    } else {
      _continuation = NULL;
    }

    std::vector<DynamicTask*> tmp;

    {
      JLOCKSCOPE(_lock);
      _dependents.swap(tmp);
      if(_continuation) _state = S_CONTINUED;
      else             _state = S_COMPLETE;
    }

    if(_continuation){
      // Unimplemented yet
    }else{
      #ifdef VERBOSE
      if(!isNullTask()) JTRACE("task complete")(tmp.size());
      #endif
      std::vector<DynamicTask*>::iterator it;
      for(it = tmp.begin(); it != tmp.end(); ++it) {
        #ifdef DEBUG
        JASSERT(*it != 0)(tmp.size());
        #endif
        (*it)->decrementPredecessors(isAborting);
      }
    }
    decRefCount(); //matches with enqueue();
  }

  int id(){ 
    return _id; 
  }

  enum GpuTaskType {
    PRERUN,
    RUN,
    POSTRUN,
  };

protected:
  GpuTaskInfoPtr _task;
  int _id;
};

}

#endif
#endif
