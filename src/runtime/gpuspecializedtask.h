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
#ifndef PETABRICKSGPUSPECIALIZEDTASK_H
#define PETABRICKSGPUSPECIALIZEDTASK_H

#include "dynamictask.h"
#include "gpudynamictask.h"
#include "matrixregion.h"
#include "string.h"

#include <vector>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif

namespace petabricks {

/**
 * A gpu task that calls a method on a given object, with a given region
 */
template< typename T, int D, DynamicTaskPtr (T::*method)(IndexT begin[D], IndexT end[D])>
class GpuSpatialMethodCallTask : public GpuDynamicTask {
public:
  GpuSpatialMethodCallTask(const jalib::JRef<T>& obj, IndexT begin[D], IndexT end[D], GpuTaskInfoPtr taskinfo, GpuTaskType tasktype, MatrixStorageInfoPtr info = NULL)
    : GpuDynamicTask(taskinfo,tasktype,info), _obj(obj)
  {
    memcpy(_begin, begin, sizeof _begin);
    memcpy(_end,   end,   sizeof _end);
  }

  DynamicTaskPtr run(){
    JASSERT(_state == S_REMOTE_READY)(_state);
    return ((*_obj).*(method))(_begin, _end);
  }

private:
  jalib::JRef<T> _obj;
  IndexT _begin[D];
  IndexT _end[D];
};

/**
 * A gpu task that calls a method on a given object, with a given list of regions amd node ID
 */
template< typename T, int D, DynamicTaskPtr (T::*method)(std::vector<IndexT*>& begins, std::vector<IndexT*>& ends, int nodeID)>
class GpuCopyOutMethodCallTask : public GpuDynamicTask {
public:
  GpuCopyOutMethodCallTask(const jalib::JRef<T>& obj, GpuTaskInfoPtr taskinfo, MatrixStorageInfoPtr info)
    : GpuDynamicTask(taskinfo,COPYOUT,info), _obj(obj)
  {
    _nodeID = -1;
  }
  DynamicTaskPtr run(){
    JASSERT(_nodeID != -1)(_nodeID).Text("Forget to set node ID before running.");
    return ((*_obj).*(method))(_begins, _ends, _nodeID);
  }
  void setRegions(std::vector<IndexT*>& begins, std::vector<IndexT*>& ends, int nodeID){
    _begins = begins;
    _ends = ends;
    _nodeID = nodeID;
  }
private:
  jalib::JRef<T> _obj;
  std::vector<IndexT*> _begins;
  std::vector<IndexT*> _ends;
  int _nodeID;
};

}

#endif

