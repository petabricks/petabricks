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
#ifndef PETABRICKSSPECIALIZEDDYNAMICTASK_H
#define PETABRICKSSPECIALIZEDDYNAMICTASK_H

#include "dynamictask.h"
#include "matrixregion.h"
#include "string.h"
#include "gputaskinfo.h"

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

typedef uint32_t DimMaskT;

/**
 * A dynamic task that does nothing
 * For building dependency chains
 */
class NullDynamicTask : public DynamicTask {
public:
  DynamicTaskPtr run(){ return 0; }
  bool isNullTask() const { return true; }
  //PADDING(64);
};


/**
 * A task that calls a method on a given object
 */
template< typename T, DynamicTaskPtr (T::*method)()>
class MethodCallTask : public DynamicTask {
public:
  MethodCallTask(const jalib::JRef<T>& obj)
    : _obj(obj)
  {}
  DynamicTaskPtr run(){
    return ((*_obj).*(method))();
  }
private:
  jalib::JRef<T> _obj;
  //PADDING(64);
};

/**
 * A task that calls a method on a given object, with a given region
 */
template< typename T, int D, DynamicTaskPtr (T::*method)(IndexT begin[D], IndexT end[D])>
class SpatialMethodCallTask : public DynamicTask {
public:
  SpatialMethodCallTask(const jalib::JRef<T>& obj, IndexT begin[D], IndexT end[D])
    : _obj(obj)
  {
    memcpy(_begin, begin, sizeof _begin);
    memcpy(_end,   end,   sizeof _end);
  }
  DynamicTaskPtr run(){
    return ((*_obj).*(method))(_begin, _end);
  }
private:
  jalib::JRef<T> _obj;
  IndexT _begin[D];
  IndexT _end[D];
};

/**
 * A task that calls a method on a given object, with a given region, task ID, a pointer to RegionNodeGroup map, and a boolean indicating the copy out status
 */
template< typename T, int D, DynamicTaskPtr (T::*method)(IndexT begin[D], IndexT end[D], int nodeID, RegionNodeGroupMapPtr map, int gpuCopyOut)>
class CreateGpuSpatialMethodCallTask : public DynamicTask {
public:
  CreateGpuSpatialMethodCallTask(const jalib::JRef<T>& obj, IndexT begin[D], IndexT end[D], int nodeID, RegionNodeGroupMapPtr map, int gpuCopyOut)
    : _obj(obj), _nodeID(nodeID), _map(map), _gpuCopyOut(gpuCopyOut)
  {
    memcpy(_begin, begin, sizeof _begin);
    memcpy(_end,   end,   sizeof _end);
  }
  DynamicTaskPtr run(){
    return ((*_obj).*(method))(_begin, _end, _nodeID, _map, _gpuCopyOut);
  }
private:
  jalib::JRef<T> _obj;
  IndexT _begin[D];
  IndexT _end[D];
  int _nodeID;
  RegionNodeGroupMapPtr _map;
  int _gpuCopyOut;
};

/**
 * A task that bundles many unstarted tasks together
 */
template< int N >
class GroupedDynamicTask : public DynamicTask {
public:
  GroupedDynamicTask() : _i(0), _all(new NullDynamicTask()) {}

  DynamicTaskPtr run(){
    if(_i < N){
      _parts[_i++]->enqueue();
      return new MethodCallTask<GroupedDynamicTask, &GroupedDynamicTask::run>(this);
    }
    if(_i == N){
      //make sure all tasks have completed
      for(int i=0; i<N; ++i)
        _all->dependsOn(_parts[i]);
      return _all;
    }
    return NULL;
  }

  DynamicTaskPtr& operator[] ( size_t i ) { return _parts[i]; }
private:
  IndexT _i;
  DynamicTaskPtr _all;
  DynamicTaskPtr _parts[N];
};



}

#endif

