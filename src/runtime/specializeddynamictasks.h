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
#include "remotetask.h"

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

template< typename T, int D, DynamicTaskPtr (T::*method)(IndexT begin[D], IndexT end[D]), void (T::*getDataHostsMethod)(DataHostPidList& list, IndexT begin[D], IndexT end[D])>
class SpatialMethodCallTask_distributed : public RemoteTask {
public:
  SpatialMethodCallTask_distributed(const jalib::JRef<T>& obj, IndexT begin[D], IndexT end[D])
    : _obj(obj)
  {
    // JTRACE("spatial distributed");
    memcpy(_begin, begin, sizeof _begin);
    memcpy(_end,   end,   sizeof _end);
  }
  SpatialMethodCallTask_distributed(const char* _buf, RemoteHost& _host){
    unserialize(_buf, _host);
  }
  DynamicTaskPtr run(){
    return ((*_obj).*(method))(_begin, _end);
  }

  // RemoteTask methods
  size_t serialSize() {
    return _obj->serialSize() + (sizeof _begin) + (sizeof _end);
  }
  void serialize(char* buf, RemoteHost& host) {
    _obj->serialize(buf, host);
    buf += _obj->serialSize();
    memcpy(buf, _begin, sizeof _begin);
    buf += (sizeof _begin);
    memcpy(buf, _end, sizeof _end);
    buf += (sizeof _end);
  }
  void unserialize(const char* buf, RemoteHost& host) {
    JASSERT(!_obj);
    _obj = new T(buf, host, false);
    buf += _obj->serialSize();
    memcpy(_begin, buf, sizeof _begin);
    buf += (sizeof _begin);
    memcpy(_end, buf, sizeof _end);
    buf += (sizeof _end);
  }
  void migrateRegions(RemoteHost& sender) {
    _obj->migrateRegions(sender);
  }
  void getDataHosts(DataHostPidList& list) {
    ((*_obj).*(getDataHostsMethod))(list, _begin, _end);
  }
  RemoteObjectGenerator generator() {
    return &RemoteTaskReciever< SpatialMethodCallTask_distributed<T, D, method, getDataHostsMethod> >::gen;
  }

private:
  jalib::JRef<T> _obj;
  IndexT _begin[D];
  IndexT _end[D];
};

template<typename T, int D, DynamicTaskPtr (*method)(IndexT begin[D], IndexT end[D], const jalib::JRef<T>& metadata)>
class SpatialMethodCallTask_partial : public DynamicTask {
public:
  SpatialMethodCallTask_partial(IndexT begin[D], IndexT end[D], const jalib::JRef<T>& metadata)
    : _metadata(metadata)
  {
    //JTRACE("partial task");
    memcpy(_begin, begin, sizeof _begin);
    memcpy(_end,   end,   sizeof _end);
  }
  DynamicTaskPtr run(){
    return (*(method))(_begin, _end, _metadata);
  }
private:
  IndexT _begin[D];
  IndexT _end[D];
  jalib::JRef<T> _metadata;
};

/**
 * A task that calls a method on a given object, with a given region and
 * a given position.
 */
template< typename T, typename MetadataClass, int D, DynamicTaskPtr (T::*method)(jalib::JRef<MetadataClass> metadata, IndexT current[D])>
class IterationTrampMethodCallTask : public NullDynamicTask {
public:
  IterationTrampMethodCallTask(const jalib::JRef<T>& obj,
                               const jalib::JRef<MetadataClass>& metadata,
                               const IndexT current[D])
    : _obj(obj), _metadata(metadata)
  {
    memcpy(_current, current, sizeof _current);
  }

  DynamicTaskPtr run(){
    return ((*_obj).*(method))(_metadata, _current);
  }
private:
  jalib::JRef<T> _obj;
  jalib::JRef<MetadataClass> _metadata;
  IndexT _current[D];
};

/**
 * A task that calls a method with a given metadata.
 */
template< typename MetadataClass, DynamicTaskPtr (*method)(jalib::JRef<MetadataClass> metadata)>
class MetadataMethodCallTask : public NullDynamicTask {
public:
  MetadataMethodCallTask(const jalib::JRef<MetadataClass>& metadata)
    : _metadata(metadata) {}

  DynamicTaskPtr run(){
    return (*(method))(_metadata);
  }
private:
  jalib::JRef<MetadataClass> _metadata;
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
      _parts[_i]->enqueue();
      _all->dependsOn(_parts[_i]);
      _parts[_i] = 0;
      ++_i;
      return new MethodCallTask<GroupedDynamicTask, &GroupedDynamicTask::run>(this);
    }
    if(_i == N){
      //make sure all tasks have completed
      return _all;
    }
    JASSERT(false);
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

