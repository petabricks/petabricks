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
#ifndef PETABRICKSSPECIALIZEDDYNAMICTASK_H
#define PETABRICKSSPECIALIZEDDYNAMICTASK_H

#include "dynamictask.h"
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

