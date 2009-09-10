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
#ifndef PETABRICKSSPECIALIZEDDYNAMICTASK_H
#define PETABRICKSSPECIALIZEDDYNAMICTASK_H

#include "dynamictask.h"
#include "matrix.h"
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


template< typename T, int D, DynamicTaskPtr (T::*method)(IndexT begin[D], IndexT end[D]), DimMaskT LTMask, DimMaskT EQMask, DimMaskT ReverseMask>
class SpatialBlockingTask : public DynamicTask {
public:
  typedef SpatialMethodCallTask<T,D,method> SubTask;

  static DynamicTaskPtr create(const jalib::JRef<T>& obj, IndexT begin[D], IndexT end[D], IndexT blockingSize){
    if(D <= 8*sizeof(DimMaskT)){ // our mask type is not large enough?
      //check of size of region is 0
      for(int i=0; i<D; ++i){
        if(end[i] == begin[i])
          return new NullDynamicTask();
      }
      //check if we need to block more
      for(int i=0; i<D; ++i){
        if(end[i]-begin[i] > blockingSize)
          return new SpatialBlockingTask(obj, begin, end, blockingSize);
      }
    }
    //just run the task directly
    return new SubTask(obj, begin, end);
  }

  
  SpatialBlockingTask(const jalib::JRef<T>& obj, IndexT begin[D], IndexT end[D], int blockingSize)
    : _blockingSize(blockingSize)
    , _i(0)
    , _obj(obj)
  {
    memcpy(_begin, begin, sizeof _begin);
    memcpy(_end,   end,   sizeof _end);
  }

  inline static DimMaskT mask(int d) { return ((DimMaskT)1)<<d; }

  DynamicTaskPtr run(){
    IndexT begin[D];
    IndexT end[D];

    //fill begin and end regions
    for(DimMaskT d=0; d<D; ++d){
      IndexT mid = (_begin[d]+_end[d])/2;
      bool isFirst    = ((mask(d)&_i) == 0);
      bool isReversed = ((mask(d)&ReverseMask) != 0);
      if( isFirst != isReversed ){
        begin[d] = _begin[d];
        end[d] = mid;
      }else{
        begin[d] = mid;
        end[d] = _end[d];
      }
    }

    //create the task
    DynamicTaskPtr t = create(_obj, _begin, _end, _blockingSize);
    _subtasks[_i] = t;

    //add deps
    for(DimMaskT n=0; n<_i; ++n){
      for(DimMaskT d=0; d<D; ++d){
        bool ldep = ((mask(d)&LTMask) != 0);
        bool edep = ((mask(d)&EQMask) != 0);
        if( ( ldep && (mask(d)&n) != (mask(d)&_i) )
         || ( edep && (mask(d)&n) == (mask(d)&_i) )){
          _subtasks[_i]->dependsOn(_subtasks[n]);
        }
      }
    }
    
    //increment our progress
    ++_i;

    if(_i<D){
      //still more work to do, return continuation task
      t->enqueue();
      return new MethodCallTask<SpatialBlockingTask, &SpatialBlockingTask::run>(this);
    }else{
      return t;
    }
  }
private:
  int _blockingSize;
  int _i;
  jalib::JRef<T> _obj;
  IndexT _begin[D];
  IndexT _end[D];
  DynamicTaskPtr _subtasks[1<<D];
};

}

#endif

