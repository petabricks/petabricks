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

#include "dynamictask.h"
#include "matrixio.h"
#include "matrixregion.h"
#include "memoization.h"
#include "petabricksruntime.h"
#include "ruleinstance.h"
#include "specializeddynamictasks.h"
#include "transforminstance.h"

#include "common/jtunable.h"

#include <algorithm>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_MATH_H
#  include <math.h>
#endif

#ifdef HAVE_OPENCL
#  include "openclutil.h"
#endif

//these must be declared in the user code
petabricks::PetabricksRuntime::Main* petabricksMainTransform();
petabricks::PetabricksRuntime::Main* petabricksFindTransform(const std::string& name);

#define PB_SPAWN(taskname, args...) \
  petabricks::spawn_hook( taskname ## _dynamic (args), _completion)

#define PB_STATIC_CALL(taskname, args...) \
  taskname ## _static (args)

#define PB_NOP() (void)0

#define PB_RETURN(rv)\
  { _pb_rv=(rv); return DEFAULT_RV; }

#define PB_RETURN_VOID\
  return DEFAULT_RV

#define PB_CAT(a,b) _PB_CAT(a,b)
#define _PB_CAT(a,b) __PB_CAT(a,b)
#define __PB_CAT(a,b) a ## b

#define PB_SYNC() sync_in_loops_not_supported_yet!

#define IS_MISSING petabricks::is_the_missing_val

#define ACCURACY_TARGET (TRANSFORM_LOCAL(accuracyTarget)())
#define ACCURACY_BIN    (_acc_bin)

namespace petabricks {
  template< typename T >
  inline DynamicTaskPtr tx_call_dynamic(T* tx){
    TransformInstancePtr txPtr(tx); //make sure tx gets deleted
    return tx->T::runDynamic(); //run without vtable use
  }
  
  template< typename T >
  inline DynamicTaskPtr run_task(T* task){
    DynamicTaskPtr ptr(task); //make sure task gets deleted
    return task->T::run(); //no vtable use
  }

  inline void spawn_hook(const DynamicTaskPtr& task,  const DynamicTaskPtr& completion){
    if(task){
      completion->dependsOn(task);
      task->enqueue();
    }
  }

  inline DynamicTaskPtr sync_hook(DynamicTaskPtr& completion, const DynamicTaskPtr& cont){
    DynamicTaskPtr tmp = completion;
    completion = new NullDynamicTask();
    if(tmp){
      completion->dependsOn(tmp);
      cont->dependsOn(tmp);
      tmp->enqueue();
    }
    return cont;
  }

  template < typename A, typename B >
  inline A side_effect_hook(const A& a, const B&){
    return a;
  }

  inline void enqueue_and_wait(const DynamicTaskPtr& task){
    if(task){
      task->enqueue();
      task->waitUntilComplete();
    }
  }

  inline int size_to_bin(IndexT size){
    return (int)log2(size);
  }

  inline IndexT interpolate_sizespecific(jalib::JTunableIntArray& cnts, int input_size, int min){
    int bin = size_to_bin(input_size);
    if(!(bin>=0)) bin = 0;
#ifdef DEBUG
    JASSERT(bin<(int)cnts.size())(bin)(cnts.size());
#endif
    IndexT rv = cnts[bin];
    if(rv<min){
      PetabricksRuntime::untrained();//usually aborts us
      return min;
    }
    return rv;
  }

  inline double interpolate_sizespecific(jalib::JTunableDoubleArray& cnts, int input_size, double min){
    int bin = size_to_bin(input_size);
    if(!(bin>=0)) bin = 0;
#ifdef DEBUG
    JASSERT(bin<(int)cnts.size())(bin)(cnts.size());
#endif
    double rv = cnts[bin];
    if(rv<min){
      PetabricksRuntime::untrained();//usually aborts us
      return min;
    }
    return rv;
  }
 

 

  template < int D >
  inline bool split_condition(IndexT thresh, IndexT begin[D], IndexT end[D]){
    //too small to split?
    for(int i=0; i<D; ++i)
      if(end[i]-begin[i] < 2)
        return false;
    //big enough to split?
    for(int i=0; i<D; ++i)
      if(end[i]-begin[i] > thresh)
        return true;
    //i guess not...
    return false;
  }


  //special val for optional values that dont exist 
  inline ElementT the_missing_val() {
    union {
      ElementT d;
      uint64_t u;
    };
    d = std::numeric_limits<ElementT>::quiet_NaN();
    u ^= 0x1234;
    return d;
  }
  inline bool is_the_missing_val(ElementT a) {
    ElementT b=the_missing_val();
    return memcmp(&a, &b, sizeof(ElementT))==0;
  }
}

#define REGION_METHOD_CALL( region, method, args... )  region . method ( args )

