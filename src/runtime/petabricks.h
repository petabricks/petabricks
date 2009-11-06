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

#ifdef HAVE_FFTW3_H
#  include <fftw3.h>
#endif


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

  inline IndexT interpolate_sizespecific(jalib::JTunableArray& cnts, int input_size, int min){
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
  
  
  template < int D >
  inline bool split_condition(jalib::TunableValue thresh, IndexT begin[D], IndexT end[D]){
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

}


