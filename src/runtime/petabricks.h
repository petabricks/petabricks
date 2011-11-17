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
#include "gpudynamictask.h"
#include "gpumanager.h"
#include "gpuspecializedtask.h"
#include "gputaskinfo.h"
#include "matrixio.h"
#include "matrixregion.h"
#include "memoization.h"
#include "petabricksruntime.h"
#include "remotetask.h"
#include "ruleinstance.h"
#include "specializeddynamictasks.h"
#include "transforminstance.h"

#include "common/jtunable.h"
#include "common/openclutil.h"

#include <algorithm>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_MATH_H
#  include <math.h>
#endif

#ifdef HAVE_ACCELERATE_ACCELERATE_H
#  include <Accelerate/Accelerate.h>
#endif

#ifdef HAVE_CBLAS_H
# include <cblas.h>
#endif

//these must be declared in the user code
petabricks::PetabricksRuntime::Main* petabricksMainTransform();
petabricks::PetabricksRuntime::Main* petabricksFindTransform(const std::string& name);
void _petabricksInit();
void _petabricksCleanup();

#define PB_SPAWN(taskname, args...) \
  PB_CAT(PB_CAT(taskname,_),PB_FLAVOR) (_completion, args)

#define PB_STATIC_CALL(taskname, args...) \
  taskname ## _sequential (NULL, args)

#define PB_NOP() (void)0

#define PB_RETURN(rv)\
  { _pb_rv=(rv); return DEFAULT_RV; }

#define PB_RETURN_VOID\
  return DEFAULT_RV

#define PB_CLEANUP_TASK\
  _cleanUpTask->dependsOn(_completion); _completion->enqueue(); DynamicTaskPtr cleanUpTaskTmp = _cleanUpTask; _cleanUpTask = NULL

#define PB_RETURN_DISTRIBUTED(rv)\
  { PB_CLEANUP_TASK; PB_RETURN(rv); }

#define PB_RETURN_VOID_DISTRIBUTED\
  { PB_CLEANUP_TASK; PB_RETURN_VOID; }

#define PB_CAT(a,b) _PB_CAT(a,b)
#define _PB_CAT(a,b) __PB_CAT(a,b)
#define __PB_CAT(a,b) a ## b

#define PB_SYNC() sync_in_loops_not_supported_yet!

#define IS_MISSING petabricks::is_the_missing_val

#define ACCURACY_TARGET (TRANSFORM_LOCAL(accuracyTarget)())
#define ACCURACY_BIN    (_acc_bin)

#define REGION_METHOD_CALL( region, method, args... )  region . method ( args )

#define CONVERT_TO_LOCAL(x) x._toLocalRegion()

namespace petabricks {
  template< typename T >
  inline DynamicTaskPtr tx_call_workstealing(T* tx){
    TransformInstancePtr txPtr(tx); //make sure tx gets deleted
    return tx->T::run(); //run without vtable use
  }


  template< typename T >
  inline DynamicTaskPtr tx_call_distributed(T* tx){
    TransformInstancePtr txPtr(tx); //make sure tx gets deleted
    return tx->T::run(); //run without vtable use
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

  template< typename T >
  inline bool is_data_local(const T& x){
    return x.isLocal();
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

  template < int D , int blockNumber>
  inline bool split_condition(IndexT thresh, IndexT begin[D], IndexT end[D]){
    //too small to split?
    for(int i=0; i<D; ++i)
      if(end[i]-begin[i] < blockNumber)
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

  typedef MatrixStoragePtr CArrayStorage;

  template<typename T>
  inline void to_c_array(const T& mr, ElementT*& ar, CArrayStorage& storage) {
    if(T::D>0 && mr.isLocal() && mr.isEntireBuffer()) {
      //TODO: check that layout is normal
      storage = mr.storage();
      ar      = storage->data();
      return;
    }

    storage = new MatrixStorage(mr.count());
    ar      = storage->data();

    _regioncopy(ar, mr);
  }

  inline void free_c_array(ElementT*& ar, CArrayStorage& storage) {
    if(storage) {
      ar = NULL;
      storage = NULL;
    }
  }

  template<typename T>
  inline void from_c_array(const T& mr, ElementT*& ar, CArrayStorage& storage) {
    if(T::D==0 || !mr.isLocal() || mr.storage() != storage) {
      _regioncopy(mr, ar);
    }
    free_c_array(ar, storage);
  }


}


