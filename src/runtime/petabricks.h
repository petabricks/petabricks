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

#include "dynamictask.h"
#include "matrix.h"
#include "matrixio.h"
#include "petabricksruntime.h"
#include "ruleinstance.h"
#include "spatialdynamictask.h"
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

#define PB_NOP() 0

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
    JASSERT(bin<cnts.size())(bin)(cnts.size());
#endif
    return std::max<int>(min, cnts[bin]);
  }
}


