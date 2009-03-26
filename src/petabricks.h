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

#include "matrix.h"
#include "matrixio.h"
#include "transforminstance.h"
#include "ruleinstance.h"
#include "dynamictask.h"
#include "spatialdynamictask.h"
#include "petabricksruntime.h"
#include "jtunable.h"
#include "config.h"

#ifdef HAVE_MATH_H
#  include <math.h>
#endif 

#ifdef HAVE_FFTW3_H
#  include <fftw3.h>
#endif

#define PB_SPAWN(taskname, args...) \
  petabricks::spawn_hook( new taskname ## _instance(args), _before, _after )

#define PB_CALL(taskname, args...) \
  petabricks::call_hook( new taskname ## _instance(args) )

#define PB_STATIC_CALL(taskname, args...) \
  petabricks::static_call_hook( new taskname ## _instance(args) )

#define PB_SYNC() \
  petabricks::sync_hook( _before, _after )

#define PB_NOP() 

#define PB_RETURN(rv)\
  if(false){}else{ _pb_rv=(rv); return DEFAULT_RV; }

#define PB_CAT(a,b) _PB_CAT(a,b)
#define _PB_CAT(a,b) __PB_CAT(a,b)
#define __PB_CAT(a,b) a ## b



namespace petabricks {
  inline void spawn_hook(const TransformInstancePtr& tx, const DynamicTaskPtr& before, const DynamicTaskPtr& after){
    DynamicTaskPtr task = tx->runAfter(before);
    after->dependsOn(task);
    task->enqueue();
  }
  
  inline void call_hook(const TransformInstancePtr& tx){
    tx->runToCompletion();
  }
  
  inline void static_call_hook(const TransformInstancePtr& tx){
    tx->runStatic();
  }

  inline void sync_hook(DynamicTaskPtr& before, DynamicTaskPtr& after){
    before = after;
    after = new NullDynamicTask();
    after->dependsOn(before);
    before->enqueue();
  }
  
}

