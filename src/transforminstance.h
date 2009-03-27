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
#ifndef PETABRICKSTRANSFORMINSTANCE_H
#define PETABRICKSTRANSFORMINSTANCE_H

#include "jrefcounted.h"
#include "dynamictask.h"

namespace petabricks {

class TransformInstance;
typedef jalib::JRef<TransformInstance> TransformInstancePtr;

/**
 * base clase for instances of user transforms
 */
class TransformInstance : public jalib::JRefCounted {
public:
  virtual ~TransformInstance(){}
  virtual DynamicTaskPtr runDynamic() = 0;
  virtual void runStatic() = 0;


  DynamicTaskPtr runAfter(const DynamicTaskPtr& before){
    if(before){
      DynamicTaskPtr t = new MethodCallTask<TransformInstance, &TransformInstance::runDynamic>(this);
      t->dependsOn(before);
      return t;
    }else{
      return runDynamic();
    }
  }
  
  void runToCompletion(){
    DynamicTaskPtr p = runDynamic();
    if(p){
      p->enqueue();
      p->waitUntilComplete();
    }
  }
};

}

#endif
