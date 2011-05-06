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
#ifndef PETABRICKSTRANSFORMINSTANCE_H
#define PETABRICKSTRANSFORMINSTANCE_H

#include "dynamictask.h"

#include "common/jrefcounted.h"

namespace petabricks {

class TransformInstance;
typedef jalib::JRef<TransformInstance> TransformInstancePtr;

/**
 * base clase for instances of user transforms
 */
class TransformInstance : public jalib::JRefCounted {
public:
  virtual ~TransformInstance(){}
//  virtual DynamicTaskPtr runDynamic() = 0;

//DynamicTaskPtr runAfter(const DynamicTaskPtr& before){
//  if(before){
//    DynamicTaskPtr t = new MethodCallTask<TransformInstance, &TransformInstance::runDynamic>(this);
//    t->dependsOn(before);
//    return t;
//  }else{
//    return runDynamic();
//  }
//}
  
//void runToCompletion(){
//  DynamicTaskPtr p = runDynamic();
//  if(p){
//    p->enqueue();
//    p->waitUntilComplete();
//  }
//}
};

}

#endif
