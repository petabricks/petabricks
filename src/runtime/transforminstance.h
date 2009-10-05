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
