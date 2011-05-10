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
#ifndef PETABRICKSMEMOIZATION_H
#define PETABRICKSMEMOIZATION_H

#include "matrixstorage.h"

#include "common/jmutex.h"

namespace petabricks {

class MemoizationSiteInterface {
private:
};

template< int IN, int OUT>
class MemoizationInstance {
public:
  MatrixStorageInfo& input(size_t n) { return _inputs[n]; }
  MatrixStorageInfo& output(size_t n) { return _outputs[n]; }
  const MatrixStorageInfo& input(size_t n)  const { return _inputs[n]; }
  const MatrixStorageInfo& output(size_t n) const { return _outputs[n]; }


  bool isMetadataMatch(const MemoizationInstance& that) const{
    for(int i=0; i<IN; ++i)
      if(!input(i).isMetadataMatch(that.input(i)))
        return false;
    for(int i=0; i<OUT; ++i)
      if(!output(i).isMetadataMatch(that.output(i)))
        return false;
    return true;
  }

  bool isInputDataMatch(const MemoizationInstance& that) const{
    for(int i=0; i<IN; ++i)
      if(!input(i).isDataMatch(that.input(i)))
        return false;
    return true;
  }

  void hashInputs(){
    for(int i=0; i<IN; ++i)
      input(i).computeDataHash();
  }

  void hashOutputs(){
    for(int i=0; i<OUT; ++i)
      output(i).computeDataHash();
  }

  void setOutputs(const MemoizationInstance& that){
    for(int i=0; i<OUT; ++i){
      output(i) = that.output(i);
    }
  }
  
  void releaeInputStorage(){
    for(int i=0; i<IN; ++i){
      input(i).releaseStorage();
    }
  }
private:
  MatrixStorageInfo _inputs[IN];
  MatrixStorageInfo _outputs[OUT];
};


template< int IN, int OUT>
class MemoizationSite {
public:
  bool memoize(MemoizationInstance<IN, OUT>& mi){
    JLOCKSCOPE(_lock);
    mi.hashInputs();
    if(mi.isMetadataMatch(_last) && mi.isInputDataMatch(_last)){
      mi.setOutputs(_last);
      JTRACE("using cached value");
      return true;
    }
    _last=mi;
    //TODO: we dont wait until the computation is complete to cache the result
    //      we just assume this computation will finish before the cache is used
    JTRACE("updating cache");
    return false;
  }
private:
  jalib::JMutex _lock;
  MemoizationInstance<IN, OUT> _last;
};

}

#endif

