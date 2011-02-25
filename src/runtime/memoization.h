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

