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
#ifndef PETABRICKSMATRIXSTORAGE_H
#define PETABRICKSMATRIXSTORAGE_H

//#ifdef HAVE_OPENCL
#include "openclutil.h"
#include <oclUtils.h>
//#endif

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "common/hash.h"

namespace petabricks {

class MatrixStorage;
class MatrixStorageInfo;
typedef jalib::JRef<MatrixStorage> MatrixStoragePtr;
typedef jalib::JRef<MatrixStorageInfo> MatrixStorageInfoPtr;
typedef std::vector<MatrixStoragePtr> MatrixStorageList;

/**
 * The raw data for a Matrix
 */
class MatrixStorage : public jalib::JRefCounted {
public:
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;
  typedef jalib::Hash HashT;
private:
  //no copy constructor
  MatrixStorage(const MatrixStorage&);
public:
  ///
  /// Constructor
  MatrixStorage(size_t n) : _count(n) {
    _data = new ElementT[n];
  }

  ///
  /// Destructor
  ~MatrixStorage(){
    delete [] _data;
  }

  ElementT* data() { return _data; }
  const ElementT* data() const { return _data; }

  size_t count() const { return _count; }

  ///
  /// Fill the matrix with random data
  void randomize();

  ///
  /// generate a single random number
  static MATRIX_ELEMENT_T rand();

  HashT hash() const { 
    jalib::HashGenerator g;
    g.update(_data, _count*sizeof(ElementT));
    return g.final();
  }
  
  void print() {
    for(size_t i = 0; i < _count; i++)
      std::cout << _data[i] << " ";
    std::cout << std::endl;
  }
private:
  ElementT* _data;
  size_t _count;
};


/**
 * Capable of storing any type of MatrixRegion plus a hash of its data
 */
class MatrixStorageInfo : public jalib::JRefCounted {
  typedef MatrixStorage::IndexT IndexT;
  typedef MatrixStorage::ElementT ElementT;
  typedef jalib::Hash HashT;
public:
  const MatrixStoragePtr& storage() const { return _storage; }
  ElementT* base() const { return _storage->data()+_baseOffset; }
  int dimensions() const { return _dimensions; }
  const IndexT* multipliers() const { return _multipliers; }
  const IndexT* sizes() const { return _sizes; }
  const HashT& hash() const { return _hash; }
  ElementT extraVal() const { return _extraVal; }
  ssize_t bytes() const {
    return _count*sizeof(ElementT);
  }
  ssize_t count() const {
    return _count;
  }

  void setStorage(const MatrixStoragePtr& s, const ElementT* base);
  void setSizeMultipliers(int dim, const IndexT* mult, const IndexT* siz);
  void setMultipliers(const IndexT* mult);
  void setExtraVal(ElementT v=0);
  void computeDataHash();
  void reset();
  void releaseStorage();
  MatrixStorageInfo();
  ~MatrixStorageInfo();
  bool isMetadataMatch(const MatrixStorageInfo& that) const;
  bool isDataMatch(const MatrixStorageInfo& that) const;

  void print() {
    std::cout << "MatrixStorage " << this << std::endl;
    if(_dimensions>0)
      _storage->print();
    else
      std::cout << _extraVal << std::endl;
  }

#ifdef HAVE_OPENCL
  // Increase reference count to the gpu mem.
  // Return true when it's first initialized.
  bool initGpuMem(cl_context& context);

  // Decrease reference count to the gpu mem.
  // Return true when it's last reference to gpu mem is removed.
  void finishGpuMem(cl_command_queue& queue,bool modify);
  void check(cl_command_queue& queue);
  bool doneReadBuffer();
  void releaseCLMem();
  bool isModified() { return _isModified; }
  MatrixStoragePtr getGpuOutputStoragePtr() { return _gpuOutputBuffer; }
  void incCoverage(int size);
  //std::vector<GpuDynamicPtr>& getOwners() { return _owners; }

  ssize_t getBaseOffset() { return _baseOffset; }

  cl_mem _clmem;
#endif

private:
  MatrixStoragePtr _storage;
  int     _dimensions;
  ssize_t _baseOffset;
  IndexT  _multipliers[MAX_DIMENSIONS];
  IndexT  _sizes[MAX_DIMENSIONS];
  ElementT _extraVal;
  HashT   _hash;
  ssize_t _count;

#ifdef HAVE_OPENCL
  int _coverage;
  int _refCount;
  bool _hasGpuMem;
  bool _isModified;
  MatrixStoragePtr _gpuOutputBuffer;
  cl_event event;

  /*CL_CALLBACK decreaseDependency(cl_event event, cl_int cmd_exec_status, void *user_data)
  {
    // handle the callback here.
    std::cerr << this << " : CALL BACK!!!!!!!!!!!!!!!" << std::endl;
  }*/
#endif
};




}

#endif

