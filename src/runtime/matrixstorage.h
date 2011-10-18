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

#include <set>
#include <map>

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "common/hash.h"

#ifdef HAVE_OPENCL
#include <oclUtils.h>
#include "openclutil.h"
#endif

namespace petabricks {

class RegionNodeGroupMap;

typedef std::pair<std::string, std::set<int> > RegionNodeGroup;
typedef jalib::JRef<RegionNodeGroupMap> RegionNodeGroupMapPtr;

class RegionNodeGroupMap: public jalib::JRefCounted, 
                          public std::multimap<std::string, std::set<int> > {
public:
  RegionNodeGroupMap(){}
};

class MatrixStorage;
class MatrixStorageInfo;
typedef jalib::JRef<MatrixStorage> MatrixStoragePtr;
typedef jalib::JRef<MatrixStorageInfo> MatrixStorageInfoPtr;
typedef std::vector<MatrixStoragePtr> MatrixStorageList;
typedef std::vector<std::set<int> > NodeGroups;
#ifdef HAVE_OPENCL
class CopyoutInfo;
typedef jalib::JRef<CopyoutInfo> CopyoutInfoPtr;
#endif

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
    float *temp = new float[_count];
    for (unsigned int i = 0; i < _count; ++i) {
        temp[i] = _data[i];
    }
    g.update(temp, _count*sizeof(float));
    delete [] temp;
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
  size_t count() const {
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
  void modifyOnCpu() { _cpuModify = true; }  //TODO: do I need to use lock?
  void setName(std::string name) { _name = name; }

  ///
  /// call after run gpu PREPARE task
  bool initGpuMem(cl_context& context);

  ///
  /// call after run gpu RUN task
  void finishGpuMem(cl_command_queue& queue,int nodeID, RegionNodeGroupMapPtr map);

  ///
  /// store a region that is modified on gpu
  void incCoverage(IndexT* begin, IndexT* end, int size);

  ///
  /// get a memmory buffer for read buffer from gpu
  MatrixStoragePtr getGpuOutputStoragePtr(int nodeID);

  CopyoutInfoPtr getCopyoutInfo(int nodeID);
  void releaseCLMem();
  void check(cl_command_queue& queue);

  ssize_t getBaseOffset() { return _baseOffset; }

  std::vector<IndexT*>& getBegins() { return _begins; }
  std::vector<IndexT*>& getEnds() { return _ends; }

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
  std::string _name;
  int _coverage;
  bool _hasGpuMem;
  bool _cpuModify;

  ///
  /// regions that are modified on gpu
  std::vector<IndexT*> _begins;
  std::vector<IndexT*> _ends;
  NodeGroups _completeGroups;
  NodeGroups _remainingGroups;
  std::vector<int> _doneCompleteNodes;
  std::vector<int> _doneRemainingNodes;
  std::map<int, CopyoutInfoPtr> _copyMap;

  void startReadBuffer(cl_command_queue& queue, std::set<int>& doneNodes);

  /*CL_CALLBACK decreaseDependency(cl_event event, cl_int cmd_exec_status, void *user_data)
  {
    // handle the callback here.
    std::cerr << this << " : CALL BACK!!!!!!!!!!!!!!!" << std::endl;
  }*/
#endif
};

#ifdef HAVE_OPENCL
class CopyoutInfo : public jalib::JRefCounted {
  typedef MatrixStorage::IndexT IndexT;
public:
  CopyoutInfo(cl_command_queue& queue, MatrixStorageInfoPtr originalBuffer, std::vector<IndexT*>& begins, std::vector<IndexT*>& ends, int coverage);
  bool complete();
  bool done() { return _done; }
  std::vector<IndexT*>& getBegins() { return _begins; }
  std::vector<IndexT*>& getEnds() { return _ends; }
  MatrixStoragePtr getGpuOutputStoragePtr() { return _gpuOutputBuffer; }

private:
  std::vector<IndexT*> _begins;
  std::vector<IndexT*> _ends;
  MatrixStorageInfoPtr _originalBuffer;
  MatrixStoragePtr     _gpuOutputBuffer;
  cl_event _event;
  int _coverage;
  bool _done;
};
#endif

}

#endif

