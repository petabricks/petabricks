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

//#define GPU_TRACE 1

#include <set>
#include <map>
#include <math.h>

#include "common/hash.h"
#include "common/jassert.h"
#include "common/jmutex.h"
#include "common/jrefcounted.h"
#include "common/openclutil.h"

namespace petabricks {

class RegionNodeGroupMap;

typedef std::pair<std::string, std::set<int> > RegionNodeGroup;
typedef jalib::JRef<RegionNodeGroupMap> RegionNodeGroupMapPtr;

class RegionNodeGroupMap: public jalib::JRefCounted, 
                          public std::multimap<std::string, std::set<int> > {
public:
  RegionNodeGroupMap(){}
};

class CopyoutInfo;
typedef jalib::JRef<CopyoutInfo> CopyoutInfoPtr;
class CopyPendingMap;

class ClMemWrapper: public jalib::JRefCounted {
public:
  ClMemWrapper(cl_mem mem) {
    _clmem = mem;
  }
  ~ClMemWrapper() { 
    if(_clmem) {
      //std::cout << "release clmem (deconstructor): " << _clmem << std::endl;
#ifdef HAVE_OPENCL
      clReleaseMemObject(_clmem);
#else
      UNIMPLEMENTED();
#endif
    }
  }
  //void setClMem(cl_mem mem) { _clmem = mem; }
  cl_mem getClMem() { return _clmem; }
  void* getClMemPtr(){ return (void*)&_clmem; }
private:
  cl_mem _clmem;
};
typedef jalib::JRef<ClMemWrapper> ClMemWrapperPtr;

class MatrixStorage;
class MatrixStorageInfo;
typedef jalib::JRef<MatrixStorage> MatrixStoragePtr;
typedef jalib::JRef<MatrixStorageInfo> MatrixStorageInfoPtr;
typedef std::vector<MatrixStoragePtr> MatrixStorageList;
typedef std::vector<std::set<int> > NodeGroups;

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
        if (fpclassify(temp[i]) == FP_ZERO) temp[i] = 0;
        if (fpclassify(temp[i]) == FP_NAN) temp[i] = fabs(temp[i]);
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

#ifdef HAVE_OPENCL
  void lock() { _lock.lock(); }
  void unlock() { _lock.unlock(); }
#else
  void lock()   { UNIMPLEMENTED(); }
  void unlock() { UNIMPLEMENTED(); }
#endif

private:
  ElementT* _data;
  size_t _count;
#ifdef HAVE_OPENCL
  jalib::JMutex  _lock;
#endif
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

  size_t bytes() const {
    return _count*sizeof(ElementT);
  }

  size_t count() const {
    return _count;
  }

  size_t coordToIndex(const IndexT* coord) const{
    IndexT rv = 0;
    for(int i=0; i<_dimensions; ++i){
      rv +=  _multipliers[i] * coord[i];
    }
    return _baseOffset + rv;
  }

  size_t coordToNormalizedIndex(const IndexT* coord) const{
#ifdef HAVE_OPENCL
    IndexT rv = 0;
    for(int i=0; i<_dimensions; ++i){
      rv +=  _normalizedMultipliers[i] * coord[i];
    }
    return rv;
#else
    USE(coord);
    UNIMPLEMENTED();
    return 0;
#endif
  }

  int incCoord(IndexT* coord) const{
    int D = dimensions();
    if(D==0)
     return -1;
    int i;
    coord[0]++;
    for(i=0; i<D-1; ++i){
      if(coord[i] >= _sizes[i]){
        coord[i]=0;
        coord[i+1]++;
      }else{
        return i;
      }
    }
    if(coord[D-1] >= _sizes[D-1]){
      return -1;
    }else{
      return D-1;
    }
  }

  int incCoordWithBound(IndexT* coord, const IndexT* c1, const IndexT* c2) const{
    int D = dimensions();
    if(D==0)
     return -1;
    int i;
    coord[0]++;
    for(i=0; i<D-1; ++i){
      if(coord[i] >= c2[i]){
        coord[i]=c1[i];
        coord[i+1]++;
      }else{
        return i;
      }
    }
    if(coord[D-1] >= c2[D-1]){
      return -1;
    }else{
      return D-1;
    }
  }

  void setStorage(const MatrixStoragePtr& s, const ElementT* base);
  void setSizeMultipliers(int dim, const IndexT* mult, const IndexT* siz);
  void setSize(int dim, const IndexT* siz);
  void setMultipliers(const IndexT* mult);
  void setExtraVal(ElementT v=0);
  void computeDataHash();
  void reset();
  void releaseStorage();
  MatrixStorageInfo();
  bool isMetadataMatch(const MatrixStorageInfo& that) const;
  bool isDataMatch(const MatrixStorageInfo& that) const;

  void print() {
    std::cout << "MatrixStorage " << this << std::endl;
    if(_dimensions>0)
      _storage->print();
    else
      std::cout << _extraVal << std::endl;
  }
  
  ssize_t getBaseOffset() { return _baseOffset; }

#ifdef HAVE_OPENCL
  void modifyOnCpu() { _cpuModify = true; }  //TODO: do I need to use lock?
  void setName(std::string name) { _name = name; }
  std::string getName() { return _name; }
  bool equal(MatrixStorageInfoPtr that);
  cl_command_queue& queue() { return _queue; }

  ///
  /// call after run gpu PREPARE task
  bool initGpuMem(cl_command_queue& queue, cl_context& context, bool input);

  ///
  /// call after run gpu RUN task
  void finishGpuMem(cl_command_queue& queue,int nodeID, RegionNodeGroupMapPtr map, int gpuCopyOut);
  MatrixStoragePtr processPending();
  void resetPending();
  void addPending(std::vector<IndexT*>& begins, std::vector<IndexT*>& ends, int coverage);

  ///
  /// store a region that is modified on gpu
  void incCoverage(IndexT* begin, IndexT* end, int size);
  int coverage() { return _coverage; }
  std::vector<IndexT*>& getBegins() { return _begins; }
  std::vector<IndexT*>& getEnds() { return _ends; }

  ///
  /// get a memmory buffer for read buffer from gpu
  MatrixStoragePtr getGpuOutputStoragePtr(int nodeID);
  CopyoutInfoPtr getCopyoutInfo(int nodeID);
  
  void releaseCLMem();
  void check(cl_command_queue& queue);
  void done(int nodeID);

  ClMemWrapperPtr _clMemWrapper;

  ClMemWrapperPtr getClMemWrapper() {
    return _clMemWrapper;
  }

  void setClMemWrapper(ClMemWrapperPtr wrapper) {
    _clMemWrapper = wrapper;
  }

  cl_mem getClMem() {
    return _clMemWrapper->getClMem();
  }

  void setClMemWrapper(cl_mem mem) {
    _clMemWrapper = new ClMemWrapper(mem);
  }

  void* getClMemPtr() {
    return _clMemWrapper->getClMemPtr();
  }

  ///
  /// Copy data of src to this
  void copy(const MatrixStoragePtr& dest, const MatrixStoragePtr& src) const
  {
#ifdef GPU_TRACE
    std::cout << "copyFrom all" << std::endl;
#endif
    if(src->data() == dest->data())
      return;
    IndexT coord[dimensions()];
    memset(coord, 0, sizeof coord);
    do {
      //*const_cast<MATRIX_ELEMENT_T*>(this->coordToPtr(coord)) = src.cell(coord);
      //std::cout << "address = " << dest->data() << "index = " << coordToIndex(coord) << std::endl;
      *(dest->data() + coordToIndex(coord)) = *(src->data() + coordToNormalizedIndex(coord));
    } while(incCoord(coord)>=0);
  }

  ///
  /// Copy data within the boundary c1 and c2 of src to this
  void copy(const MatrixStoragePtr& dest, const MatrixStoragePtr& src,const IndexT* c1, const IndexT* c2) const
  {
#ifdef GPU_TRACE
    std::cout << "copyFrom boundary" << std::endl;
    for(int i = 0; i < dimensions(); i++)
      std::cout << "[" << i << "] : " << c1[i] << "-" << c2[i] << std::endl;
    //src->print();
#endif
    if(src->data() == dest->data())
      return;
    for(int i = 0; i < dimensions(); i++)
      if(c1[i] >= c2[i])
        return;
    IndexT coord[dimensions()];
    memcpy(coord, c1, sizeof coord);
    do {
      //*const_cast<MATRIX_ELEMENT_T*>(this->coordToPtr(coord)) = src.cell(coord);
      //std::cout << "address = " << dest->data() << " index = " << coordToIndex(coord) << " normalized index = " << coordToNormalizedIndex(coord) << std::endl;
      //std::cout << "src->data = " << *(src->data() + coordToNormalizedIndex(coord)) << std::endl;
      *(dest->data() + coordToIndex(coord)) = *(src->data() + coordToNormalizedIndex(coord));
    } while(incCoordWithBound(coord, c1, c2)>=0);
    //std::cout << "base = " << base() << std::endl;
    //dest->print();
  }

  ///
  /// Copy data within the given boundaries of src to this
  void copy(const MatrixStoragePtr& dest, const MatrixStoragePtr& src,std::vector<IndexT*>& begins, std::vector<IndexT*>& ends) const
  {
    #ifdef DEBUG
    JASSERT(begins.size() == ends.size())(begins.size())(ends.size());
    #endif
    if(src->data() == dest->data())
      return;

    if(begins.size() == 0){
      copy(dest,src);
    }
    else{
      for(size_t i = 0; i < begins.size(); ++i)
        copy(dest, src, begins[i], ends[i]);
    }
  }

  void addGpuInputBuffer(MatrixStoragePtr& buffer) {
    _gpuInputBuffers.push_back(buffer);
  }

  std::vector<MatrixStoragePtr>& getGpuInputBuffers() { return _gpuInputBuffers; }
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
  std::vector<MatrixStoragePtr> _gpuInputBuffers;
  IndexT  _normalizedMultipliers[MAX_DIMENSIONS];
  
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

  cl_command_queue _queue;

  void startReadBuffer(cl_command_queue& queue, std::set<int>& doneNodes, bool now);

  /*CL_CALLBACK decreaseDependency(cl_event event, cl_int cmd_exec_status, void *user_data)
  {
    // handle the callback here.
    std::cerr << this << " : CALL BACK!!!!!!!!!!!!!!!" << std::endl;
  }*/
#endif
};

class CopyoutInfo : public jalib::JRefCounted {
  typedef MatrixStorage::IndexT IndexT;
public:
  CopyoutInfo(cl_command_queue& queue, MatrixStorageInfoPtr originalBuffer, std::vector<IndexT*>& begins, std::vector<IndexT*>& ends, int coverage);
  bool complete();
  bool done();
  std::vector<IndexT*>& getBegins() { return _begins; }
  std::vector<IndexT*>& getEnds() { return _ends; }
  MatrixStoragePtr getGpuOutputStoragePtr() { return _gpuOutputBuffer; }
  int coverage() { return _coverage; }

private:
  std::vector<IndexT*> _begins;
  std::vector<IndexT*> _ends;
  MatrixStoragePtr     _gpuOutputBuffer;
  cl_event _event;
  size_t _coverage;
  bool _done;
};

class CopyPendingMap : public jalib::JRefCounted {
public:
  void put(const MatrixStorageInfoPtr& info) {
#ifdef HAVE_OPENCL
    _lock.lock();
    std::map<MatrixStoragePtr, std::set<MatrixStorageInfoPtr> >::iterator it = _map.find(info->storage());
    std::map<MatrixStoragePtr, std::set<MatrixStorageInfoPtr> >::iterator end = _map.end();
    _lock.unlock();
    if(it != end) {
      std::set<MatrixStorageInfoPtr> infoList = it->second;
      bool add = false; 
      std::set<MatrixStorageInfoPtr>::iterator i;
      for(i = infoList.begin(); i != infoList.end(); ++i) {
        if((*i)->equal(info)) {
          (*i)->addPending(info->getBegins(), info->getEnds(), info->coverage());
          add = true;
          break;
        }
      }
      
      if(!add) {
        it->second.insert(info);
      }
    }
    else {
      std::set<MatrixStorageInfoPtr> newInfoList;
      newInfoList.insert(info);
      _lock.lock();
      _map[info->storage()] = newInfoList;
      _lock.unlock();
    }

    /*std::cout << "@@@ pending map: contains..." << std::endl;
    std::set<MatrixStorageInfoPtr> set = _map[info->storage()];
    for(std::set<MatrixStorageInfoPtr>::iterator i = set.begin(); i != set.end(); ++i) {
      std::cout << &(*(*i)) << std::endl;
    }*/
#else
    USE(info);
    UNIMPLEMENTED();
#endif
  }

  std::set<MatrixStorageInfoPtr>& allPendings(MatrixStoragePtr storage) {     
    /*std::cout << "@@@ pending map: allPendings..." << std::endl;
    std::set<MatrixStorageInfoPtr> set = _map[storage];
    std::cout << "size = " << set.size() << std::endl;
    for(std::set<MatrixStorageInfoPtr>::iterator i = set.begin(); i != set.end(); ++i) {
      std::cout << &(*(*i)) << std::endl;
    }*/
    _lock.lock();
    std::set<MatrixStorageInfoPtr>& pendings = _map[storage];
    _lock.unlock();
    return pendings; 

  }
  
  void clearPendings(MatrixStoragePtr storage) { 
    _lock.lock();
    std::set<MatrixStorageInfoPtr>& pendings = _map[storage];
    _lock.unlock();
    pendings.clear(); 
    //std::cout << "@@@ pending map: clear" << std::endl;
  }

  void print() {
    std::cout << "@@@ pending map: print" << std::endl;
    for(std::map<MatrixStoragePtr, std::set<MatrixStorageInfoPtr> >::iterator it = _map.begin(); it != _map.end(); ++it) {
      std::cout << "storage: " << &(*(it->first)) << std::endl;
      std::set<MatrixStorageInfoPtr>& set = it->second;
      for(std::set<MatrixStorageInfoPtr>::iterator i = set.begin(); i != set.end(); ++i) {
        std::cout << "  " << &(*(*i)) << std::endl;
      }
    }
  }

  void addBuffer(MatrixStoragePtr buffer) {
    _bufferlock.lock();
    _buffers.push_back(buffer);
    _bufferlock.unlock();
  }
  
  static CopyPendingMap _pendingMap;

private:
  std::map<MatrixStoragePtr, std::set<MatrixStorageInfoPtr> > _map;
  jalib::JMutex  _lock;

  std::vector<MatrixStoragePtr> _buffers;
  jalib::JMutex  _bufferlock;
};




}

#endif

