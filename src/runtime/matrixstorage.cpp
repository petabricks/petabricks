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
#include <ext/hash_set>

#include "matrixstorage.h"
#include "petabricksruntime.h"
#include "gpumanager.h"

#ifdef HAVE_OPENCL
petabricks::CopyPendingMap petabricks::CopyPendingMap::_pendingMap;

void petabricks::MatrixStorageInfo::modifyOnCpu(IndexT firstRow){
  //std::cout << "modifyOnCpu: _storage = " << &(*storage()) << ", row = " << firstRow << std::endl;
  if(firstRow < _firstRowOnCpu)
    _firstRowOnCpu = firstRow; 
}

void petabricks::MatrixStorage::updateDataFromGpu(IndexT firstRow){
  //std::cout << ">>> updateDataFromGpu" << std::endl;
  std::set<MatrixStorageInfoPtr>& pendings = CopyPendingMap::_pendingMap.allPendings(this);
  if(pendings.size() == 0)
    return;
  lock();
  pendings = CopyPendingMap::_pendingMap.allPendings(this);
  bool needUpdate = false;
  for(std::set<MatrixStorageInfoPtr>::iterator it = pendings.begin(); it != pendings.end(); ++it) {
#ifdef GPU_TRACE
    std::cout << "pending data: lastRowOnGpu = " << (*it)->lastRowOnGpu() << std::endl;
#endif
    if((*it)->lastRowOnGpu() - 1 >= firstRow) {
      needUpdate = true;
      break;
    }
  }
  if(needUpdate) {
    for(std::set<MatrixStorageInfoPtr>::iterator it = pendings.begin(); it != pendings.end(); ++it) {
      MatrixStoragePtr storage = (*it)->processPending();
      if(storage) {
#ifdef GPU_TRACE
	std::cout << "_storage = " << &(*this) << " on gpu!" << std::endl;
#endif
	(*it)->copy(this, storage, (*it)->getBegins(), (*it)->getEnds());
	(*it)->resetPending();
      }
    }
    CopyPendingMap::_pendingMap.clearPendings(this);
  }
  unlock();
}
#endif

MATRIX_ELEMENT_T petabricks::MatrixStorage::rand(){
  return petabricks::PetabricksRuntime::randDouble(-2147483648, 2147483648);
}

void petabricks::MatrixStorage::randomize(){
  for(size_t i=0;i<_count; ++i){
    _data[i] = MatrixStorage::rand();
  }
}

petabricks::MatrixStorageInfo::MatrixStorageInfo(){
  reset();
}

void petabricks::MatrixStorageInfo::setStorage(const MatrixStoragePtr& s, const ElementT* base){
  if(s){
    _storage=s;
    #ifdef DEBUG
    //std::cout << "base = " << base << " data = " << s->data() << " count = " << s->count() << " data + count = " << s->data()+s->count() << std::endl;
    JASSERT(base >= s->data());
    JASSERT(base < s->data()+s->count());
    #endif
    _baseOffset=base-s->data();
  }else{
    _storage=NULL;
    _baseOffset=0;
  }
  _hash=HashT();
}

void petabricks::MatrixStorageInfo::setSizeMultipliers(int dim, const IndexT* mult, const IndexT* siz){
  setSize(dim,siz);
  setMultipliers(mult);
}

void petabricks::MatrixStorageInfo::setSize(int dim, const IndexT* siz){
  _dimensions=dim;
  _count = 1;
  for(int d=0; d<_dimensions; ++d) {
    _sizes[d]=siz[d];
#ifdef HAVE_OPENCL
    _normalizedMultipliers[d] = _count;
#endif
    _count *= _sizes[d];
  }
#ifdef HAVE_OPENCL
  if(storage())
    _contiguous = (_count == storage()->count());
  _lastRowOnGpu = 0;
  _firstRowOnCpu = _sizes[_dimensions - 1];
#endif
}

void petabricks::MatrixStorageInfo::setMultipliers(const IndexT* mult){
  for(int d=0; d<_dimensions; ++d)
    _multipliers[d]=mult[d];
}

void petabricks::MatrixStorageInfo::setExtraVal(ElementT v)
{
  _extraVal=v;
}

void petabricks::MatrixStorageInfo::computeDataHash() { 
  if(_storage) 
    _hash=_storage->hash();
  else
    _hash=HashT();
}

void petabricks::MatrixStorageInfo::reset(){
  setExtraVal();
  setStorage(0,0);
  _dimensions=-1;

  #ifdef HAVE_OPENCL
  _hasGpuMem = false;
  _lastRowOnGpu = 0;
  _firstRowOnCpu = -1;
  _coverage = 0;
  _contiguous = true;
  #endif
}

void petabricks::MatrixStorageInfo::releaseStorage() { _storage=0; }


bool petabricks::MatrixStorageInfo::isMetadataMatch(const MatrixStorageInfo& that) const{
  if(_dimensions != that._dimensions) return false;
  if(_dimensions<0)                   return false;
  if(_baseOffset != that._baseOffset) return false;
  for(int d=0; d<_dimensions; ++d)
    if(_multipliers[d]!=that._multipliers[d])
      return false;
  for(int d=0; d<_dimensions; ++d)
    if(_sizes[d]!=that._sizes[d])
      return false;
  return true;
}

bool petabricks::MatrixStorageInfo::isDataMatch(const MatrixStorageInfo& that) const{
  if(_extraVal != that._extraVal) return false;
  if(_storage && _hash==HashT()) return false;
  return _hash==that._hash;
}

#ifdef HAVE_OPENCL
bool petabricks::MatrixStorageInfo::initGpuMem(cl_command_queue& queue, cl_context& context, double gpuRatio, bool input) {
#ifdef GPU_TRACE
  if(storage())
    std::cout << "initGpuMem " << &(*this) << " _storage = " << &(*storage()) << std::endl;
#endif

  // Set upperound of the region to be copied into GPU
  int upperbound;
  if(_dimensions == 0)
    upperbound = 0;
  else if(_iterDim != _dimensions || _lastRowOnGpuGuide == -1)  
    // If the given bound is -1, that means we need to copy entire region, so set upperbound to the size
    upperbound = _sizes[_dimensions - 1];
  else
    upperbound = _lastRowOnGpuGuide;

  if(upperbound < gpuRatio*_sizes[_dimensions - 1])
    upperbound = gpuRatio*_sizes[_dimensions - 1];

  _queue = queue;

  if(_hasGpuMem) {
    if(!input && _lastRowOnGpu >= upperbound) {
      // if everything is already on gpu
#ifdef GPU_TRACE
      std::cout << "initGpu: has updated gpu mem" << std::endl;
      JASSERT(_dimensions == 0 || _lastRowOnGpu <= _sizes[_dimensions - 1])(_lastRowOnGpu)( _sizes[_dimensions - 1])(_dimensions);
#endif
      return false;
    }
    if(_lastRowOnGpu >= upperbound && _firstRowOnCpu >= upperbound) {
      // gpu mem that we have is big enought for this output
#ifdef GPU_TRACE
      std::cout << "initGpu: has updated gpu mem" << std::endl;
      JASSERT(_dimensions == 0 || _lastRowOnGpu <= _sizes[_dimensions - 1])(_lastRowOnGpu)( _sizes[_dimensions - 1])(_dimensions);
#endif
      return false;
    }
  }

  if(storage()) {
    storage()->lock();
    std::set<MatrixStorageInfoPtr>& set = CopyPendingMap::_pendingMap.allPendings(storage());
    std::set<MatrixStorageInfoPtr>::iterator first = set.begin();
    
    if(set.size() == 1 && equal(*first)
       && (*first)->_lastRowOnGpu >= upperbound) { 
      // If 1) there is already buffer on GPU and there is one copy of it 2) the buffer is big enough to hold all data
      // then we will resuer this buffer.
      
#ifdef GPU_TRACE
      std::cout << "initGpu: obtain existing cl_mem ^^" << std::endl;
      std::cout << "initGpu: lastRowOnGpu = " << (*first)->_lastRowOnGpu << " , firstRowOnCpu = " << (*first)->_firstRowOnCpu << std::endl;
#endif

      setClMemWrapper((*first)->getClMemWrapper());
      _lastRowOnGpu = (*first)->_lastRowOnGpu;
      /*if(out){
      // for output buffer, transfer all data to the current one 
      // and remove the old one from the pendingMap
      _begins = (*first)->getBegins();
      _ends = (*first)->getEnds();
      _coverage = (*first)->coverage();
      //_gpuInputBuffers = (*first)->getGpuInputBuffers();
      (*first)->resetPending();
      CopyPendingMap::_pendingMap.clearPendings(storage());
      }*/
      storage()->unlock();
      //CopyoutInfoPtr info = new CopyoutInfo(queue, this, _begins, _ends, _coverage);
      _hasGpuMem = true;

#ifdef DEBUG
      JASSERT(_dimensions == 0 || _lastRowOnGpu <= _sizes[_dimensions - 1])(_lastRowOnGpu)( _sizes[_dimensions - 1])(_dimensions);
#endif

      if((*first)->_lastRowOnGpu <= this->_firstRowOnCpu && (*first)->_lastRowOnGpu <= (*first)->_firstRowOnCpu) {
	// don't have to copy in
	return false;
      }
      
      if(input){
	// If it is an input, we have cpu data on gpu, buffer on gpu and cpu will be the same, so first row on cpu is always the one after last row on gpu
	_firstRowOnCpu = _lastRowOnGpu;
      }
      return true; // need to copy in
    } //end if size == 1
    storage()->unlock();

    if(set.size() > 0) {
      // If there is remining data on GPU, need to update data on CPU first
      storage()->updateDataFromGpu();
    }
    
#ifdef AMD || INTEL
    // OpenCL on CPU
    if(input && _count == storage()->count()) {
      
      // Use host ptr without creating new buffer to avoid extra copy when running on CPU.
      cl_int err;
      std::cout << &(*this) << " use_host_ptr" << std::endl;
      
      // Buffer on gpu and cpu will be the same, so first row on cpu is always the one after last row on gpu
      _lastRowOnGpu = upperbound;
      _firstRowOnCpu = upperbound;
      setClMemWrapper(clCreateBuffer(context, CL_MEM_USE_HOST_PTR, bytesOnGpu(), storage()->data(), &err));
      JASSERT(_dimensions == 0 || _lastRowOnGpu <= _sizes[_dimensions - 1])(_lastRowOnGpu)( _sizes[_dimensions - 1])(_dimensions);
      JASSERT(CL_SUCCESS == err)(_dimensions)(_sizes[dimensions - 1])(_lastRowOnGpu).Text("Failed to create input memory object.");
      _hasGpuMem = true;

      return false;
    }
#endif
  } //end if storage()
  
  // Can't use host ptr because run on GPU.
  cl_int err;

  _lastRowOnGpu = upperbound;
  
  setClMemWrapper(clCreateBuffer(context, CL_MEM_READ_WRITE, bytesOnGpu(), NULL, &err));

#ifdef GPU_TRACE
  JASSERT(CL_SUCCESS == err).Text("Failed to create input memory object.");
  JASSERT(_dimensions == 0 || _lastRowOnGpu <= _sizes[_dimensions - 1])(_lastRowOnGpu)( _sizes[_dimensions - 1])(_dimensions)(upperbound);
  std::cout << "initGpu: " << &(*this) << " (not use_host_ptr) -> create buffer size = " << bytesOnGpu() << ", orig_size = " << bytes() << std::endl;
  std::cout << "initGpu: " << "cl_mem = " << getClMem() << std::endl;
#endif

  _hasGpuMem = true;
  if(input) {      
    // If it is an input, we have cpu data on gpu, buffer on gpu and cpu will be the same, so first row on cpu is always the one after last row on gpu
    _firstRowOnCpu = upperbound;
  }
  return true;
}

void petabricks::MatrixStorageInfo::check(cl_command_queue& queue) {
    std::cout << "input: check" << std::endl;
    std::cout << "baseoffset = " << _baseOffset << std::endl;
    print();
    std::cout << "clmem = " << getClMemWrapper()->getClMem() << std::endl;
    ElementT data[_count];
#ifdef NVIDIA
    clEnqueueReadBuffer(queue, getClMemWrapper()->getClMem(), CL_FALSE, 0, bytes(), data, 0, NULL, NULL);
#else
    clEnqueueReadBuffer(queue, getClMemWrapper()->getClMem(), CL_TRUE, 0, bytes(), data, 0, NULL, NULL);
#endif
    for(size_t i=0;i<_count;i++)
      std::cout << data[i] << " ";
    std::cout << std::endl << std::endl;
}

void petabricks::MatrixStorageInfo::finishGpuMem(cl_command_queue& queue, int nodeID, RegionNodeGroupMapPtr map, int gpuCopyOut) {
  #ifdef GPU_TRACE
  std::cout << "matrixstorageinfo   =   " << &(*this) << std::endl;
  std::cout << "nodeID = " << nodeID << " _name = " << _name << " gpuCopyOut = " << gpuCopyOut << std::endl;
  std::cout << "_storage = " << &(*storage()) << std::endl;
  for(RegionNodeGroupMap::iterator it = map->begin(); it != map->end(); ++it) {
    std::cout << "matrix = " << (*it).first << " : ";
    for(std::set<int>::iterator node = (*it).second.begin(); node != (*it).second.end(); ++node) {
      std::cout << *node << ", ";
    }
    std::cout << std::endl;
  }
  #endif

  if(gpuCopyOut != 0) {
    for(RegionNodeGroupMap::iterator it = map->equal_range(_name).first; it != map->equal_range(_name).second; ++it) {

      // Remove node that is already executed
      std::set<int> newGroup = (*it).second;
      for(std::vector<int>::iterator node = _doneRemainingNodes.begin(); node != _doneRemainingNodes.end(); ++node) {
        newGroup.erase(*node);
      }
      // Add new node group to its list
      _remainingGroups.push_back(newGroup);

      // Remove node whose data is already copied out
      newGroup = (*it).second;
      for(std::vector<int>::iterator node = _doneCompleteNodes.begin(); node != _doneCompleteNodes.end(); ++node) {
        newGroup.erase(*node);
      }
      // Add new node group to its list
      _completeGroups.push_back(newGroup);
    }

    // Put this node into done list
    _doneRemainingNodes.push_back(nodeID);

    std::set<int> doneNodes;
    for(size_t i = 0; i < _completeGroups.size(); ++i) {
      std::set<int>& remainingGroup = _remainingGroups[i];
      remainingGroup.erase(nodeID);

      #ifdef GPU_TRACE
      std::cout << "-----remaining group ------" << std::endl;
      for(std::set<int>::iterator node = remainingGroup.begin(); node != remainingGroup.end(); ++node) {
        std::cout << *node << ", ";
      }
      std::cout << std::endl;
      std::cout << "-----complete group ------" << std::endl;
      for(std::set<int>::iterator node = _completeGroups[i].begin(); node != _completeGroups[i].end(); ++node) {
        std::cout << *node << ", ";
      }
      std::cout << std::endl;
      #endif

      if(remainingGroup.empty())
        doneNodes.insert(_completeGroups[i].begin(), _completeGroups[i].end());
    }
    if(!doneNodes.empty()) {
      startReadBuffer(queue, doneNodes, gpuCopyOut==1 );
    }
  }
}

void petabricks::MatrixStorageInfo::startReadBuffer(cl_command_queue& queue, std::set<int>& doneNodes, bool now) {
  // TODO: wrong
  if(now) {
    CopyoutInfoPtr info = new CopyoutInfo(queue, this, _begins, _ends, _coverage);
    for(std::set<int>::iterator node = doneNodes.begin(); node != doneNodes.end(); ++node) {
      _doneCompleteNodes.push_back(*node);
      _copyMap[*node] = info;
      for(NodeGroups::iterator group = _completeGroups.begin(); group != _completeGroups.end(); ++group) {
        group->erase(*node);
      }
    }
    resetPending();
  }
  else if(_coverage != countOnGpu()) {
    // If don't cover the whole matrix, need to process pending copy-out
    storage()->lock();
    CopyPendingMap::_pendingMap.put(this);
    storage()->unlock();
    MatrixStoragePtr storage = processPending();
    if(storage) {
      copy(this->storage(), storage, getBegins(), getEnds());
    }
    resetPending();
  }
  else {
    #ifdef GPU_TRACE
    std::cout << "@@@ pending map: put " << &(*this) << std::endl;
    #endif
    storage()->lock();
    CopyPendingMap::_pendingMap.put(this);
    storage()->unlock();
  }
}

bool petabricks::MatrixStorageInfo::equal(MatrixStorageInfoPtr that) {
#ifdef GPU_TRACE
  std::cout << "@ equal that: " << &(*that) << " base = " << that->base() << std::endl;
  std::cout << "@ equal this: " << &(*this) << " base = " << that->base() << std::endl;
#endif
  if(base() != that->base() || dimensions() != that->dimensions() || queue() != that->queue()) {
    return false;
  }
  for(int i = 0; i < dimensions(); ++i) {
    if(multipliers()[i] != that->multipliers()[i] || sizes()[i] != that->sizes()[i]) {
      return false;
    }
  }
  printf("equal\n");
  return true;
}

petabricks::CopyoutInfoPtr petabricks::MatrixStorageInfo::getCopyoutInfo(int nodeID) {
  return _copyMap[nodeID];
}

petabricks::MatrixStoragePtr petabricks::MatrixStorageInfo::getGpuOutputStoragePtr(int nodeID) {
  return _copyMap[nodeID]->getGpuOutputStoragePtr();
}

void petabricks::MatrixStorageInfo::done(int nodeID) {
  _copyMap[nodeID] = NULL;
}

void petabricks::MatrixStorageInfo::addPending(std::vector<IndexT*>& begins, std::vector<IndexT*>& ends, int coverage) {
  if(coverage > _coverage) {
    _coverage = coverage;
  }
  if(_coverage < _count) {
    for(size_t i = 0; i != begins.size(); ++i) {
      _begins.push_back(begins[i]);
      _ends.push_back(ends[i]);
    }
  }
}

petabricks::MatrixStoragePtr petabricks::MatrixStorageInfo::processPending() {
  if(_coverage == 0)
    return NULL;
  //TODO: how to deal with queue when region is on multiple gpus?
  CopyoutInfoPtr copy = new CopyoutInfo(_queue, this, _begins, _ends, _coverage);
  while(!copy->complete()) {}
  //copy->getGpuOutputStoragePtr()->print();
  return copy->getGpuOutputStoragePtr();
}

void petabricks::MatrixStorageInfo::resetPending() {
  _coverage = 0;
  _begins.clear();
  _ends.clear();
}

void petabricks::MatrixStorageInfo::incCoverage(IndexT* begin, IndexT* end, int size) {
  _coverage += size;

  IndexT* myBegin = new IndexT[_dimensions];
  memcpy(myBegin, begin, sizeof(IndexT) * _dimensions);
  IndexT* myEnd = new IndexT[_dimensions];
  memcpy(myEnd, end, sizeof(IndexT) * _dimensions);
  _begins.push_back(myBegin);
  _ends.push_back(myEnd);
}

petabricks::CopyoutInfo::CopyoutInfo(cl_command_queue& queue, MatrixStorageInfoPtr originalBuffer, std::vector<IndexT*>& begins, std::vector<IndexT*>& ends, int coverage) {
#ifdef GPU_TRACE
  std::cout << "CopyoutInfo _storageinfo = " << &(*originalBuffer) << std::endl;
#endif
  _coverage = coverage;
  if(_coverage == 0) {
    _empty = true;
    _done = true;
  }
  else {
    _empty = false;
    _done = false;
  }

  if(!_empty) {
    // if not cover the whole matrix, need to store which regions we need to copy
    // if cover the whole matrix, don't store. We copy all.
    if(originalBuffer->isContiguous() && _coverage != originalBuffer->countOnGpu()) {
      _begins = begins;
      _ends = ends;
    }
    
    // Create storage for read data
    if(originalBuffer->dimensions() == 0 || !originalBuffer->isContiguous() || originalBuffer->countOnGpu() != _coverage)
      _gpuOutputBuffer = new MatrixStorage(originalBuffer->count());
    else
      _gpuOutputBuffer = originalBuffer->storage();
    
    //cl_int err = clSetEventCallBack(event, CL_COMPLETE, NULL, NULL);
    
    // Read buffer
    clEnqueueReadBuffer(queue, originalBuffer->getClMem(), CL_FALSE, 0, originalBuffer->bytesOnGpu(), _gpuOutputBuffer->data(), 0, NULL, &_event);
    clFlush(queue);
  }
}

bool petabricks::CopyoutInfo::closed() {
#ifdef GPU_TRACE
  std::cout << "already done" << std::endl;
#endif
  return _done;
}

bool petabricks::CopyoutInfo::complete() {
#ifdef DEBUG
  JASSERT(!_empty);
#endif
  cl_int ret;
  clGetEventInfo(_event, CL_EVENT_COMMAND_EXECUTION_STATUS, sizeof(cl_int), &ret, NULL);
#ifdef GPU_TRACE
  //std::cout << "status = " << ret << "  queue = " << CL_QUEUED << "  submitted = " << CL_SUBMITTED << "  running = " << CL_RUNNING << "  complete = " << CL_COMPLETE << std::endl;
#endif
  if(ret == CL_COMPLETE)
    _done = true;
  return _done;
}

#endif
