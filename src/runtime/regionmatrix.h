#ifndef PETABRICKSREGIONMATRIX_H
#define PETABRICKSREGIONMATRIX_H

#include <map>
#include <pthread.h>
#include <stdarg.h>
#include <string.h>

#include "common/jassert.h"

#include "cellproxy.h"
#include "matrixregion.h"
#include "matrixstorage.h"
#include "petabricksruntime.h"
#include "remotehost.h"
#include "regiondata0D.h"
#include "regiondatai.h"
#include "regiondataraw.h"
#include "regionhandler.h"

// #define DEBUG_SCRATCH_REGION 1

namespace petabricks {
  template< int D, typename ElementT> class RegionMatrixWrapper;

  //
  // RegionMatrixSliceInfo
  //
  class RegionMatrixSliceInfo;
  typedef jalib::JRef<RegionMatrixSliceInfo> RegionMatrixSliceInfoPtr;

  class RegionMatrixSliceInfo : public jalib::JRefCounted {
  private:
    int _numSliceDimensions;
    int* _sliceDimensions;
    IndexT* _slicePositions;
  private:
    // no copy constructor
    RegionMatrixSliceInfo(const RegionMatrixSliceInfo&);
  public:
    RegionMatrixSliceInfo(int n) : _numSliceDimensions(n) {
      _sliceDimensions = new int[n];
      _slicePositions = new IndexT[n];
    }

    ~RegionMatrixSliceInfo(){
      delete [] _sliceDimensions;
      delete [] _slicePositions;
    }

    int numSliceDimensions() const { return _numSliceDimensions; }

    int* sliceDimensions() { return _sliceDimensions; }
    const int* sliceDimensions() const { return _sliceDimensions; }
    int sliceDimensions(int i) const { return _sliceDimensions[i]; }

    IndexT* slicePositions() { return _slicePositions; }
    const IndexT* slicePositions() const { return _slicePositions; }
    IndexT slicePositions(int i) const { return _slicePositions[i]; }
  };

  //
  // RegionMatrix
  //
  template< int _D, typename ElementT>
  class RegionMatrix {
  public:
    enum { D = _D };
  protected:
    IndexT _size[D];
    IndexT _splitOffset[D];
    bool _isTransposed;
    RegionMatrixSliceInfoPtr _sliceInfo;
    RegionHandlerPtr _regionHandler;

  public:
    void init(const IndexT* size, const IndexT* splitOffset, const bool isTransposed, const RegionMatrixSliceInfoPtr sliceInfo, const RegionHandlerPtr handler) {

      const size_t sizeof_sizes = sizeof this->_size;
      const size_t sizeof_splitOffset = sizeof this->_splitOffset;

      if(sizeof_sizes > 0) {
        if (size != NULL) {
          memcpy(_size, size, sizeof_sizes);
        } else {
          memset(_size, -1, sizeof_sizes);
        }
      }

      if(sizeof_splitOffset > 0) {
        if (splitOffset != NULL) {
          memcpy(_splitOffset, splitOffset, sizeof_splitOffset);
        } else {
          memset(_splitOffset, 0, sizeof_splitOffset);
        }
      }

      _isTransposed = isTransposed;
      _sliceInfo = sliceInfo;
      setRegionHandler(handler);
    }
    void init(const IndexT* size, const RegionHandlerPtr handler) {
      init(size, NULL, false, new RegionMatrixSliceInfo(0), handler);
    }
    void copy(const RegionMatrix<D, MATRIX_ELEMENT_T>& that) {
      JASSERT(D == that.dimensions());
      init(that.size(), that.splitOffset(), that.isTransposed(), that.sliceInfo(), that.regionHandler());
    }
    void copy(const RegionMatrix<D, const MATRIX_ELEMENT_T>& that) {
      JASSERT(D == that.dimensions());
      init(that.size(), that.splitOffset(), that.isTransposed(), that.sliceInfo(), that.regionHandler());
    }

    INLINE void setRegionHandler(RegionHandlerPtr handler) {
      _regionHandler = handler;
    }

    //
    // Constructors
    //
    RegionMatrix() {
      init(NULL, NULL);
    }
    RegionMatrix(const IndexT* size) {
      init(size, new RegionHandler(D));
    }
    RegionMatrix(const IndexT* size, const RegionHandlerPtr handler) {
      init(size, handler);
    }
    RegionMatrix(const IndexT* size, const IndexT* splitOffset, const bool isTransposed, const RegionMatrixSliceInfoPtr sliceInfo, const RegionHandlerPtr handler) {
      init(size, splitOffset, isTransposed, sliceInfo, handler);
    }

    RegionMatrix(const RegionMatrix<D, MATRIX_ELEMENT_T>& that) {
      copy(that);
    }
    RegionMatrix operator=(const RegionMatrix<D, MATRIX_ELEMENT_T>& that) {
      copy(that);
      return *this;
    }
    RegionMatrix(const RegionMatrix<D, const MATRIX_ELEMENT_T>& that) {
      copy(that);
    }
    RegionMatrix operator=(const RegionMatrix<D, const MATRIX_ELEMENT_T>& that) {
      copy(that);
      return *this;
    }

    //
    // Getter
    //
    int dimensions() const { return D; }
    const IndexT* splitOffset() const { return _splitOffset; };
    bool isTransposed() const { return _isTransposed; };
    RegionMatrixSliceInfoPtr sliceInfo() const { return _sliceInfo; };
    RegionHandlerPtr regionHandler() const { return _regionHandler; };
    RegionDataIPtr regionData() const { return _regionHandler->regionData(); };

    //
    // gpu
    //
    void useOnCpu() {
      #ifdef HAVE_OPENCL
      if(D == 0) return;
      if(isLocal()) {
      	// Check if data is still resided in GPU
	this->storage()->updateDataFromGpu();
      }
      else {
      	// Already handled by clean_up_distributed task (in RegionMatrix::fromScratchRegion())
      }
      #endif
    }

    //
    // Initialization
    //
    void splitData(IndexT* splitSize) {
      _regionHandler->splitData(D, _size, splitSize);
    }

    void createDataPart(int partIndex, RemoteHostPtr host) {
      _regionHandler->createDataPart(partIndex, host);
    }

    void allocData(int distributedCutoff, int distributionType, int distributionSize) {
      _regionHandler->allocData(_size, distributedCutoff, distributionType, distributionSize);
    }

    void allocDataLocal() {
      _regionHandler->allocDataLocal(_size);
    }

    static RegionMatrix allocate(const IndexT size[D]) {
      RegionMatrix region = RegionMatrix(size);
      region.allocDataLocal();
      return region;
    }

    static RegionMatrix allocate(const IndexT size[D], int distributedCutoff, int distributionType, int distributionSize) {
      RegionMatrix region = RegionMatrix(size);
      region.allocData(distributedCutoff, distributionType, distributionSize);
      return region;
    }

    inline static RegionMatrix allocate() {
      IndexT c1[D];
      return allocate(c1);
    }

    //
    // Read & Write
    //
    MATRIX_ELEMENT_T readCell (const IndexT* coord) const {
      IndexT rd_coord[_regionHandler->dimensions()];
      this->regionDataCoord(coord, rd_coord);
      return _regionHandler->readCell(rd_coord);
    }

    void writeCell(const IndexT* coord, ElementT value) {
      IndexT rd_coord[_regionHandler->dimensions()];
      this->regionDataCoord(coord, rd_coord);
      _regionHandler->writeCell(rd_coord, value);
    }

    void invalidateCache() {
      _regionHandler->invalidateCache();
    }

    const IndexT* size() const { return _size; }
    IndexT size(int i) const {
      if (_isTransposed) {
        i = D - 1 - i;
      }
      return _size[i];
    }
    bool isSize(const IndexT size[D]) const{
      if (!_size) {
        return false;
      }
      for(int i=0; i<D; ++i){
        if(this->size(i) != size[i]){
          return false;
        }
      }
      return true;
    }
    bool isSize() const { return true; }

    IndexT width() const { return size(0); }
    IndexT height() const { return size(1); }
    IndexT depth() const { return size(2); }

    bool contains(const IndexT coord[D]) const {
      for(int i=0; i<D; ++i)
        if(coord[i]<0 || coord[i]>=size(i))
          return false;
      return true;
    }

    /// Number of elements in this region
    ssize_t count() const {
      ssize_t s=1;
      for(int i=0; i<D; ++i)
        s*=this->size()[i];
      return s;
    }

    CellProxy cell(IndexT coord[D]) const {
      IndexT rd_coord[_regionHandler->dimensions()];
      regionDataCoord(coord, rd_coord);
      return CellProxy(_regionHandler, rd_coord);
    }
    INLINE CellProxy cell() const {
      IndexT c1[0];
      return this->cell(c1);
    }

    //
    // Matrix manipulation
    //
    RegionMatrix<D, ElementT> splitRegion(const IndexT* offset, const IndexT* size) const {
      IndexT offset_new[_regionHandler->dimensions()];

      if (_isTransposed) {
        for (int i = 0; i < D; ++i) {
          offset_new[i] = _splitOffset[i] + offset[D - 1 - i];
        }

        IndexT size_n[_regionHandler->dimensions()];
        for (int i = 0; i < _regionHandler->dimensions(); i++) {
          size_n[i] = size[D - 1 - i];
        }

        return RegionMatrix<D, ElementT>
          (size_n, offset_new, _isTransposed, _sliceInfo, _regionHandler);

      } else {
        for (int i = 0; i < D; ++i) {
          offset_new[i] = _splitOffset[i] + offset[i];
        }
        return RegionMatrix<D, ElementT>
          (size, offset_new, _isTransposed, _sliceInfo, _regionHandler);

      }
    }

    RegionMatrix<D-1, ElementT> sliceRegion(int d, IndexT pos) const {
      if (_isTransposed) {
        d = D - d - 1;
      }

      int dimensions = D - 1;
      IndexT size[dimensions];
      memcpy(size, _size, sizeof(IndexT) * d);
      memcpy(size + d, _size + d + 1, sizeof(IndexT) * (dimensions - d));

      IndexT offset[dimensions];
      memcpy(offset, _splitOffset, sizeof(IndexT) * d);
      memcpy(offset + d, _splitOffset + d + 1, sizeof(IndexT) * (dimensions - d));

      // maintain ordered array of _sliceDimensions + update d as necessary
      RegionMatrixSliceInfoPtr sliceInfo =
        new RegionMatrixSliceInfo(_sliceInfo->numSliceDimensions() + 1);

      if (_sliceInfo->numSliceDimensions() == 0) {
        sliceInfo->sliceDimensions()[0] = d;
        sliceInfo->slicePositions()[0] = pos + _splitOffset[d];
      } else {
        bool isAddedNewD = false;
        for (int i = 0; i < sliceInfo->numSliceDimensions(); i++) {
          if (isAddedNewD) {
            sliceInfo->sliceDimensions()[i] = _sliceInfo->sliceDimensions(i-1);
            sliceInfo->slicePositions()[i] = _sliceInfo->slicePositions(i-1);
          } else if (d >= _sliceInfo->sliceDimensions(i)) {
            sliceInfo->sliceDimensions()[i] = _sliceInfo->sliceDimensions(i);
            sliceInfo->slicePositions()[i] = _sliceInfo->slicePositions(i);
            d++;
          } else {
            sliceInfo->sliceDimensions()[i] = d;
            sliceInfo->slicePositions()[i] = pos + _splitOffset[d];
            isAddedNewD = true;
          }
        }
      }

      return RegionMatrix<D-1, ElementT>
        (size, offset, _isTransposed, sliceInfo, _regionHandler);
    }

    RegionMatrixWrapper<D, ElementT> region(const IndexT c1[D], const IndexT c2[D]) const{
      IndexT newSizes[D];
      for(int i=0; i<D; ++i){
        #ifdef DEBUG
        JASSERT(c1[i]<=c2[i])(c1[i])(c2[i])
          .Text("region has negative size");
        JASSERT(c2[i]<=size(i))(c2[i])(size(i))
          .Text("region goes out of bounds");
        #endif
        newSizes[i]=c2[i]-c1[i];
      }
      return RegionMatrixWrapper<D, ElementT>(this->splitRegion(c1, newSizes));
    }

    RegionMatrixWrapper<D-1, ElementT> slice(int d, IndexT pos) const{
      return RegionMatrixWrapper<D-1, ElementT>(this->sliceRegion(d, pos));
    }
    RegionMatrixWrapper<D-1, ElementT> col(IndexT x) const{ return slice(0, x); }
    RegionMatrixWrapper<D-1, ElementT> column(IndexT x) const{ return slice(0, x); }
    RegionMatrixWrapper<D-1, ElementT> row(IndexT y) const{  return slice(1, y); }


    void transpose() {
      _isTransposed = !_isTransposed;
    }

    RegionMatrixWrapper<D, ElementT> transposed() const {
      RegionMatrix transposed = RegionMatrix(*this);
      transposed.transpose();
      return RegionMatrixWrapper<D, ElementT>(transposed);
    }

    RegionMatrixWrapper<D, MATRIX_ELEMENT_T> forceMutable() {
      return RegionMatrixWrapper<D, MATRIX_ELEMENT_T>(*this);
    }

    //
    // Migration
    //

    size_t serialSize() {
      size_t sz = sizeof(int);                    // D
      sz += sizeof(IndexT) * D;                   // _size
      sz += sizeof(IndexT) * D;                   // _splitOffset
      sz += sizeof(int);                          // _numSliceDimensions
      // _sliceDimensions
      sz += sizeof(int) * _sliceInfo->numSliceDimensions();
      // _slicePositions
      sz += sizeof(IndexT) * _sliceInfo->numSliceDimensions();
      sz += sizeof(bool);                         // _isTransposed
      sz += sizeof(int);                          // regionHandler dimension
      // regionhandler size
      sz += sizeof(IndexT) * _regionHandler->dimensions();
      sz += sizeof(RemoteRegionHandler);          // RemoteRegionHandler
      sz += sizeof(bool);                         // isDataSplit
      return sz;
    }

    void serialize(char* buf, RemoteHost& /*host*/) {
      size_t sz = sizeof(int);
      *reinterpret_cast<int*>(buf) = D;
      buf += sz;

      sz = sizeof(IndexT) * D;
      memcpy(buf, _size, sz);
      buf += sz;

      sz = sizeof(IndexT) * D;
      memcpy(buf, _splitOffset, sz);
      buf += sz;

      sz = sizeof(int);
      *reinterpret_cast<int*>(buf) = _sliceInfo->numSliceDimensions();
      buf += sz;

      sz = sizeof(int) * _sliceInfo->numSliceDimensions();
      memcpy(buf, _sliceInfo->sliceDimensions(), sz);
      buf += sz;

      sz = sizeof(IndexT) * _sliceInfo->numSliceDimensions();
      memcpy(buf, _sliceInfo->slicePositions(), sz);
      buf += sz;

      sz = sizeof(bool);
      *reinterpret_cast<bool*>(buf) = _isTransposed;
      buf += sz;

      sz = sizeof(int);
      *reinterpret_cast<int*>(buf) = _regionHandler->dimensions();
      buf += sz;

      sz = sizeof(IndexT) * _regionHandler->dimensions();
      memcpy(buf, _regionHandler->size(), sz);
      buf += sz;

      sz = sizeof(RemoteRegionHandler);
      RemoteRegionHandler remoteRegionHandler = _regionHandler->remoteRegionHandler();
      memcpy(buf, &remoteRegionHandler, sz);
      buf += sz;

      sz = sizeof(bool);
      *reinterpret_cast<bool*>(buf) = _regionHandler->isDataSplit();
      buf += sz;

      _regionHandler->incRefCount();
    }

    void unserialize(const char* buf, RemoteHost& /*host*/) {
      size_t sz = sizeof(int);
      JASSERT(*reinterpret_cast<const int*>(buf) == D)(*reinterpret_cast<const int*>(buf))(_D).Text("RegionMatrix dimension mismatch.");
      buf += sz;

      sz = sizeof(IndexT) * D;
      memcpy(_size, buf, sz);
      buf += sz;

      sz = sizeof(IndexT) * D;
      memcpy(_splitOffset, buf, sz);
      buf += sz;

      sz = sizeof(int);
      int numSliceDimensions = *reinterpret_cast<const int*>(buf);
      _sliceInfo = new RegionMatrixSliceInfo(numSliceDimensions);
      buf += sz;

      sz = sizeof(int) * numSliceDimensions;
      memcpy(_sliceInfo->sliceDimensions(), buf, sz);
      buf += sz;

      sz = sizeof(IndexT) * numSliceDimensions;
      memcpy(_sliceInfo->slicePositions(), buf, sz);
      buf += sz;

      sz = sizeof(bool);
      _isTransposed = *reinterpret_cast<const bool*>(buf);
      buf += sz;

      sz = sizeof(int);
      int regionHandlerDimensions = *reinterpret_cast<const int*>(buf);
      buf += sz;

      sz = sizeof(IndexT) * regionHandlerDimensions;
      const IndexT* regionHandlerSize = (const IndexT*)buf;
      buf += sz;

      sz = sizeof(RemoteRegionHandler);
      RemoteRegionHandler remoteRegionHandler = *reinterpret_cast<const RemoteRegionHandler*>(buf);
      buf += sz;

      sz = sizeof(bool);
      bool isDataSplit = *reinterpret_cast<const bool*>(buf);
      buf += sz;

      setRegionHandler(RegionHandlerDB::instance().getLocalRegionHandler(remoteRegionHandler.hostPid, remoteRegionHandler.remoteHandler, regionHandlerDimensions, regionHandlerSize, isDataSplit));
    }

    void updateHandlerChain() const {
      _regionHandler->updateHandlerChain();
    }

    //
    // Find location of data (data can be in many hosts)
    //
    void dataHosts(DataHostPidList& list, const IndexT* begin, const IndexT* end) const {
      IndexT rd_begin[_regionHandler->dimensions()];
      IndexT rd_end[_regionHandler->dimensions()];
      this->regionDataCoord(begin, rd_begin);
      this->regionDataCoord(end, rd_end);
      _regionHandler->hosts(rd_begin, rd_end, list);
    }

    void dataHosts(DataHostPidList& list) const {
      IndexT begin[D];
      IndexT end[D];

      memset(begin, 0, sizeof(IndexT) * D);
      for (int i = 0; i < D; i++) {
        end[i] = size(i);
      }

      dataHosts(list, begin, end);
    }

    void printDataHosts() const {
      DataHostPidList list;
      dataHosts(list);
      printf("(%d) DataHostPidList\n", getpid());
      for (unsigned int i = 0; i < list.size(); i++) {
        printf("  %lx/%d ==> %.5g\n", list[i].hostPid.hostid, list[i].hostPid.pid, list[i].weight);
      }
    }

    //
    // Similar to dataHosts, but will not send any remote messages
    //
    RemoteHostPtr dataHost() const {
      return _regionHandler->dataHost();
    }

    //
    // Local
    //
    typedef MatrixRegion<D, ElementT> LocalT;
    typedef MatrixRegion<D, const ElementT> ConstLocalT;

    bool isEntireBuffer() const {
      return isLocal() && _toLocalConstRegion().isEntireBuffer();
    }

    void exportTo(MatrixStorageInfo& ms) const {
      if(isLocal()){
        _toLocalRegion().exportTo(ms);
      }else{
        UNIMPLEMENTED();
      }
    }

    void copyFrom(const MatrixStorageInfo& ms){
      if(isLocal()){
        _toLocalRegion().copyFrom(ms);
      }else{
        UNIMPLEMENTED();
      }
    }

    MatrixStoragePtr storage() const {
      if(isLocal())
        return _toLocalRegion().storage();
      UNIMPLEMENTED();
      return NULL;
    }

    const MatrixStorageInfoPtr storageInfo() const {
      MatrixStoragePtr ms;
      ElementT* base;
      to_c_array(*this, base, ms);
      MatrixStorageInfoPtr info = new MatrixStorageInfo();
      info->setStorage(ms, base);
      info->setSize(D, _size);
      info->setExtraVal();
      return info;
    }

    // Compute metadata for Return StartOffset.
    void computeMatrixRegionMetaData(IndexT* startOffset, IndexT multipliers[D]) const {
      RegionDataIPtr regionData = _regionHandler->regionData();
      IndexT mult = 1;
      int last_slice_index = 0;

      for(int i = 0; i < regionData->dimensions(); i++){
        if ((last_slice_index < _sliceInfo->numSliceDimensions()) &&
            (i == _sliceInfo->sliceDimensions(last_slice_index))) {
          *startOffset += mult * _sliceInfo->slicePositions(last_slice_index);
          last_slice_index++;
        } else {
          multipliers[i - last_slice_index] = mult;
          *startOffset += mult * _splitOffset[i - last_slice_index];
        }
        mult *= regionData->size()[i];
      }
    }
    void computeMatrixRegionMetaData(MatrixRegionMetadata& metadata) const {
      metadata.dimensions = D;
      metadata.startOffset = 0;
      computeMatrixRegionMetaData(&(metadata.startOffset), metadata.multipliers);
      memcpy(metadata.size(), _size, sizeof(IndexT) * D);
    }

    int regionMatrixMetadataLen() const {
      return RegionMatrixMetadata::len(D, _sliceInfo->numSliceDimensions());
    }
    void computeRegionMatrixMetadata(RegionMatrixMetadata& metadata) const {
      metadata.dimensions = D;
      metadata.numSliceDimensions = _sliceInfo->numSliceDimensions();
      memcpy(metadata.splitOffset, _splitOffset, sizeof(IndexT) * D);
      memcpy(metadata.size(), _size, sizeof(IndexT) * D);
      memcpy(metadata.sliceDimensions(), _sliceInfo->sliceDimensions(), sizeof(int) * D);
      memcpy(metadata.slicePositions(), _sliceInfo->slicePositions(), sizeof(IndexT) * D);
      //JTRACE("metadata")(metadata.size()[0])(metadata.size()[1]);
    }

    bool isLocal() const {
      return (_regionHandler->type() == RegionDataTypes::REGIONDATARAW);
    }
    bool isRegionDataRaw() const {
      return (_regionHandler->type() == RegionDataTypes::REGIONDATARAW);
    }
    MatrixRegion<D, const ElementT> _toLocalConstRegion() const {
      return _toLocalRegion();
    }
    MatrixRegion<D, ElementT> _toLocalRegion() const {
      RegionDataIPtr regionData = _regionHandler->regionData();
      JASSERT(regionData->type() == RegionDataTypes::REGIONDATARAW).Text("Cannot cast to MatrixRegion.");

      IndexT startOffset = 0;
      IndexT multipliers[D];
      this->computeMatrixRegionMetaData(&startOffset, multipliers);

      MatrixRegion<D, ElementT> matrixRegion =
        MatrixRegion<D, ElementT>(regionData->storage(), regionData->storage()->data() + startOffset, _size, multipliers);

      if (_isTransposed) {
        matrixRegion = matrixRegion.transposed();
      }

      return matrixRegion;
    }

    //
    // Copy the entire matrix and store it locally. Writes to this copy
    // **might or might not** be seen by the original.
    void localCopy(RegionMatrix& scratch, bool cacheable=false) const {
      #ifdef DEBUG
      JASSERT(scratch.isRegionDataRaw());
      // Copy to the entire region
      RegionMatrix tmp = *this;
      if (_isTransposed) { tmp.transpose(); }
      JASSERT(tmp.isSize(scratch.size()));
      JASSERT(tmp.isSize(scratch.regionData()->size()));
      if (_isTransposed) { tmp.transpose(); }
      #endif

      if (isRegionDataRaw()) {
        // already local
        scratch.copy(*this);
        return;
      }

      size_t len = regionMatrixMetadataLen();
      char buf[len];
      CopyToMatrixStorageMessage* msg = (CopyToMatrixStorageMessage*) buf;
      this->computeRegionMatrixMetadata(msg->srcMetadata);

      char scratchMetadataBuf[scratch.regionMatrixMetadataLen()];
      RegionMatrixMetadata* scratchMetadata = (RegionMatrixMetadata*)scratchMetadataBuf;
      scratch.computeRegionMatrixMetadata(*scratchMetadata);

      // cacheable = false;
      if (cacheable) {
        RegionHandlerPtr newHandler = _regionHandler->copyToScratchMatrixStorageCache(msg, len, scratch.regionData()->storage(), scratchMetadata, scratch.regionData()->size(), scratch.regionHandler());
        if (newHandler) {
          JTRACE("found");
          scratch.setRegionHandler(newHandler);
        }

      } else {
        _regionHandler->copyToScratchMatrixStorage(msg, len, scratch.regionData()->storage(), scratchMetadata, scratch.regionData()->size());
      }

      if (_isTransposed) {
        scratch.transpose();
      }

      #ifdef DEBUG_SCRATCH_REGION
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        JASSERT(fabs(this->cell(coord) - scratch.cell(coord)) < 0.000001)(this->cell(coord))(scratch.cell(coord));
      } while (this->incCoord(coord) >= 0);
      #endif
    }

    RegionMatrix localCopy(bool cacheable=false) const {
      if (isRegionDataRaw()) {
        // already local
        return *this;
      }

      RegionMatrix copy = RegionMatrix(this->size());
      copy.allocDataLocal();
      localCopy(copy, cacheable);
      return copy;
    }

    //
    // copy to workstealing region
    //
    MatrixRegion<D, ElementT> toScratchRegion() const {
      if (isLocal()) {
        return _toLocalRegion();
      } else {
        return localCopy()._toLocalRegion();
      }
    }

    void fromScratchRegion(const MatrixRegion<D, ElementT>& /*scratch*/) const {
      // We need to pass metadata for scratchStorage to _regionHandler
      UNIMPLEMENTED();

      /*
      #ifdef DEBUG
      for (int i = 0; i < D; ++i) {
        JASSERT(size(i) == scratchOrig.size(i));
      }
      #endif

      MatrixRegion<D, ElementT> scratch;
      if (_isTransposed) {
        scratch = scratchOrig.transposed();
      } else {
        scratch = scratchOrig;
      }

      if (isRegionDataRaw()) {
        // Do nothing

      } else {
        unsigned int storage_count = 1;
        for (unsigned int i=0; i<D; ++i) {
          storage_count *= scratch.size(i);
        }

        size_t len = regionMatrixMetadataLen() + (storage_count * sizeof(ElementT));
        char buf[len];
        CopyFromMatrixStorageMessage* msg = (CopyFromMatrixStorageMessage*) buf;
        this->computeRegionMatrixMetadata(msg->srcMetadata);

        // Copy storage.
        if (scratch.storage()->count() == storage_count) {
          // send the entire storage
          memcpy(msg->storage(), scratch.storage()->data(), sizeof(ElementT) * storage_count);

        } else {
          unsigned int n = 0;
          IndexT coord[D];
          memset(coord, 0, sizeof coord);
          do {
            msg->storage()[n] = scratch.cell(coord);
            n++;
          } while(scratch.incCoord(coord) >= 0);
          JASSERT(n == storage_count)(n)(storage_count);
        }

        _regionHandler->copyFromScratchMatrixStorage(msg, len, scratch.storage());
      }

      #ifdef DEBUG_SCRATCH_REGION
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        JASSERT(fabs(this->cell(coord) - scratchOrig.cell(coord)) < 0.000001)(this->cell(coord))(scratchOrig.cell(coord));
      } while (this->incCoord(coord) >= 0);
      #endif
*/
    }

    void fromScratchRegion(const RegionMatrix& scratch) const {
      #ifdef DEBUG
      for (int i = 0; i < D; ++i) {
        JASSERT(size(i) == scratch.size(i));
      }
      #endif

      JASSERT(scratch.isRegionDataRaw());

      if (isRegionDataRaw()) {
        // Do nothing

      } else {
        unsigned int storage_count = 1;
        for (unsigned int i=0; i<D; ++i) {
          storage_count *= scratch.size(i);
        }

        size_t len = regionMatrixMetadataLen() + (storage_count * sizeof(ElementT));
        // allocate in heap since the message can be huge.
        char* buf = new char[len];

        CopyFromMatrixStorageMessage* msg = (CopyFromMatrixStorageMessage*) buf;
        this->computeRegionMatrixMetadata(msg->srcMetadata);

        char scratchMetadataBuf[scratch.regionMatrixMetadataLen()];
        RegionMatrixMetadata* scratchMetadata = (RegionMatrixMetadata*)scratchMetadataBuf;
        scratch.computeRegionMatrixMetadata(*scratchMetadata);

	#ifdef HAVE_OPENCL
	// TODO: check if this works
	scratch.storage()->updateDataFromGpu();
	#endif
        _regionHandler->copyFromScratchMatrixStorage(msg, len, scratch.storage(), scratchMetadata, scratch.regionHandler()->size());
        delete [] buf;
      }

      #ifdef DEBUG_SCRATCH_REGION
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        JASSERT(fabs(this->cell(coord) - scratch.cell(coord)) < 0.000001)(this->cell(coord))(scratch.cell(coord));
      } while (this->incCoord(coord) >= 0);
      #endif
    }

    void randomize() {
      _regionHandler->randomize();
    }

    void hash(jalib::HashGenerator& gen) {
      RegionMatrix tmp;
      if (D > 0) {
        tmp = this->localCopy();
      } else {
        tmp = *this;
      }

      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        float v = tmp.readCell(coord);
        gen.update(&v, sizeof(v));
      } while (this->incCoord(coord) >= 0);
    }


    int incCoord(IndexT* coord) const {
      if (D == 0) {
        return -1;
      }

      coord[0]++;
      for (int i = 0; i < D - 1; ++i){
        if (coord[i] >= size(i)){
          coord[i]=0;
          coord[i+1]++;
        } else{
          return i;
        }
      }
      if (coord[D - 1] >= size(D - 1)){
        return -1;
      }else{
        return D - 1;
      }
    }

    // Specialized va_list

    INLINE static RegionMatrix allocate(IndexT x){
      IndexT c1[] = {x};
      return allocate(c1);
    }
    INLINE CellProxy cell(IndexT x) const{
      IndexT c1[] = {x};
      return cell(c1);
    }
    INLINE bool isSize(IndexT x) const{
      IndexT c1[] = {x};
      return isSize(c1);
    }
    INLINE bool contains(IndexT x) const{
      IndexT c1[] = {x};
      return contains(c1);
    }
    INLINE RegionMatrix region(IndexT x1, IndexT x2) const{
      IndexT c1[] = {x1};
      IndexT c2[] = {x2};
      return region(c1,c2);
    }

    INLINE static RegionMatrix allocate(IndexT x, IndexT y){
      IndexT c1[] = {x, y};
      return allocate(c1);
    }
    INLINE CellProxy cell(IndexT x, IndexT y) const{
      IndexT c1[] = {x, y};
      return cell(c1);
    }
    INLINE bool isSize(IndexT x, IndexT y) const{
      IndexT c1[] = {x, y};
      return isSize(c1);
    }
    INLINE bool contains(IndexT x, IndexT y) const{
      IndexT c1[] = {x, y};
      return contains(c1);
    }
    INLINE RegionMatrix region(IndexT x1, IndexT y1, IndexT x2, IndexT y2) const{
      IndexT c1[] = {x1, y1};
      IndexT c2[] = {x2, y2};
      return region(c1,c2);
    }

    INLINE static RegionMatrix allocate(IndexT x, IndexT y, IndexT z){
      IndexT c1[] = {x, y, z};
      return allocate(c1);
    }
    INLINE CellProxy cell(IndexT x, IndexT y, IndexT z) const{
      IndexT c1[] = {x, y, z};
      return cell(c1);
    }
    INLINE bool isSize(IndexT x, IndexT y, IndexT z) const{
      IndexT c1[] = {x, y, z};
      return isSize(c1);
    }
    INLINE bool contains(IndexT x, IndexT y, IndexT z) const{
      IndexT c1[] = {x, y, z};
      return contains(c1);
    }
    INLINE RegionMatrix region(IndexT x1, IndexT y1, IndexT z1, IndexT x2, IndexT y2, IndexT z2) const{
      IndexT c1[] = {x1, y1, z1};
      IndexT c2[] = {x2, y2, z2};
      return region(c1,c2);
    }

    INLINE static RegionMatrix allocate(IndexT x, IndexT y, IndexT z, IndexT a){
      IndexT c1[] = {x, y, z, a};
      return allocate(c1);
    }
    INLINE CellProxy cell(IndexT x, IndexT y, IndexT z, IndexT a) const{
      IndexT c1[] = {x, y, z, a};
      return cell(c1);
    }
    INLINE bool isSize(IndexT x, IndexT y, IndexT z, IndexT a) const{
      IndexT c1[] = {x, y, z, a};
      return isSize(c1);
    }
    INLINE bool contains(IndexT x, IndexT y, IndexT z, IndexT a) const{
      IndexT c1[] = {x, y, z, a};
      return contains(c1);
    }
    INLINE RegionMatrix region(IndexT x1, IndexT y1, IndexT z1, IndexT a1, IndexT x2, IndexT y2, IndexT z2, IndexT a2) const{
      IndexT c1[] = {x1, y1, z1, a1};
      IndexT c2[] = {x2, y2, z2, a2};
      return region(c1,c2);
    }

  private:
    void regionDataCoord(const IndexT* coord_orig, IndexT* coord_new) const {
      IndexT slice_index = 0;
      IndexT split_index = 0;

      for (int d = 0; d < _regionHandler->dimensions(); d++) {
        if (slice_index < _sliceInfo->numSliceDimensions() &&
            d == _sliceInfo->sliceDimensions(slice_index)) {
          // slice
          coord_new[d] = _sliceInfo->slicePositions(slice_index);
          slice_index++;
        } else {
          // split
          int offset = _splitOffset[split_index];

          if (_isTransposed) {
            coord_new[d] = coord_orig[D - 1 - split_index] + offset;
          } else {
            coord_new[d] = coord_orig[split_index] + offset;
          }

          split_index++;
        }
      }
    }
  };

  //
  // RegionMatrixWrapper
  //
  template< int _D, typename ElementT>
  class RegionMatrixWrapper : public RegionMatrix<_D, ElementT> {
  public:
    enum {D = _D};

    typedef RegionMatrix<D, ElementT> Base;
    typedef const RegionMatrix<D, ElementT> ConstBase;

    RegionMatrixWrapper() : Base() {}
    RegionMatrixWrapper(const IndexT* size) : Base(size) {}

    RegionMatrixWrapper(const ElementT* data, const IndexT* size) : Base(size) {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      Base::_regionHandler->allocDataLocal(size);

      // TODO (yod): optimize this
      IndexT i = 0;
      do {
        this->writeCell(coord, data[i]);
        i++;
      } while (this->incCoord(coord) >= 0);
    }

    RegionMatrixWrapper(const RegionMatrix<D, MATRIX_ELEMENT_T>& that) : Base(that) {}
    RegionMatrixWrapper(const RegionMatrix<D, const MATRIX_ELEMENT_T>& that) : Base(that) {}

    // for testing
    void copyDataFromRegion(RegionMatrixWrapper in) {
      this->allocDataLocal();
      IndexT coord[D];
      memset(coord, 0, sizeof(IndexT) * D);
      do {
        this->writeCell(coord, in.readCell(coord));
      } while (this->incCoord(coord) >= 0);
    }

    void assertEqual(const RegionMatrixWrapper& that) {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        JASSERT(fabs(this->cell(coord) - that.cell(coord)) < 0.000001)(this->cell(coord))(that.cell(coord));
      } while (this->incCoord(coord) >= 0);
    }
  };


  //
  // RegionMatrix0DInfo
  //
  class RegionMatrix0DInfo;
  typedef jalib::JRef<RegionMatrix0DInfo> RegionMatrix0DInfoPtr;

  class RegionMatrix0DInfo : public jalib::JRefCounted {
  private:
    int _sourceDimensions;
    IndexT* _sourceIndex;
  private:
    // no copy constructor
    RegionMatrix0DInfo(const RegionMatrix0DInfo&);
  public:
    RegionMatrix0DInfo(int n) : _sourceDimensions(n) {
      _sourceIndex = new IndexT[n];
    }

    ~RegionMatrix0DInfo(){
      delete [] _sourceIndex;
    }

    int sourceDimensions() const { return _sourceDimensions; }

    IndexT* sourceIndex() { return _sourceIndex; }
    const IndexT* sourceIndex() const { return _sourceIndex; }

    void print() {
      printf("sourceinfo: ");
      for (int i = 0; i < _sourceDimensions; ++i) {
        printf("%d, ", _sourceIndex[i]);
      }
      printf("\n");
    }
  };

  template<typename ElementT>
    class RegionMatrixWrapper<0, ElementT> : public RegionMatrix<0, ElementT> {
  private:
    RegionMatrix0DInfoPtr _sourceInfo;

  public:
    enum { D = 0 };
    typedef RegionMatrix<D, ElementT> Base;

    void init(RegionMatrix0DInfoPtr sourceInfo, RegionHandlerPtr regionHandler) {
      this->setRegionHandler(regionHandler);
      if (sourceInfo) {
        _sourceInfo = sourceInfo;
      } else {
        _sourceInfo = new RegionMatrix0DInfo(0);
      }
    }

    RegionMatrixWrapper() : Base() {
      init(NULL, new RegionHandler(new RegionData0D()));
    }
    RegionMatrixWrapper(Base val) : Base() {
      init(NULL, val.regionHandler());
    }
    RegionMatrixWrapper(ElementT* data, IndexT* /*size*/) : Base() {
      RegionDataIPtr regionData = new RegionData0D();
      regionData->writeCell(NULL, *data);
      init(NULL, new RegionHandler(regionData));
    }
    RegionMatrixWrapper(const RegionMatrixWrapper& that) : Base() {
      init(that.sourceInfo(), that.regionHandler());
    }

    ///
    /// Implicit conversion from ElementT/CellProxy
    RegionMatrixWrapper(ElementT& value) : Base() {
      init(NULL, new RegionHandler(new RegionData0D(value)));
    }
    RegionMatrixWrapper(const ElementT& value) : Base() {
      init(NULL, new RegionHandler(new RegionData0D(value)));
    }
    RegionMatrixWrapper(CellProxy& value) : Base() {
      RegionMatrix0DInfoPtr sourceInfo = new RegionMatrix0DInfo(value._handler->dimensions());
      if (value._handler->dimensions() > 0) {
        memcpy(sourceInfo->sourceIndex(), value._index, sizeof(IndexT) * sourceInfo->sourceDimensions());
      }
      init(sourceInfo, value._handler);
    }
    RegionMatrixWrapper(const CellProxy& value) : Base() {
      RegionMatrix0DInfoPtr sourceInfo = new RegionMatrix0DInfo(value._handler->dimensions());
      if (value._handler->dimensions() > 0) {
        memcpy(sourceInfo->sourceIndex(), value._index, sizeof(IndexT) * sourceInfo->sourceDimensions());
      }
      init(sourceInfo, value._handler);
    }

    ///
    /// Allow implicit conversion to CellProxy
    operator CellProxy () const { return this->cell(); }

    operator MATRIX_ELEMENT_T () const { return this->readCell(NULL); }

    RegionMatrixWrapper operator=(Base val) {
      RegionHandlerPtr regionHandler = val.regionHandler();
      RegionMatrix0DInfoPtr sourceInfo = new RegionMatrix0DInfo(regionHandler->dimensions());
      JASSERT(regionHandler->dimensions() == val->sliceInfo()->numSliceDimensions());
      memcpy(sourceInfo->sourceIndex(), val->sliceInfo->slicePositions(), sizeof(IndexT) * regionHandler->dimensions());
      init(sourceInfo, regionHandler);

      // We might need this instead.
      // this->writeCell(NULL, val.readCell(NULL));
      return *this;
    }
    RegionMatrixWrapper operator=(const RegionMatrixWrapper& that) {
      init(that.sourceInfo(), that.regionHandler());
      return *this;
    }

    MATRIX_ELEMENT_T readCell (const IndexT*) const {
      return Base::_regionHandler->readCell(_sourceInfo->sourceIndex());
    }
    void writeCell(const IndexT*, ElementT value) const {
      Base::_regionHandler->writeCell(_sourceInfo->sourceIndex(), value);
    }

    CellProxy cell(IndexT coord[D]) const {
      return cell();
    }
    INLINE CellProxy cell() const {
      return CellProxy(Base::_regionHandler, _sourceInfo->sourceIndex());
    }

    RegionMatrix0DInfoPtr sourceInfo() const {
      return _sourceInfo;
    }

    // toLocalRegion
    bool isLocal() const {
      return (Base::_regionHandler->type() == RegionDataTypes::REGIONDATARAW)
        || (Base::_regionHandler->type() == RegionDataTypes::REGIONDATA0D)
        || (Base::_regionHandler->type() == RegionDataTypes::CONSTREGIONDATA0D);
    }
    MatrixRegion<D, ElementT> _toLocalRegion() const {
      RegionDataIPtr regionData = Base::_regionHandler->regionData();

      if (regionData->type() == RegionDataTypes::REGIONDATARAW
          || regionData->type() == RegionDataTypes::REGIONDATA0D) {
        return MatrixRegion<D, ElementT>(regionData->value0D(_sourceInfo->sourceIndex()));

      } else if (regionData->type() == RegionDataTypes::CONSTREGIONDATA0D) {
        MatrixRegion<D, ElementT> copy = MatrixRegion<D, ElementT>::allocate();
        copy.cell() = this->readCell(NULL);
        return copy;

      } else {
        JASSERT(false)(regionData->type()).Text("Cannot cast to MatrixRegion.");
        return MatrixRegion<D, ElementT>();
      }
    }

    void localCopy(RegionMatrixWrapper& scratch, bool=false) const {
      scratch.writeCell(NULL, this->readCell(NULL));
    }
    RegionMatrixWrapper localCopy(bool cacheable=false) const {
      if (isLocal()) {
        return *this;
      }
      RegionMatrixWrapper copy = RegionMatrixWrapper();
      localCopy(copy, cacheable);
      return copy;
    }
    void fromScratchRegion(const MatrixRegion<D, ElementT>& scratch) const {
      this->writeCell(NULL, scratch.cell(NULL));
    }
    void fromScratchRegion(const RegionMatrixWrapper& scratch) const {
      this->writeCell(NULL, scratch.readCell(NULL));
    }

    RegionMatrixWrapper region(const IndexT[D], const IndexT[D]) const{
      return *this;
    }

    static RegionMatrixWrapper allocate(const IndexT[D]) {
      return RegionMatrixWrapper();
    }
    static RegionMatrixWrapper allocate(const IndexT[D], int, int, int) {
      return RegionMatrixWrapper();
    }
    static RegionMatrixWrapper allocate() {
      return RegionMatrixWrapper();
    }

    size_t serialSize() {
      size_t sz = sizeof(int);                    // D
      sz += sizeof(int);                          // regionHandler dimension
      // regionhandler size
      sz += sizeof(IndexT) * Base::_regionHandler->dimensions();
      // source index
      sz += sizeof(IndexT) * Base::_regionHandler->dimensions();
      sz += sizeof(RemoteRegionHandler);          // RemoteRegionHandler
      sz += sizeof(bool);                         // isDataSplit

      return sz;
    }

    void serialize(char* buf, RemoteHost&) {
      size_t sz = sizeof(int);
      *reinterpret_cast<int*>(buf) = D;
      buf += sz;

      sz = sizeof(int);
      *reinterpret_cast<int*>(buf) = Base::_regionHandler->dimensions();
      buf += sz;

      sz = sizeof(IndexT) * Base::_regionHandler->dimensions();
      memcpy(buf, Base::_regionHandler->size(), sz);
      buf += sz;

      sz = sizeof(IndexT) * Base::_regionHandler->dimensions();
      memcpy(buf, _sourceInfo->sourceIndex(), sz);
      buf += sz;

      sz = sizeof(RemoteRegionHandler);
      RemoteRegionHandler remoteRegionHandler = Base::_regionHandler->remoteRegionHandler();
      memcpy(buf, &remoteRegionHandler, sz);
      buf += sz;

      sz = sizeof(bool);
      *reinterpret_cast<bool*>(buf) = Base::_regionHandler->isDataSplit();
      buf += sz;

      Base::_regionHandler->incRefCount();
    }

    void unserialize(const char* buf, RemoteHost&) {
      size_t sz = sizeof(int);
      JASSERT(*reinterpret_cast<const int*>(buf) == 0)(*reinterpret_cast<const int*>(buf)).Text("RegionMatrix dimension mismatch. (must be 0)");
      buf += sz;

      sz = sizeof(int);
      int regionHandlerDimensions = *reinterpret_cast<const int*>(buf);
      buf += sz;

      sz = sizeof(IndexT) * regionHandlerDimensions;
      const IndexT* regionHandlerSize = (const IndexT*)buf;
      buf += sz;

      sz = sizeof(IndexT) * regionHandlerDimensions;
      const IndexT* sourceIndex = (const IndexT*)buf;
      buf += sz;

      _sourceInfo = new RegionMatrix0DInfo(regionHandlerDimensions);
      if (regionHandlerDimensions > 0) {
        memcpy(_sourceInfo->sourceIndex(), buf, sz);
      }

      sz = sizeof(RemoteRegionHandler);
      RemoteRegionHandler remoteRegionHandler = *reinterpret_cast<const RemoteRegionHandler*>(buf);
      buf += sz;

      sz = sizeof(bool);
      bool isDataSplit = *reinterpret_cast<const bool*>(buf);
      buf += sz;

      Base::setRegionHandler(RegionHandlerDB::instance().getLocalRegionHandler(remoteRegionHandler.hostPid, remoteRegionHandler.remoteHandler, regionHandlerDimensions, regionHandlerSize, isDataSplit));
    }

  };

  // Specialized for ConstMatrixRegion0D.
  template<>
  class RegionMatrixWrapper<0, const MATRIX_ELEMENT_T> : public RegionMatrix<0, const MATRIX_ELEMENT_T> {
  public:
    enum { D = 0 };
    typedef const MATRIX_ELEMENT_T ElementT;
    typedef RegionMatrix<D, ElementT> Base;

    INLINE void initWithValue(ElementT value) {
      RegionDataIPtr regionData = new RegionData0D();
      regionData->writeCell(NULL, value);
      this->setRegionHandler(new RegionHandler(regionData));

      // TODO: ConstRegionData0D has a problem with forceMutable
      // this->setRegionHandler(new RegionHandler(new ConstRegionData0D(value)));
    }

    RegionMatrixWrapper() : Base() {
      initWithValue(0);
    }
    RegionMatrixWrapper(Base val) : Base() {
      initWithValue(val.readCell(NULL));
    }
    RegionMatrixWrapper(ElementT* data, IndexT* /*size*/) : Base() {
      initWithValue(*data);
    }
    RegionMatrixWrapper(const RegionMatrixWrapper& that) : Base() {
      initWithValue(that.readCell(NULL));
    }
    RegionMatrixWrapper(const RegionMatrixWrapper<0, MATRIX_ELEMENT_T>& that) : Base() {
      initWithValue(that.readCell(NULL));
    }

    ///
    /// Implicit conversion from ElementT/CellProxy
    RegionMatrixWrapper(ElementT value) : Base() {
      initWithValue(value);
    }
    RegionMatrixWrapper(CellProxy& value) : Base() {
      initWithValue(value);
    }
    RegionMatrixWrapper(const CellProxy& value) : Base() {
      initWithValue(value);
    }

    ///
    /// Allow implicit conversion to CellProxy
    operator CellProxy () const { return this->cell(); }

    operator MATRIX_ELEMENT_T () const { return this->readCell(NULL); }

    RegionMatrixWrapper operator=(Base val) {
      initWithValue(val.readCell(NULL));
      return *this;
    }
    RegionMatrixWrapper operator=(const RegionMatrixWrapper& val) {
      initWithValue(val.readCell(NULL));
      return *this;
    }
    RegionMatrixWrapper operator=(const RegionMatrixWrapper<0, MATRIX_ELEMENT_T>& val) {
      initWithValue(val.readCell(NULL));
      return *this;
    }

    MATRIX_ELEMENT_T readCell(const IndexT*) const {
      return _regionHandler->readCell(NULL);
    }
    void writeCell(const IndexT*, ElementT) {
      JASSERT(false);
    }

    // toLocalRegion
    bool isLocal() const {
      return true;
    }
    MatrixRegion<D, ElementT> _toLocalRegion() const {
      return MatrixRegion<D, ElementT>((ElementT)readCell(NULL));
    }

    void localCopy(RegionMatrixWrapper& scratch, bool=false) const {
      scratch.writeCell(NULL, this->readCell(NULL));
    }
    RegionMatrixWrapper localCopy(bool=false) const {
      return *this;
    }
    void fromScratchRegion(const MatrixRegion<D, ElementT>& /*scratch*/) const {
      JASSERT(false);
    }
    void fromScratchRegion(const RegionMatrixWrapper& /*scratch*/) const {
      JASSERT(false);
    }

    RegionMatrixWrapper region(const IndexT[D], const IndexT[D]) const{
      return *this;
    }

    static RegionMatrixWrapper allocate(const IndexT[D]) {
      return RegionMatrixWrapper();
    }
    static RegionMatrixWrapper allocate(const IndexT[D], int, int, int) {
      return RegionMatrixWrapper();
    }
    static RegionMatrixWrapper allocate() {
      return RegionMatrixWrapper();
    }

    size_t serialSize() {
      size_t sz = sizeof(ElementT); // value
      return sz;
    }

    void serialize(char* buf, RemoteHost& /*host*/) {
      size_t sz = sizeof(ElementT);
      *reinterpret_cast<MATRIX_ELEMENT_T*>(buf) = this->readCell(NULL);
      buf += sz;
    }

    void unserialize(const char* buf, RemoteHost& /*host*/) {
      size_t sz = sizeof(ElementT);
      initWithValue(*reinterpret_cast<const ElementT*>(buf));
      buf += sz;
    }
  };

  namespace distributed {
    typedef RegionMatrixWrapper<0, MATRIX_ELEMENT_T> MatrixRegion0D;
    typedef RegionMatrixWrapper<1, MATRIX_ELEMENT_T> MatrixRegion1D;
    typedef RegionMatrixWrapper<2, MATRIX_ELEMENT_T> MatrixRegion2D;
    typedef RegionMatrixWrapper<3, MATRIX_ELEMENT_T> MatrixRegion3D;
    typedef RegionMatrixWrapper<4, MATRIX_ELEMENT_T> MatrixRegion4D;
    typedef RegionMatrixWrapper<5, MATRIX_ELEMENT_T> MatrixRegion5D;
    typedef RegionMatrixWrapper<6, MATRIX_ELEMENT_T> MatrixRegion6D;
    typedef RegionMatrixWrapper<7, MATRIX_ELEMENT_T> MatrixRegion7D;
    typedef RegionMatrixWrapper<8, MATRIX_ELEMENT_T> MatrixRegion8D;
    typedef RegionMatrixWrapper<9, MATRIX_ELEMENT_T> MatrixRegion9D;

    typedef RegionMatrixWrapper<0, const MATRIX_ELEMENT_T> ConstMatrixRegion0D;
    typedef RegionMatrixWrapper<1, const MATRIX_ELEMENT_T> ConstMatrixRegion1D;
    typedef RegionMatrixWrapper<2, const MATRIX_ELEMENT_T> ConstMatrixRegion2D;
    typedef RegionMatrixWrapper<3, const MATRIX_ELEMENT_T> ConstMatrixRegion3D;
    typedef RegionMatrixWrapper<4, const MATRIX_ELEMENT_T> ConstMatrixRegion4D;
    typedef RegionMatrixWrapper<5, const MATRIX_ELEMENT_T> ConstMatrixRegion5D;
    typedef RegionMatrixWrapper<6, const MATRIX_ELEMENT_T> ConstMatrixRegion6D;
    typedef RegionMatrixWrapper<7, const MATRIX_ELEMENT_T> ConstMatrixRegion7D;
    typedef RegionMatrixWrapper<8, const MATRIX_ELEMENT_T> ConstMatrixRegion8D;
    typedef RegionMatrixWrapper<9, const MATRIX_ELEMENT_T> ConstMatrixRegion9D;

  }
}

#endif
