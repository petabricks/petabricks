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
#include "regiondataremote.h"
#include "regiondatasplit.h"
#include "regionhandler.h"
#include "regionmatrixproxy.h"

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
  template< int D, typename ElementT>
  class RegionMatrix {
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

      if (size != NULL) {
        memcpy(_size, size, sizeof_sizes);
      } else {
        memset(_size, -1, sizeof_sizes);
      }

      if (splitOffset != NULL) {
        memcpy(_splitOffset, splitOffset, sizeof_splitOffset);
      } else {
        memset(_splitOffset, 0, sizeof_splitOffset);
      }

      _isTransposed = isTransposed;
      _sliceInfo = sliceInfo;
      _regionHandler = handler;
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

    //
    // Constructors
    //
    RegionMatrix() {
      init(NULL, NULL);
    }

    RegionMatrix(const IndexT* size) {
      RegionDataIPtr regionData = new RegionDataRaw(D, size);
      init(size, new RegionHandler(regionData));
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
    RegionHandlerPtr getRegionHandler() const { return _regionHandler; };

    //
    // Initialization
    //
    void splitData(IndexT* splitSize) {
      JASSERT(_regionHandler->type() == RegionDataTypes::REGIONDATARAW);
      RegionDataIPtr newRegionData =
        new RegionDataSplit((RegionDataRaw*)_regionHandler->getRegionData().asPtr(), splitSize);
      _regionHandler->updateRegionData(newRegionData);
    }

    void createDataPart(int partIndex, RemoteHostPtr host) {
      JASSERT(_regionHandler->type() == RegionDataTypes::REGIONDATASPLIT);
      ((RegionDataSplit*)_regionHandler->getRegionData().asPtr())->createPart(partIndex, host);
    }

    void allocData() {
      _regionHandler->allocData();
    }

    static RegionMatrix allocate(IndexT* size) {
      RegionMatrix region = RegionMatrix(size);
      region.allocData();
      return region;
    }

    static RegionMatrix allocate(IndexT x, ...) {
      IndexT c1[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      va_end(ap);
      return allocate(c1);
    }

    inline static RegionMatrix allocate() {
      IndexT c1[D];
      return allocate(c1);
    }

    //
    // Read & Write
    //
    MATRIX_ELEMENT_T readCell(const IndexT* coord) {
      IndexT rd_coord[_regionHandler->dimensions()];
      this->getRegionDataCoord(coord, rd_coord);
      return _regionHandler->readCell(rd_coord);
    }

    void writeCell(const IndexT* coord, ElementT value) {
      IndexT rd_coord[_regionHandler->dimensions()];
      this->getRegionDataCoord(coord, rd_coord);
      _regionHandler->writeCell(rd_coord, value);
    }

    const IndexT* size() const { return _size; }
    IndexT size(int i) const { return _size[i]; }
    bool isSize(const IndexT size[D]) const{
      if (!_size) {
        return false;
      }
      for(int i=0; i<D; ++i){
        if(_size[i] != size[i]){
          return false;
        }
      }
      return true;
    }
    bool isSize(IndexT x, ...) const{
      IndexT c1[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      va_end(ap);
      return isSize(c1);
    }
    bool isSize() const { return true; }

    IndexT width() const { return size(0); }
    IndexT height() const { return size(1); }
    IndexT depth() const { return size(2); }

    bool contains(const IndexT* coord) const {
      for(int i=0; i<D; ++i)
        if(coord[i]<0 || coord[i]>=size(i))
          return false;
      return true;
    }
    bool contains(IndexT x, ...) const {
      IndexT c1[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      va_end(ap);
      return contains(c1);
    }

    /// Number of elements in this region
    ssize_t count() const {
      ssize_t s=1;
      for(int i=0; i<D; ++i)
        s*=this->size()[i];
      return s;
    }

    CellProxy& cell(IndexT x, ...) const {
      IndexT c1[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      va_end(ap);
      return cell(c1);
    }
    CellProxy& cell(IndexT* coord) const {
      IndexT rd_coord[_regionHandler->dimensions()];
      getRegionDataCoord(coord, rd_coord);
      return *(new CellProxy(_regionHandler, rd_coord));
    }
    INLINE CellProxy& cell() const {
      IndexT c1[0];
      return this->cell(c1);
    }

    //
    // Matrix manipulation
    //
    RegionMatrix<D, ElementT> splitRegion(const IndexT* offset, const IndexT* size) const {
      IndexT offset_new[_regionHandler->dimensions()];
      this->getRegionDataCoord(offset, offset_new);

      return RegionMatrix<D, ElementT>
        (size, offset_new, _isTransposed, _sliceInfo, _regionHandler);
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

    RegionMatrixWrapper<D, ElementT> region(IndexT x, ...) const{
      IndexT c1[D], c2[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      for(int i=0; i<D; ++i) c2[i]=va_arg(ap, IndexT);
      va_end(ap);
      return region(c1,c2);
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
    EncodedPtr moveToRemoteHost(RemoteHostPtr host) {
      RegionMatrixProxyPtr proxy =
        new RegionMatrixProxy(this->getRegionHandler());
      RegionMatrixProxyRemoteObjectPtr local = proxy->genLocal();

      // InitialMsg
      RegionDataRemoteMessage::InitialMessage msg = RegionDataRemoteMessage::InitialMessage();
      msg.dimensions = _regionHandler->dimensions();
      memcpy(msg.size, _regionHandler->size(), sizeof(msg.size));
      int len = sizeof(RegionDataRemoteMessage::InitialMessage);

      host->createRemoteObject(local.asPtr(), &RegionDataRemote::genRemote, &msg, len);
      local->waitUntilCreated();
      return local->remoteObjPtr();
    }

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
      sz += sizeof(EncodedPtr);                   // remoteObj
      return sz;
    }

    void serialize(char* buf, RemoteHost& host) {
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

      EncodedPtr remoteObj = moveToRemoteHost(&host);
      sz = sizeof(EncodedPtr);
      *reinterpret_cast<EncodedPtr*>(buf) = remoteObj;
      buf += sz;
    }

    void unserialize(const char* buf, RemoteHost& /*host*/) {
      // (yod) destruct old data
      size_t sz = sizeof(int);
      JASSERT(*reinterpret_cast<const int*>(buf) == D)(*reinterpret_cast<const int*>(buf))(D).Text("RegionMatrix dimension mismatch.");
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

      sz = sizeof(EncodedPtr);
      EncodedPtr remoteObjPtr = *reinterpret_cast<const EncodedPtr*>(buf);
      RegionDataRemoteObject* remoteObj = reinterpret_cast<RegionDataRemoteObject*>(remoteObjPtr);
      _regionHandler = new RegionHandler(remoteObj->regionData());
      buf += sz;
    }

    void updateHandlerChain() {
      RegionDataIPtr regionData = _regionHandler->getRegionData();
      if (regionData->type() == RegionDataTypes::REGIONDATAREMOTE) {
        RegionDataRemoteMessage::UpdateHandlerChainReplyMessage* reply =
          ((RegionDataRemote*)regionData.asPtr())->updateHandlerChain();
        JTRACE("updatehandler")(reply->dataHost)(reply->numHops)(reply->regionData.asPtr());

        if (reply->dataHost == HostPid::self()) {
          // Data is in the same process. Update handler to point directly to the data.
          _regionHandler->updateRegionData(reply->regionData);
        } else if (reply->numHops > 1) {
          // Multiple network hops to data. Create a direct connection to data.

          // (yod) TODO:
          //this->updateHandler(999);
        }
      }
    }

    DataHostList dataHosts() const {
      IndexT begin[D];
      IndexT end[D];

      memset(begin, 0, sizeof(IndexT) * D);
      for (int i = 0; i < D; i++) {
        end[i] = size(i) - 1;
      }

      IndexT rd_begin[_regionHandler->dimensions()];
      IndexT rd_end[_regionHandler->dimensions()];
      this->getRegionDataCoord(begin, rd_begin);
      this->getRegionDataCoord(end, rd_end);

      return _regionHandler->hosts(rd_begin, rd_end);
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

    bool isLocal() const {
      return _regionHandler->type() == RegionDataTypes::REGIONDATARAW;
    }
    MatrixRegion<D, const ElementT> _toLocalConstRegion() const {
      return _toLocalRegion();
    }
    MatrixRegion<D, ElementT> _toLocalRegion() const {
      JASSERT(!_isTransposed).Text("(yod) to be implemented");

      RegionDataIPtr regionData = _regionHandler->getRegionData();
      JASSERT(regionData->type() == RegionDataTypes::REGIONDATARAW).Text("Cannot cast to MatrixRegion.");

      IndexT startOffset = 0;
      IndexT multipliers[D];

      IndexT mult = 1;
      int last_slice_index = 0;
      for(int i = 0; i < regionData->dimensions(); i++){
        if ((last_slice_index < _sliceInfo->numSliceDimensions()) &&
            (i == _sliceInfo->sliceDimensions(last_slice_index))) {
          startOffset += mult * _sliceInfo->slicePositions(last_slice_index);
          last_slice_index++;
        } else {
          multipliers[i - last_slice_index] = mult;

          if (_splitOffset) {
            startOffset += mult * _splitOffset[i - last_slice_index];
          }
        }

        mult *= regionData->size()[i];
      }

      MatrixRegion<D, ElementT> matrixRegion =
        MatrixRegion<D, ElementT>(regionData->storage(), regionData->storage()->data() + startOffset, _size, multipliers);

      if (_isTransposed) {
        matrixRegion = matrixRegion.transposed();
      }

      return matrixRegion;
    }

    ///
    /// Copy the entire matrix and store it locally
    RegionMatrix localCopy() {
      RegionMatrix copy = RegionMatrix(this->size());
      copy.allocData();

      IndexT coord[D];
      memset(coord, 0, sizeof coord);

      do {
        copy.writeCell(coord, this->readCell(coord));
      } while (this->incCoord(coord) >= 0);

      return copy;
    }

    void randomize() {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        this->writeCell(coord, MatrixStorage::rand());
      } while (this->incCoord(coord) >= 0);
    }

    void hash(jalib::HashGenerator& gen) {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        ElementT v = this->readCell(coord);
        gen.update(&v, sizeof(ElementT));
      } while (this->incCoord(coord) >= 0);
    }


    int incCoord(IndexT* coord) const {
      if (D == 0) {
        return -1;
      }

      coord[0]++;
      for (int i = 0; i < D - 1; ++i){
        if (coord[i] >= _size[i]){
          coord[i]=0;
          coord[i+1]++;
        } else{
          return i;
        }
      }
      if (coord[D - 1] >= _size[D - 1]){
        return -1;
      }else{
        return D - 1;
      }
    }

  private:
    void getRegionDataCoord(const IndexT* coord_orig, IndexT* coord_new) const {
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
          int offset = 0;
          if (_splitOffset) {
            offset = _splitOffset[split_index];
          }

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
    RegionMatrixWrapper(IndexT* size) : Base(size) {}

    RegionMatrixWrapper(ElementT* data, IndexT* size) : Base(size) {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      Base::_regionHandler->allocData();

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
      this->allocData();
      IndexT coord[D];
      memset(coord, 0, sizeof(IndexT) * D);

      while (true) {
        this->writeCell(coord, in.readCell(coord));

        int z = this->incCoord(coord);
        if (z == -1) {
          break;
        }
      }
    }
  };


  template<typename ElementT>
    class RegionMatrixWrapper<0, ElementT> : public RegionMatrix<0, ElementT> {
  private:
    int _sourceDimension;
    IndexT* _sourceIndex;

  public:
    enum { D = 0 };
    typedef RegionMatrix<D, ElementT> Base;

    RegionMatrixWrapper() : Base() {
      _sourceDimension = 0;
      Base::_regionHandler = new RegionHandler(new RegionData0D());
    }

    RegionMatrixWrapper(Base val) : Base() {
      _sourceDimension = 0;
      Base::_regionHandler = val.getRegionHandler();
    }

    RegionMatrixWrapper(ElementT* data, IndexT* size) : Base() {
     _sourceDimension = 0;
     Base::_regionHandler = new RegionHandler(new RegionData0D(*data));
    }

    RegionMatrixWrapper(const RegionMatrixWrapper& that) : Base() {
      Base::_regionHandler = that.getRegionHandler();

      _sourceDimension = that._sourceDimension;
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, that._sourceIndex, sizeof(IndexT) * _sourceDimension);
    }

    ///
    /// Implicit conversion from ElementT/CellProxy
    RegionMatrixWrapper(ElementT& value) : Base() {
      _sourceDimension = 0;
      Base::_regionHandler = new RegionHandler(new RegionData0D(value));
    }
    RegionMatrixWrapper(CellProxy& value) : Base() {
      Base::_regionHandler = value._handler;

      _sourceDimension = value._handler->dimensions();
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, value._index, sizeof(IndexT) * _sourceDimension);
    }
    RegionMatrixWrapper(const CellProxy& value) : Base() {
      Base::_regionHandler = value._handler;

      _sourceDimension = value._handler->dimensions();
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, value._index, sizeof(IndexT) * _sourceDimension);
    }

    ///
    /// Allow implicit conversion to CellProxy
    operator CellProxy& () const { return this->cell(); }

    RegionMatrixWrapper operator=(Base val) {
      this->cell() = val.readCell(NULL);
      return *this;
    }

    CellProxy& cell(IndexT x, ...) const {
      return cell();
    }
    CellProxy& cell(IndexT* coord) const {
      return cell();
    }
    INLINE CellProxy& cell() const {
      return Base::cell(_sourceIndex);
    }
  };

  // Specialized for ConstMatrixRegion0D.
  template<>
  class RegionMatrixWrapper<0, const MATRIX_ELEMENT_T> : public RegionMatrix<0, MATRIX_ELEMENT_T> {
  public:
    enum { D = 0 };
    typedef RegionMatrix<D, ElementT> Base;

    INLINE void initWithValue(ElementT value) {
      Base::_regionHandler = new RegionHandler(new ConstRegionData0D(value));
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
      initWithValue(that.cell());
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
    operator CellProxy& () const { return this->cell(); }

    RegionMatrixWrapper operator=(Base val) {
      initWithValue(val.readCell(NULL));
      return *this;
    }
  };

  namespace distributed {
    typedef RegionMatrixWrapper<0, MATRIX_ELEMENT_T> MatrixRegion0D;
    typedef RegionMatrixWrapper<1, MATRIX_ELEMENT_T> MatrixRegion1D;
    typedef RegionMatrixWrapper<2, MATRIX_ELEMENT_T> MatrixRegion2D;
    typedef RegionMatrixWrapper<3, MATRIX_ELEMENT_T> MatrixRegion3D;

    typedef RegionMatrixWrapper<0, const MATRIX_ELEMENT_T> ConstMatrixRegion0D;
    typedef RegionMatrixWrapper<1, const MATRIX_ELEMENT_T> ConstMatrixRegion1D;
    typedef RegionMatrixWrapper<2, const MATRIX_ELEMENT_T> ConstMatrixRegion2D;
    typedef RegionMatrixWrapper<3, const MATRIX_ELEMENT_T> ConstMatrixRegion3D;
  }
}

#endif
