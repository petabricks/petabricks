#ifndef PETABRICKSREGIONDATAI_H
#define PETABRICKSREGIONDATAI_H

#include "common/jrefcounted.h"
#include "iregionreplyproxy.h"
#include "matrixstorage.h"
#include "regiondataremotemessages.h"

namespace petabricks {

  using namespace petabricks::RegionDataRemoteMessage;

  //
  // RegiondataI
  //

  class RegionDataI;
  typedef jalib::JRef<RegionDataI> RegionDataIPtr;

  class RegionDataI {
  protected:
    int _D;
    RegionDataType _type;

    // _size is the size of this part, not the entire region
    IndexT _size[MAX_DIMENSIONS];

  public:
    virtual long refCount() const = 0;
    virtual void incRefCount() const = 0;
    virtual void decRefCount() const = 0;

    virtual int allocData() = 0;

    virtual ElementT readCell(const IndexT* coord) const = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;
    virtual void invalidateCache() {}

    virtual MatrixStoragePtr storage() const {
      JASSERT(false)(_type).Text("This should not be called.");
      return NULL;
    }

    virtual RegionDataIPtr copyToScratchMatrixStorage(CopyToMatrixStorageMessage* /*origMetadata*/, size_t /*len*/, MatrixStoragePtr /*scratchStorage*/, RegionMatrixMetadata* /*scratchMetadata*/, const IndexT* /*scratchStorageSize*/, RegionDataI** /*newScratchRegionData*/) = 0;
    virtual void copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* /*origMetadata*/, size_t /*len*/, MatrixStoragePtr /*scratchStorage*/, RegionMatrixMetadata* /*scratchMetadata*/, const IndexT* /*scratchStorageSize*/) = 0;

    virtual void setStorage(MatrixStoragePtr /*storage*/) {
      JASSERT(false)(_type).Text("This should not be called.");
    }

    virtual void randomize() {
      this->storage()->randomize();
    }

    // for toLocalRegion
    virtual ElementT& value0D(const IndexT* /*coord*/) const {
      JASSERT(false)(_type).Text("This should not be called.");
      throw;
    }

    int dimensions();
    IndexT* size();

    RegionDataType type() const {
      return _type;
    }

    virtual RegionDataIPtr hosts(const IndexT* begin, const IndexT* end, DataHostPidList& list) = 0;
    virtual RemoteHostPtr dataHost() = 0;

    // Process Remote Messages
    virtual void processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processReadCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processWriteCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processCopyFromMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processCopyToMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processRandomizeDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    virtual void processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller, EncodedPtr regionHandlerPtr);
    virtual void processCopyRegionDataSplitMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);

    //  Coordinate helpers
    static int incCoord(int dimensions, IndexT* size, IndexT* coord);
    static void toRegionDataCoord(int dimensions, const IndexT* coord, int numSliceDimensions, const IndexT* splitOffset, const int* sliceDimensions, const IndexT* slicePositions, IndexT* newCoord);
    static IndexT coordToOffset(int dimensions, const IndexT* coord, const IndexT* multipliers);
    static void sizeToMultipliers(int dimensions, const IndexT* size, IndexT* multipliers);
    static IndexT toRegionDataIndex(int dimensions, const IndexT* coord, int numSliceDimensions, const IndexT* splitOffset, const int* sliceDimensions, const IndexT* slicePositions, const IndexT* multipliers);

    // for tests
    virtual void print();
  };
}

#endif
