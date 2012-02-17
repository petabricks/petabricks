#ifndef PETABRICKSREGIONDATASPLIT_H
#define PETABRICKSREGIONDATASPLIT_H

#include "regiondatai.h"
#include "regiondataraw.h"
#include "regiondataremotemessages.h"
#include "regionhandler.h"
#include "remotehost.h"

#include <vector>

namespace petabricks {
  typedef std::vector<RegionHandlerPtr> PartsList;

  class RegionDataSplit;
  typedef jalib::JRef<RegionDataSplit> RegionDataSplitPtr;

  class RegionDataSplit : public RegionDataI, public jalib::JRefCounted {

  private:
    IndexT _splitSize[MAX_DIMENSIONS];
    PartsList _parts;
    IndexT _partsSize[MAX_DIMENSIONS];
    IndexT _numParts;
    IndexT _partsMultipliers[MAX_DIMENSIONS];

  public:
    RegionDataSplit(int dimensions, const IndexT* sizes, const IndexT* splitSize);
    void init(int dimensions, const IndexT* sizes, const IndexT* splitSize);

    long refCount() const { return jalib::JRefCounted::refCount(); }
    void incRefCount() const { jalib::JRefCounted::incRefCount(); }
    void decRefCount() const { jalib::JRefCounted::decRefCount(); }

    int allocData();
    void createPart(int partIndex, RemoteHostPtr host);
    void setPart(int partIndex, const RemoteRegionHandler& remoteRegionHandler);
    IndexT numParts() const { return _numParts; };

    void randomize();

    ElementT readCell(const IndexT* coord) const;
    void writeCell(const IndexT* coord, ElementT value);

    void copyHelper(bool isCopyTo, RegionMatrixMetadata* origMetadata, MatrixStoragePtr scratchStorage) const;
    RegionDataIPtr copyToScratchMatrixStorage(CopyToMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize) const;
    void copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize);

    DataHostPidList hosts(const IndexT* begin, const IndexT* end) const;
    RemoteHostPtr dataHost() { return NULL; }

    RegionHandlerPtr coordToPart(const IndexT* coord, IndexT* coordPart) const;
    int incPartCoord(IndexT* coord, const IndexT* begin, const IndexT* end) const;

    void processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processCopyFromMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processCopyToMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);

    // test
    void print();
  };
}

#endif
