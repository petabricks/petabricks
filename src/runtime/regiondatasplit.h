#ifndef PETABRICKSREGIONDATASPLIT_H
#define PETABRICKSREGIONDATASPLIT_H

#include "regiondatai.h"
#include "regiondataraw.h"
#include "remotehost.h"

#include <vector>

namespace petabricks {
  typedef std::vector<RegionDataIPtr> PartsList;

  class RegionDataSplit;
  typedef jalib::JRef<RegionDataSplit> RegionDataSplitPtr;

  class RegionDataSplit : public RegionDataI {

  private:
    IndexT _splitSize[MAX_DIMENSIONS];
    PartsList _parts;
    IndexT _partsSize[MAX_DIMENSIONS];
    IndexT _numParts;
    IndexT _partsMultipliers[MAX_DIMENSIONS];

  public:
    RegionDataSplit(int dimensions, IndexT* sizes, IndexT* splitSize);
    void init(int dimensions, IndexT* sizes, IndexT* splitSize);

    int allocData();
    void createPart(int partIndex, RemoteHostPtr host);

    ElementT readCell(const IndexT* coord) const;
    void writeCell(const IndexT* coord, ElementT value);

    void copyToScratchMatrixStorage(CopyToMatrixStorageMessage* origMetadata, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata=0, const IndexT* scratchStorageSize=0) const;
    void copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* origMetadata, size_t len);

    DataHostPidList hosts(const IndexT* begin, const IndexT* end) const;
    RemoteHostPtr host() { UNIMPLEMENTED(); return NULL; }

    RegionDataIPtr coordToPart(const IndexT* coord, IndexT* coordPart) const;
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
