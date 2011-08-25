#ifndef PETABRICKSREGIONDATARAW_H
#define PETABRICKSREGIONDATARAW_H

#include "matrixstorage.h"
#include "regiondatai.h"

namespace petabricks {
  class RegionDataRaw;
  typedef jalib::JRef<RegionDataRaw> RegionDataRawPtr;

  class RegionDataRaw : public RegionDataI {

  private:
    MatrixStoragePtr _storage;
    IndexT _multipliers[MAX_DIMENSIONS];
    bool _isPart;
    IndexT _partOffset[MAX_DIMENSIONS];

  public:
    RegionDataRaw(const char* filename);
    RegionDataRaw(const int dimensions, const IndexT* size);
    RegionDataRaw(const int dimensions, const IndexT* size, const ElementT* data);
    RegionDataRaw(const int dimensions, const IndexT* size, const IndexT* partOffset);

    ElementT readCell(const IndexT* coord) const;
    void writeCell(const IndexT* coord, ElementT value);
    int allocData();

    MatrixStoragePtr storage() const {return _storage;}
    ElementT& value0D(const IndexT* coord) const;

    DataHostPidList hosts(IndexT* begin, IndexT* end);
    RemoteHostPtr host();

    void processReadCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processWriteCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);

  private:
    void init(const int dimensions, const IndexT* size, const ElementT* data, const IndexT* partOffset);
    ElementT* coordToPtr(const IndexT* coord) const;
    IndexT coordOffset(const IndexT* coord) const;
  };
}

#endif
