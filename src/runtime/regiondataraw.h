#ifndef PETABRICKSREGIONDATARAW_H
#define PETABRICKSREGIONDATARAW_H

#include "matrixio.h"
#include "matrixstorage.h"
#include "regiondatai.h"

namespace petabricks {
  class RegionDataRaw;
  typedef jalib::JRef<RegionDataRaw> RegionDataRawPtr;

  class RegionDataRaw : public RegionDataI {

  private:
    MatrixStoragePtr _storage;
    IndexT* _multipliers;
    bool _isPart;
    IndexT* _partOffset;

  public:
    RegionDataRaw(const char* filename);
    RegionDataRaw(int dimensions, IndexT* size);
    RegionDataRaw(int dimensions, IndexT* size, ElementT* data);
    RegionDataRaw(int dimensions, IndexT* size, IndexT* partOffset);
    ~RegionDataRaw();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);
    int allocData();

    MatrixStoragePtr storage() const {return _storage;}

  private:
    void init(int dimensions, IndexT* size, ElementT* data, IndexT* partOffset);
    ElementT* coordToPtr(const IndexT* coord);
  };
}

#endif
