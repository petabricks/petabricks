#ifndef PETABRICKSREGIONDATARAW_H
#define PETABRICKSREGIONDATARAW_H

#include "matrixio.h"
#include "regiondatai.h"

namespace petabricks {
  class RegionDataRaw;
  typedef jalib::JRef<RegionDataRaw> RegionDataRawPtr;

  class RegionDataRaw : public RegionDataI {

  private:
    ElementT* _data;
    IndexT* _multipliers;
    IndexT* _partOffset;

  public:
    RegionDataRaw(char* filename);
    RegionDataRaw(int dimensions, IndexT* size);
    RegionDataRaw(int dimensions, IndexT* size, ElementT* data);
    RegionDataRaw(int dimensions, IndexT* size, IndexT* partOffset);
    ~RegionDataRaw();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);
    int allocData();

  private:
    void init(int dimensions, IndexT* size, ElementT* data, IndexT* partOffset);
    ElementT* coordToPtr(const IndexT* coord);
  };
}

#endif
