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

  public:
    RegionDataRaw(char* filename);
    RegionDataRaw(int dimensions, IndexT* size, ElementT* data);
    ~RegionDataRaw();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

  private:
    void init(int dimensions, IndexT* size, ElementT* data);
    ElementT* coordToPtr(const IndexT* coord);
  };
}

#endif
