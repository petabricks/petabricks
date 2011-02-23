#ifndef PETABRICKSREGIONDATASPLIT_H
#define PETABRICKSREGIONDATASPLIT_H

#include "regiondatai.h"
#include "regiondataraw.h"

namespace petabricks {
  class RegionDataSplit;
  typedef jalib::JRef<RegionDataSplit> RegionDataSplitPtr;

  class RegionDataSplit : public RegionDataI {

  private:
    IndexT* _splitSize;
    RegionDataIPtr* _parts;
    IndexT _numParts;
    IndexT* _partsMultipliers;

  public:
    RegionDataSplit(RegionDataRawPtr originalRegionData, IndexT* splitSize);
    ~RegionDataSplit();

    int allocData();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    RegionDataIPtr coordToPart(const IndexT* coord);

    // test
    void print();
  };
}

#endif
