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
    RegionDataSplit(RegionDataRawPtr originalRegionData, IndexT* splitSize);

    int allocData();
    void createPart(int partIndex, RemoteHostPtr host);

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);
    DataHostList hosts(IndexT* begin, IndexT* end);

    RegionDataIPtr coordToPart(const IndexT* coord);

    // test
    void print();
  };
}

#endif
