#ifndef PETABRICKSREGIONHANDLER_H
#define PETABRICKSREGIONHANDLER_H

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "regiondatai.h"

namespace petabricks {
  class RegionHandler;
  typedef jalib::JRef<RegionHandler> RegionHandlerPtr;

  class RegionHandler : public jalib::JRefCounted {
  private:
    int _D;
    RegionDataIPtr _regionData;

  public:
    RegionHandler(RegionDataIPtr regionData);

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    int allocData();

    RegionDataIPtr getRegionData();
    void updateRegionData(RegionDataIPtr regionData);

    DataHostList hosts(IndexT* begin, IndexT* end);

    int dimensions();
    RegionDataType type() const;
  };
}

#endif
