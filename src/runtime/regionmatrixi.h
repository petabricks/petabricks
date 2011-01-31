#ifndef PETABRICKSREGIONMATRIXI_H
#define PETABRICKSREGIONMATRIXI_H

#include "common/jrefcounted.h"
#include "regiondatai.h"
#include "regionhandler.h"

namespace petabricks {
  class RegionMatrixI;
  typedef jalib::JRef<RegionMatrixI> RegionMatrixIPtr;
  
  class RegionMatrixI : public jalib::JRefCounted {
  protected:
    RegionHandlerPtr _regionHandler;
    RegionDataIPtr _regionData;
  
  public:
    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    virtual void acquireRegionData();
    virtual void releaseRegionData();

    RegionHandlerPtr getRegionHandler();
  };
}

#endif
