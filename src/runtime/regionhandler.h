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
    
    RegionDataIPtr acquireRegionData(const void* caller);
    void releaseRegionData(const void* caller);

    void updateRegionData(RegionDataIPtr regionData);

    int dimensions();
  };
}

#endif
