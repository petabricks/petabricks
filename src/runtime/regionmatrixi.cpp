#include "regionmatrixi.h"

using namespace petabricks;

void RegionMatrixI::acquireRegionData() {
  // TODO: implement R/W lock
  _regionData = _regionHandler->acquireRegionData(this);
}

void RegionMatrixI::releaseRegionData() {
  // TODO: implement R/W lock
  _regionData = NULL;
  _regionHandler->releaseRegionData(this);
}

RegionHandlerPtr RegionMatrixI::getRegionHandler(){
  return _regionHandler;
}
