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

RegionHandlerPtr RegionMatrixI::getRegionHandler() const {
  return _regionHandler;
}

CellProxy& RegionMatrixI::cell(IndexT x, IndexT y){
  return *(new CellProxy(this, x, y));
}

CellProxy& RegionMatrixI::cell(IndexT* coord){
  return *(new CellProxy(this, coord[0], coord[1]));
}


