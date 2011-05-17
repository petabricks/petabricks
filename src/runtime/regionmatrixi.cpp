#include "regionmatrixi.h"

#include <stdarg.h>

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

RegionDataIPtr RegionMatrixI::acquireRegionDataConst() const {
  if (_regionData) {
    return _regionData;
  } else {
    return _regionHandler->acquireRegionData(this);
  }
}
void RegionMatrixI::releaseRegionDataConst() const {
  // only release when _regionData is not set
  if (!_regionData) {
    _regionHandler->releaseRegionData(this);
  }
}


RegionHandlerPtr RegionMatrixI::getRegionHandler() const {
  return _regionHandler;
}

CellProxy& RegionMatrixI::cell(IndexT x, ...) const {
  IndexT c1[_D];
  va_list ap;
  va_start(ap, x);
  c1[0]=x;
  for(int i=1; i<_D; ++i) c1[i]=va_arg(ap, IndexT);
  va_end(ap);
  return cell(c1);
}

CellProxy& RegionMatrixI::cell(IndexT* coord) const {
  return *(new CellProxy(_regionHandler, coord));
}

DataHostList RegionMatrixI::dataHosts() const {
  DataHostList hosts = this->acquireRegionDataConst()->hosts();
  this->releaseRegionDataConst();
  return hosts;
}

