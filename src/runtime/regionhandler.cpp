#include "regionhandler.h"

using namespace petabricks;

RegionHandler::RegionHandler(RegionDataIPtr regionData) {
  _regionData = regionData;
  _D = _regionData->dimensions();
}

int RegionHandler::dimensions() {
  return _D;
}

RegionDataIPtr RegionHandler::acquireRegionData(const void* /*caller*/) {
  // TODO: implement list - used when move RegionData
  return _regionData;
}

void RegionHandler::releaseRegionData(const void* /*caller*/) {
  // TODO: implement list
}

void RegionHandler::updateRegionData(RegionDataIPtr regionData) {
  // TODO: update all regions using this handle
  _regionData = regionData;
}
