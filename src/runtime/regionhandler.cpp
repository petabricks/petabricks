#include "regionhandler.h"

using namespace petabricks;

RegionHandler::RegionHandler(int dimensions) {
  _D = dimensions;
}

RegionHandler::RegionHandler(RegionDataIPtr regionData) {
  _regionData = regionData;
  _D = _regionData->dimensions();
}

ElementT RegionHandler::readCell(const IndexT* coord) {
  return _regionData->readCell(coord);
}

void RegionHandler::writeCell(const IndexT* coord, ElementT value) {
  _regionData->writeCell(coord, value);
}

int RegionHandler::allocData() {
  return _regionData->allocData();
}

RegionDataIPtr RegionHandler::getRegionData() {
  return _regionData;
}

void RegionHandler::updateRegionData(RegionDataIPtr regionData) {
  _regionData = regionData;
}

DataHostList RegionHandler::hosts(IndexT* begin, IndexT* end) {
  return _regionData->hosts(begin, end);
}

int RegionHandler::dimensions() {
  return _D;
}

RegionDataType RegionHandler::type() const {
  return _regionData->type();
}
