#include "regionhandlercache.h"

using namespace petabricks;

RegionHandlerCache::RegionHandlerCache(RegionDataIPtr regionData, size_t cacheLineSize, size_t numCacheLines) {
  _regionData = regionData;
  _cacheLineSize = cacheLineSize;
  _numCacheLines = numCacheLines;
  init();
}

RegionHandlerCache::RegionHandlerCache(RegionDataIPtr regionData) {
  _regionData = regionData;
  _cacheLineSize = REGIONHANDLER_CACHE_LINE_SIZE;
  _numCacheLines = REGIONHANDLER_CACHE_NUM_LINES;
  init();
}

void RegionHandlerCache::init() {
  _multipliers[0] = 1;
  for (int i = 1; i < _regionData->dimensions(); i++) {
    _multipliers[i] = _multipliers[i - 1] * _regionData->size()[i - 1];
  }
}

ElementT RegionHandlerCache::readCell(const IndexT* coord) {
  static const IndexT pageSize = _cacheLineSize * _numCacheLines;

  IndexT coordOffset = offset(coord);
  IndexT pageOffset = coordOffset % pageSize;
  IndexT offset = coordOffset - pageOffset;
  IndexT elementOffset = pageOffset % _cacheLineSize;
  IndexT key = pageOffset - elementOffset;


  RegionHandlerCacheLines::iterator it = _cacheLines.find(key);
  RegionHandlerCacheLine* cacheLine;

  if (it == _cacheLines.end()) {
    cacheLine = new RegionHandlerCacheLine(_cacheLineSize);
    _cacheLines[key] = cacheLine;

  } else {
    cacheLine = it->second;
  }

  if (cacheLine->isValid &&
      cacheLine->offset == offset &&
      cacheLine->start <= elementOffset &&
      cacheLine->end >= elementOffset) {
    return *(cacheLine->base + elementOffset);

  } else {
    return _regionData->readCell(coord);
  }
}

void RegionHandlerCache::invalidate() {
  _cacheLines = RegionHandlerCacheLines();
}

IndexT RegionHandlerCache::offset(const IndexT* coord) {
  IndexT offset = 0;
  for(int i = 0; i < _regionData->dimensions(); i++){
    offset += _multipliers[i] * coord[i];
  }
  return offset;
}
