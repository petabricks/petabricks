#include "regiondataremotecache.h"

using namespace petabricks;

RegionDataRemoteCache::RegionDataRemoteCache(IRegionCacheable* regionData, int dimensions, IndexT* multipliers, size_t cacheLineSize, size_t numCacheLines) {
  _regionData = regionData;
  _dimensions = dimensions;
  memcpy(_multipliers, multipliers, sizeof(IndexT) * dimensions);
  _cacheLineSize = cacheLineSize;
  _numCacheLines = numCacheLines;
}

RegionDataRemoteCache::RegionDataRemoteCache(IRegionCacheable* regionData, int dimensions, IndexT* multipliers) {
  _regionData = regionData;
  _dimensions = dimensions;
  memcpy(_multipliers, multipliers, sizeof(IndexT) * dimensions);
  _cacheLineSize = REGIONDATA_CACHE_LINE_SIZE;
  _numCacheLines = REGIONDATA_CACHE_NUM_LINES;
}

ElementT RegionDataRemoteCache::readCell(const IndexT* coord) {
  static const IndexT pageSize = _cacheLineSize * _numCacheLines;

  IndexT coordOffset = offset(coord);
  IndexT pageOffset = coordOffset % pageSize;
  IndexT offset = coordOffset - pageOffset;
  IndexT elementOffset = pageOffset % _cacheLineSize;
  IndexT key = pageOffset - elementOffset;


  RegionDataRemoteCacheLines::iterator it = _cacheLines.find(key);
  RegionDataRemoteCacheLine* cacheLine;

  if (it == _cacheLines.end()) {
    cacheLine = new RegionDataRemoteCacheLine(_cacheLineSize);
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
    ElementT x;
    _regionData->readToCache(coord, &x);
    return x;
  }
}

void RegionDataRemoteCache::invalidate() {
  _cacheLines = RegionDataRemoteCacheLines();
}

IndexT RegionDataRemoteCache::offset(const IndexT* coord) {
  IndexT offset = 0;
  for(int i = 0; i < _dimensions; i++){
    offset += _multipliers[i] * coord[i];
  }
  return offset;
}
