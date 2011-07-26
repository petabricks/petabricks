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


  _mux.lock();

  RegionDataRemoteCacheLines::iterator it = _cacheLines.find(key);
  RegionDataRemoteCacheLine* cacheLine;

  if (it == _cacheLines.end()) {
    cacheLine = new RegionDataRemoteCacheLine(_cacheLineSize);
    _cacheLines[key] = cacheLine;

  } else {
    cacheLine = it->second;
  }

  _mux.unlock();

  if (cacheLine->isValid &&
      cacheLine->offset == offset &&
      cacheLine->start <= elementOffset &&
      cacheLine->end >= elementOffset) {
    return *(cacheLine->base + elementOffset);

  } else {
    ElementT x;
    _regionData->readByCache(coord, &x);
    cacheLine->isValid = true;
    cacheLine->offset = offset;
    cacheLine->start = 0;
    cacheLine->end = 0;
    *(cacheLine->base) = x;
    return *(cacheLine->base + elementOffset);
  }
}

void RegionDataRemoteCache::writeCell(const IndexT* coord, ElementT value) {
  _regionData->writeByCache(coord, value);

  static const IndexT pageSize = _cacheLineSize * _numCacheLines;

  IndexT coordOffset = offset(coord);
  IndexT pageOffset = coordOffset % pageSize;
  IndexT offset = coordOffset - pageOffset;
  IndexT elementOffset = pageOffset % _cacheLineSize;
  IndexT key = pageOffset - elementOffset;

  _mux.lock();
  RegionDataRemoteCacheLines::iterator it = _cacheLines.find(key);

  if (it != _cacheLines.end()) {
    RegionDataRemoteCacheLine* cacheLine = it->second;

    if (cacheLine->isValid &&
        cacheLine->offset == offset &&
        cacheLine->start <= elementOffset &&
        cacheLine->end >= elementOffset) {
      *(cacheLine->base + elementOffset) = value;
    }
  }
  _mux.unlock();
}

void RegionDataRemoteCache::invalidate() {
  _mux.lock();
  _cacheLines = RegionDataRemoteCacheLines();
  _mux.unlock();
}

IndexT RegionDataRemoteCache::offset(const IndexT* coord) {
  IndexT offset = 0;
  for(int i = 0; i < _dimensions; i++){
    offset += _multipliers[i] * coord[i];
  }
  return offset;
}
