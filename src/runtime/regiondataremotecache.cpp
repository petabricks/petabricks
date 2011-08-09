#include "regiondataremotecache.h"

#include "regiondataremotemessages.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRemoteCache::RegionDataRemoteCache(const IRegionCacheable* regionData, int dimensions, IndexT* multipliers, size_t cacheLineSize, size_t numCacheLines) {
  _regionData = regionData;
  _dimensions = dimensions;
  memcpy(_multipliers, multipliers, sizeof(IndexT) * dimensions);
  _cacheLineSize = cacheLineSize;
  _numCacheLines = numCacheLines;
}

RegionDataRemoteCache::RegionDataRemoteCache(const IRegionCacheable* regionData, int dimensions, IndexT* multipliers) {
  _regionData = regionData;
  _dimensions = dimensions;
  memcpy(_multipliers, multipliers, sizeof(IndexT) * dimensions);
  _cacheLineSize = REGIONDATA_CACHE_LINE_SIZE;
  _numCacheLines = REGIONDATA_CACHE_NUM_LINES;
}

ElementT RegionDataRemoteCache::readCell(const IndexT* coord) {
  const IndexT pageSize = _cacheLineSize * _numCacheLines;

  IndexT coordOffset = offset(coord);
  IndexT pageOffset = coordOffset % pageSize;
  IndexT offset = coordOffset - pageOffset;
  IndexT elementOffset = pageOffset % _cacheLineSize;
  IndexT key = pageOffset - elementOffset;

  RegionDataRemoteCacheLinePtr cacheLine = getCacheLine(key);

  if (cacheLine->isValid &&
      cacheLine->offset == offset &&
      cacheLine->start <= elementOffset &&
      cacheLine->end >= elementOffset) {
    ElementT v = *(cacheLine->base + elementOffset);
    return v;
  }

  // Construct request message
  size_t coord_sz = _dimensions * sizeof(IndexT);
  size_t msg_len = sizeof(ReadCellCacheMessage) + coord_sz;

  char buf[msg_len];
  ReadCellCacheMessage* msg = (ReadCellCacheMessage*)buf;
  size_t reply_len;
  msg->cacheLineSize = _cacheLineSize;
  memcpy(msg->coord, coord, coord_sz);
  _regionData->readByCache(msg, msg_len, cacheLine.asPtr(), reply_len);

  cacheLine->isValid = true;
  cacheLine->offset = offset;
  ElementT x = *(cacheLine->base + elementOffset);

  return x;
}

void RegionDataRemoteCache::writeCell(const IndexT* coord, ElementT value) {
  const IndexT pageSize = _cacheLineSize * _numCacheLines;

  IndexT coordOffset = offset(coord);
  IndexT pageOffset = coordOffset % pageSize;
  IndexT offset = coordOffset - pageOffset;
  IndexT elementOffset = pageOffset % _cacheLineSize;
  IndexT key = pageOffset - elementOffset;

  RegionDataRemoteCacheLinePtr cacheLine = getCacheLine(key);
  if (cacheLine->isValid &&
      cacheLine->offset == offset &&
      cacheLine->start <= elementOffset &&
      cacheLine->end >= elementOffset) {
    *(cacheLine->base + elementOffset) = value;
  }

  _regionData->writeByCache(coord, value);
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

RegionDataRemoteCacheLinePtr RegionDataRemoteCache::getCacheLine(IndexT key) {
  RegionDataRemoteCacheLinePtr cacheLine;
  RegionDataRemoteCacheLines::iterator it = _cacheLines.find(key);
  if (it != _cacheLines.end()) {
    cacheLine = it->second;
  } else {
    cacheLine = new RegionDataRemoteCacheLine(_cacheLineSize);
    _cacheLines[key] = cacheLine;
  }
  return cacheLine;
}
