#ifndef PETABRICKSREGIONHANDLERCACHE_H
#define PETABRICKSREGIONHANDLERCACHE_H

#include "common/jmutex.h"
#include "iregioncache.h"

#include <map>

#define REGIONDATA_CACHE_LINE_SIZE 16
#define REGIONDATA_CACHE_NUM_LINES 16

namespace petabricks {

  class RegionDataRemoteCacheLine : public jalib::JRefCounted {
  public:
    bool isValid;
    IndexT offset;
    IndexT start;
    IndexT end;
    ElementT* base;

  private:
    RegionDataRemoteCacheLine(const RegionDataRemoteCacheLine&);

  public:
    RegionDataRemoteCacheLine(size_t cacheLineSize) {
      isValid = false;
      base = new ElementT[cacheLineSize];
    }

    ~RegionDataRemoteCacheLine() {
      delete [] base;
    }
  };

  typedef jalib::JRef<RegionDataRemoteCacheLine> RegionDataRemoteCacheLinePtr;
  typedef std::map<IndexT, RegionDataRemoteCacheLinePtr> RegionDataRemoteCacheLines;

  class RegionDataRemoteCache : public IRegionCache {
  private:
    const IRegionCacheable* _regionData;
    int _dimensions;
    size_t _cacheLineSize;
    size_t _numCacheLines;
    RegionDataRemoteCacheLines _cacheLines;
    IndexT _multipliers[MAX_DIMENSIONS];

  private:
    RegionDataRemoteCache(const RegionDataRemoteCache&);

  public:
    RegionDataRemoteCache(const IRegionCacheable* regionData, int dimensions, IndexT* multipliers, size_t cacheLineSize, size_t numCacheLines);
    RegionDataRemoteCache(const IRegionCacheable* regionData, int dimensions, IndexT* multipliers);

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);
    void invalidate();

  private:
    IndexT offset(const IndexT* coord);
    RegionDataRemoteCacheLinePtr getCacheLine(IndexT key);

  };

  typedef jalib::JRef<RegionDataRemoteCache> RegionDataRemoteCachePtr;
}

#endif
