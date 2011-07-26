#ifndef PETABRICKSREGIONHANDLERCACHE_H
#define PETABRICKSREGIONHANDLERCACHE_H

#include "regiondatai.h"

#include <map>
#include <vector>

#define REGIONHANDLER_CACHE_LINE_SIZE 16
#define REGIONHANDLER_CACHE_NUM_LINES 64

namespace petabricks {

  class RegionHandlerCacheLine : public jalib::JRefCounted {
  public:
    bool isValid;
    IndexT offset;
    IndexT start;
    IndexT end;
    ElementT* base;

  private:
    RegionHandlerCacheLine(const RegionHandlerCacheLine&);

  public:
    RegionHandlerCacheLine(size_t cacheLineSize) {
      isValid = false;
      base = new ElementT[cacheLineSize];
    }

    ~RegionHandlerCacheLine() {
      delete [] base;
    }
  };

  typedef std::map<IndexT, RegionHandlerCacheLine*> RegionHandlerCacheLines;

  class RegionHandlerCache : public jalib::JRefCounted {
  private:
    RegionDataIPtr _regionData;
    size_t _cacheLineSize;
    size_t _numCacheLines;
    RegionHandlerCacheLines _cacheLines;
    IndexT _multipliers[MAX_DIMENSIONS];

  private:
    RegionHandlerCache(const RegionHandlerCache&);

  public:
    RegionHandlerCache(RegionDataIPtr regionData, size_t cacheLineSize, size_t numCacheLines);
    RegionHandlerCache(RegionDataIPtr regionData);

    void init();

    ElementT readCell(const IndexT* coord);
    void invalidate();

  private:
    IndexT offset(const IndexT* coord);
  };

  typedef jalib::JRef<RegionHandlerCache> RegionHandlerCachePtr;
}

#endif
