#ifndef PETABRICKSIREGIONCACHE_H
#define PETABRICKSIREGIONCACHE_H

#include "regiondataremotemessages.h"

namespace petabricks {

  class IRegionCache {
  public:
    virtual ~IRegionCache() {}
    virtual ElementT readCell(const IndexT* coord) = 0;
  };

  class IRegionCacheable {
  public:
    virtual ~IRegionCacheable() {}
    virtual void readToCache(const IndexT* coord, void* msg) const = 0;
  };

}

#endif
