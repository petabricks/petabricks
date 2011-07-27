#ifndef PETABRICKSIREGIONCACHE_H
#define PETABRICKSIREGIONCACHE_H

#include "regiondataremotemessages.h"

namespace petabricks {

  class IRegionCache {
  public:
    virtual ~IRegionCache() {}
    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;
    virtual void invalidate() = 0;
  };

  class IRegionCacheable {
  public:
    virtual ~IRegionCacheable() {}
    virtual void readByCache(void* request, size_t request_len, void* reply, size_t &reply_len) const = 0;
    virtual void writeByCache(const IndexT* coord, ElementT value) const = 0;
  };

}

#endif
