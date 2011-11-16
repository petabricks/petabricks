#ifndef PETABRICKSIREGIONCACHE_H
#define PETABRICKSIREGIONCACHE_H

#include "common/jrefcounted.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  class IRegionCache : public jalib::JRefCounted {
  public:
    virtual ~IRegionCache() {}
    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;
    virtual void invalidate() = 0;
  };

  typedef jalib::JRef<IRegionCache> IRegionCachePtr;

  class IRegionCacheable {
  public:
    virtual ~IRegionCacheable() {}
    virtual IRegionCachePtr cacheGenerator() const = 0;
    virtual void readByCache(void* request, size_t request_len, void* reply, size_t &reply_len) const = 0;
    virtual void writeByCache(const IndexT* coord, ElementT value) const = 0;
  };
}

#endif
