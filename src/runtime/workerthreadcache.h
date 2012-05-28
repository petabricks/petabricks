#ifndef PETABRICKSWORKERTHREADCACHE_H
#define PETABRICKSWORKERTHREADCACHE_H

#include "iregioncache.h"

#include <map>

namespace petabricks {

  typedef std::map<const IRegionCacheable*, IRegionCachePtr> WorkerThreadCacheMap;

  //
  // per thread cache.
  //

  class WorkerThreadCache : public jalib::JRefCounted {
  private:
    WorkerThreadCache(const WorkerThreadCache&);

  public:
    WorkerThreadCache() {}

    IRegionCachePtr get(const IRegionCacheable* cacheable) {
      // does not need to lock

      WorkerThreadCacheMap::iterator it = _map.find(cacheable);
      if (it != _map.end()) {
        return it->second;
      } else {
        IRegionCachePtr cache = cacheable->cacheGenerator();
        _map[cacheable] = cache;
        return cache;
      }
    }

    void invalidate() {
      _map = WorkerThreadCacheMap();
    }

  private:
    WorkerThreadCacheMap _map;
  };

  typedef jalib::JRef<WorkerThreadCache> WorkerThreadCachePtr;
}


#endif
