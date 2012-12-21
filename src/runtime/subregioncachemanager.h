#ifndef SUBREGIONCACHEMANAGER_H
#define SUBREGIONCACHEMANAGER_H

#include "common/jasm.h"
#include "common/jassert.h"

namespace petabricks {
  class SubRegionCacheManager {
  private:
    // ban
    SubRegionCacheManager() {}
    SubRegionCacheManager(const SubRegionCacheManager&) {}

  public:
    static void initialize() {
      _version = 0;
    }

    static void incVersion() {
      jalib::atomicIncrement(&_version);
      // JTRACE("inc cache")(_version);
    }

    static bool isValid(bool cacheVersion) {
      return cacheVersion == _version;
    }

    static long version() {
      return _version;
    }

  private:
    static jalib::AtomicT _version;
  };
}

#endif

