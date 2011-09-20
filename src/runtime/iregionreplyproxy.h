#ifndef PETABRICKSIREGIONREPLYPROXY_H
#define PETABRICKSIREGIONREPLYPROXY_H

#include "regiondataremotemessages.h"

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class IRegionReplyProxy {
  public:
    virtual ~IRegionReplyProxy() {}

    virtual void processReplyMsg(const BaseMessageHeader* base, size_t baseLen) = 0;
    virtual void sendReply(const void* data, size_t len, const BaseMessageHeader* base) = 0;
  };

}

#endif

