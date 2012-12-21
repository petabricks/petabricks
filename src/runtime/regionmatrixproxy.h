#ifndef PETABRICKSREGIONMATRIXPROXY_H
#define PETABRICKSREGIONMATRIXPROXY_H

#include "iregionreplyproxy.h"
#include "regiondataremote.h"
#include "regionmatrixi.h"
#include "remoteobject.h"

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class RegionMatrixProxy;
  typedef jalib::JRef<RegionMatrixProxy> RegionMatrixProxyPtr;

  class RegionMatrixProxy : public RegionMatrixI, public IRegionReplyProxy, public RemoteObject {

  public:
    RegionMatrixProxy() {}
    RegionMatrixProxy(RegionHandlerPtr regionHandler);

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    void onRecv(const void* data, size_t len, int arg);
    void onRecvInitial(const void* buf, size_t len);

    static RemoteObjectPtr genRemote();

    // IRegionReplyProxy
    void processReplyMsg(const BaseMessageHeader* base, size_t baseLen, int replyType);
    void sendReply(const void* data, size_t len, const BaseMessageHeader* base, int replyType=0);

  private:
    void forwardReplyMsg(const BaseMessageHeader* base, size_t baseLen, int replyType);
  };
}

#endif
