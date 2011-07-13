#ifndef PETABRICKSREGIONMATRIXPROXY_H
#define PETABRICKSREGIONMATRIXPROXY_H

#include "regiondataremote.h"
#include "regionmatrixi.h"
#include "remoteobject.h"

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class RegionMatrixProxy;
  typedef jalib::JRef<RegionMatrixProxy> RegionMatrixProxyPtr;

  class RegionMatrixProxyRemoteObject;
  typedef jalib::JRef<RegionMatrixProxyRemoteObject> RegionMatrixProxyRemoteObjectPtr;

  class RegionMatrixProxy : public RegionMatrixI {
    RegionMatrixProxyRemoteObject* _remoteObject;

  public:
    RegionMatrixProxy(RegionHandlerPtr regionHandler);
    RegionMatrixProxy(RegionHandlerPtr regionHandler, RegionMatrixProxyRemoteObjectPtr remoteObject);
    ~RegionMatrixProxy() {
      JTRACE("Destruct RegionMatrixProxy");
    };

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    void onRecv(const void* data, size_t len);

    RegionMatrixProxyRemoteObjectPtr genLocal();
    static RemoteObjectPtr genRemote();

    void processReplyMsg(const BaseMessageHeader* base, size_t baseLen);
    void sendReply(const void* data, size_t len, const BaseMessageHeader* base);

  private:
    EncodedPtr encodedPtr() { return reinterpret_cast<EncodedPtr>(this); }

    void processReadCellMsg(const BaseMessageHeader* base, size_t baseLen);
    void processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen);
    void processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen);
    void processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen);
    void processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen);

    void forwardReplyMsg(const BaseMessageHeader* base, size_t baseLen);
  };

  class RegionMatrixProxyRemoteObject : public RemoteObject {
  protected:
    RegionMatrixProxyPtr _regionMatrix;
  public:
    RegionMatrixProxyRemoteObject() {};
    RegionMatrixProxyRemoteObject(RegionMatrixProxyPtr regionMatrix) {
      _regionMatrix = regionMatrix;
    }

    ~RegionMatrixProxyRemoteObject() {
      JTRACE("Destruct RegionMatrixProxyRemoteObject");
    }

    void onRecv(const void* data, size_t len) {
      _regionMatrix->onRecv(data, len);
    }

    void onRecvInitial(const void* buf, size_t len);

    EncodedPtr remoteObjPtr() {
      return remoteObj();
    }
  };

}

#endif
