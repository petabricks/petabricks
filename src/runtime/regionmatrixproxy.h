#ifndef PETABRICKSREGIONMATRIXPROXY_H
#define PETABRICKSREGIONMATRIXPROXY_H

#include "regiondataremote.h"
#include "regionmatrixi.h"
#include "remoteobject.h"

namespace petabricks {
  class RegionMatrixProxy;
  typedef jalib::JRef<RegionMatrixProxy> RegionMatrixProxyPtr;

  class RegionMatrixProxyRemoteObject;
  typedef jalib::JRef<RegionMatrixProxyRemoteObject> RegionMatrixProxyRemoteObjectPtr;

  class RegionMatrixProxy : public RegionMatrixI {
    RegionMatrixProxyRemoteObject* _remoteObject;

  public:
    RegionMatrixProxy(RegionHandlerPtr regionHandler);
    ~RegionMatrixProxy();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    void onRecv(const void* data, size_t len);

    RegionMatrixProxyRemoteObjectPtr genLocal();

  private:
    void processReadCellMsg(RegionDataRemoteMessage::ReadCellMessage* msg);
    void processWriteCellMsg(RegionDataRemoteMessage::WriteCellMessage* msg);
    void processGetHostListMsg(RegionDataRemoteMessage::GetHostListMessage* msg);
    void processUpdateHandlerChainMsg(RegionDataRemoteMessage::UpdateHandlerChainMessage msg);
  };

  class RegionMatrixProxyRemoteObject : public RemoteObject {
  protected:
    RegionMatrixProxyPtr _regionMatrix;
  public:
    RegionMatrixProxyRemoteObject(RegionMatrixProxyPtr regionMatrix) {
      _regionMatrix = regionMatrix;
    }

    ~RegionMatrixProxyRemoteObject() {
      JTRACE("Destruct RegionMatrixProxyRemoteObject");
    }

    void onRecv(const void* data, size_t len) {
      _regionMatrix->onRecv(data, len);
    }

    EncodedPtr remoteObjPtr() {
      return remoteObj();
    }
  };

}

#endif
