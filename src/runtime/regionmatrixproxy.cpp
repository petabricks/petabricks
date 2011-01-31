#include "regionmatrixproxy.h"

using namespace petabricks;

RegionMatrixProxy::RegionMatrixProxy(RegionHandlerPtr regionHandler) {
  _regionHandler = regionHandler;
}

ElementT RegionMatrixProxy::readCell(const IndexT* coord) {
  return _regionData->readCell(coord);
}

void RegionMatrixProxy::writeCell(const IndexT* coord, ElementT value) {
  _regionData->writeCell(coord, value);
}

void RegionMatrixProxy::onRecv(const void* data, size_t len) {


}
 
RemoteObjectPtr RegionMatrixProxy::genLocal() {
  _remoteObject = new RegionMatrixProxyRemoteObject(this);
  return _remoteObject;
}
