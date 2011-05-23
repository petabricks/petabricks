#include "regionmatrixproxy.h"

#include "common/jassert.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionMatrixProxy::RegionMatrixProxy(RegionHandlerPtr regionHandler) {
  _regionHandler = regionHandler;
}

ElementT RegionMatrixProxy::readCell(const IndexT* coord) {
  return _regionData->readCell(coord);
}

void RegionMatrixProxy::writeCell(const IndexT* coord, ElementT value) {
  _regionData->writeCell(coord, value);
}

void RegionMatrixProxy::processReadCellMsg(ReadCellMessage* msg) {
  // TODO: add acquire/release regiondata methods
  this->acquireRegionData();
  ElementT cell = this->readCell(msg->coord);
  this->releaseRegionData();

  _remoteObject->send(&cell, sizeof(ElementT));
}

void RegionMatrixProxy::processWriteCellMsg(WriteCellMessage* msg) {
  // TODO: add acquire/release regiondata methods
  this->acquireRegionData();
  this->writeCell(msg->coord, msg->value);
  this->releaseRegionData();

  _remoteObject->send(&msg->value, sizeof(ElementT));
}

void RegionMatrixProxy::onRecv(const void* data, size_t len) {
  switch(*(MessageType*)data) {
  case MessageTypes::READCELL:
    JASSERT(len == sizeof(ReadCellMessage));
    this->processReadCellMsg((ReadCellMessage*)data);
    break;
  case MessageTypes::WRITECELL:
    JASSERT(len == sizeof(WriteCellMessage));
    this->processWriteCellMsg((WriteCellMessage*)data);
    break;
  default:
    JASSERT(false)("Unknown RegionRemoteMsgTypes.");
  }
}
 
RemoteObjectPtr RegionMatrixProxy::genLocal() {
  _remoteObject = new RegionMatrixProxyRemoteObject(this);
  return _remoteObject;
}
