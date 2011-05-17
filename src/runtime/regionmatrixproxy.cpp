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

void RegionMatrixProxy::processGetHostListMsg(GetHostListMessage* msg) {
  DataHostList list = this->acquireRegionDataConst()->hosts(msg->begin, msg->end);
  this->releaseRegionDataConst();
  int hosts_array_size = list.size() * sizeof(DataHostListItem);
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)malloc(sizeof(GetHostListReplyMessage) + hosts_array_size);
  reply->numHosts = list.size();
  memcpy(reply->hosts, &list[0], hosts_array_size);
  _remoteObject->send(reply, sizeof(GetHostListReplyMessage) + hosts_array_size);
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
  case MessageTypes::GETHOSTLIST:
    JASSERT(len==sizeof(GetHostListMessage));
    this->processGetHostListMsg((GetHostListMessage*)data);
    break;
  default:
    JASSERT(false)("Unknown RegionRemoteMsgTypes.");
  }
}

RemoteObjectPtr RegionMatrixProxy::genLocal() {
  _remoteObject = new RegionMatrixProxyRemoteObject(this);
  return _remoteObject;
}
