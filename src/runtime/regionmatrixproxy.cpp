#include "regionmatrixproxy.h"

#include "common/jassert.h"
#include "common/jconvert.h"
#include "regionmatrix.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionMatrixProxy::RegionMatrixProxy(RegionHandlerPtr regionHandler) {
  _regionHandler = regionHandler;
}

RegionMatrixProxy::RegionMatrixProxy(RegionHandlerPtr regionHandler, RegionMatrixProxyRemoteObjectPtr remoteObject) {
  _regionHandler = regionHandler;
  _remoteObject = remoteObject.asPtr();
}

ElementT RegionMatrixProxy::readCell(const IndexT* coord) {
  JASSERT(false).Text("Should not be called.");
  return 0;
}

void RegionMatrixProxy::writeCell(const IndexT* coord, ElementT value) {
  _regionHandler->writeCell(coord, value);
}

void RegionMatrixProxy::processReadCellMsg(const BaseMessageHeader* base, size_t baseLen) {
  ReadCellReplyMessage reply;
  size_t len = 0;
  _regionHandler->processReadCellMsg(base, baseLen, reply, len, encodedPtr());
  if (len > 0) {
    this->sendReply(&reply, len, base);
  }
}

void RegionMatrixProxy::processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen) {
  WriteCellReplyMessage reply;
  size_t len = 0;
  _regionHandler->processWriteCellMsg(base, baseLen, reply, len, encodedPtr());
  if (len > 0) {
    this->sendReply(&reply, len, base);
  }
}

void RegionMatrixProxy::processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen) {
  GetHostListMessage* msg = (GetHostListMessage*)base->content();

  DataHostList list = _regionHandler->hosts(msg->begin, msg->end);
  size_t hosts_array_size = list.size() * sizeof(DataHostListItem);
  size_t sz = sizeof(GetHostListReplyMessage) + hosts_array_size;

  char buf[sz];
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)buf;
  reply->numHosts = list.size();
  memcpy(reply->hosts, &list[0], hosts_array_size);
  this->sendReply(buf, sz, base);
}

void RegionMatrixProxy::processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen) {
  UpdateHandlerChainReplyMessage reply;
  size_t len = 0;
  _regionHandler->processUpdateHandlerChainMsg(base, baseLen, reply, len, encodedPtr());
  if (len > 0) {
    this->sendReply(&reply, len, base);
  }
}

void RegionMatrixProxy::processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen) {
  AllocDataReplyMessage reply;
  size_t len = 0;
  _regionHandler->processAllocDataMsg(base, baseLen, reply, len, encodedPtr());
  if (len > 0) {
    this->sendReply(&reply, len, base);
  }
}

void RegionMatrixProxy::onRecv(const void* data, size_t len) {
  const BaseMessageHeader* base = (const BaseMessageHeader*)data;
  size_t msg_len = len - base->contentOffset;

  switch(base->type) {
  case MessageTypes::READCELL:
    JASSERT(msg_len == sizeof(ReadCellMessage));
    this->processReadCellMsg(base, len);
    break;
  case MessageTypes::WRITECELL:
    JASSERT(msg_len == sizeof(WriteCellMessage));
    this->processWriteCellMsg(base, len);
    break;
  case MessageTypes::GETHOSTLIST:
    JASSERT(msg_len == sizeof(GetHostListMessage));
    this->processGetHostListMsg(base, len);
    break;
  case MessageTypes::UPDATEHANDLERCHAIN:
    JASSERT(msg_len == sizeof(UpdateHandlerChainMessage));
    this->processUpdateHandlerChainMsg(base, len);
    break;
  case MessageTypes::ALLOCDATA:
    JASSERT(msg_len == sizeof(AllocDataMessage));
    this->processAllocDataMsg(base, len);
    break;
  default:
    JASSERT(false)(base->type).Text("Unknown RegionRemoteMsgTypes.");
  }
}

void RegionMatrixProxy::sendReply(const void* msg, size_t len, const BaseMessageHeader* base) {
  size_t dataLen = base->contentOffset + len;
  void* data = malloc(dataLen);

  memcpy(data, base, base->contentOffset);
  memcpy(((BaseMessageHeader*)data)->content(), msg, len);

  _remoteObject->send(data, dataLen);
  free(data);
}

//
// Process reply messages

void RegionMatrixProxy::processReplyMsg(const BaseMessageHeader* base, size_t baseLen) {
  switch(base->type) {
  case MessageTypes::READCELL:
  case MessageTypes::WRITECELL:
  case MessageTypes::ALLOCDATA:
  case MessageTypes::UPDATEHANDLERCHAIN:
    this->forwardReplyMsg(base, baseLen);
    break;
  default:
    JASSERT(false)(base->type).Text("Unknown RegionRemoteMsgTypes.");
  }
}

void RegionMatrixProxy::forwardReplyMsg(const BaseMessageHeader* base, size_t baseLen) {
  _remoteObject->send(base->next(), baseLen - sizeof(ForwardMessageHeader));
}

RegionMatrixProxyRemoteObjectPtr RegionMatrixProxy::genLocal() {
  _remoteObject = new RegionMatrixProxyRemoteObject(this);
  return _remoteObject;
}

RemoteObjectPtr RegionMatrixProxy::genRemote() {
  return new RegionMatrixProxyRemoteObject();
}

//
// RegionMatrixProxyRemoteObject
//

void RegionMatrixProxyRemoteObject::onRecvInitial(const void* buf, size_t len) {
  JASSERT(len == sizeof(InitialMessageToRegionMatrixProxy))(len)(sizeof(InitialMessageToRegionMatrixProxy));
  InitialMessageToRegionMatrixProxy* msg = (InitialMessageToRegionMatrixProxy*) buf;

  if (msg->remoteRegionData == 0) {
    _regionMatrix = new RegionMatrixProxy(new RegionHandler(msg->dimensions, msg->size, msg->partOffset), this);
  } else {
    RegionDataIPtr regionData = reinterpret_cast<RegionDataI*>(msg->remoteRegionData);
    _regionMatrix = new RegionMatrixProxy(new RegionHandler(regionData), this);
  }
}

