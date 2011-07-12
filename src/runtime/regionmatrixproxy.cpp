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
  return _regionHandler->readCell(coord);
}

void RegionMatrixProxy::writeCell(const IndexT* coord, ElementT value) {
  _regionHandler->writeCell(coord, value);
}

void RegionMatrixProxy::processReadCellMsg(const BaseMessageHeader* base) {
  ReadCellMessage* msg = (ReadCellMessage*)base->content();

  ReadCellReplyMessage reply;
  reply.value = this->readCell(msg->coord);
  this->sendReply(&reply, sizeof(ReadCellReplyMessage), base);
}

void RegionMatrixProxy::processWriteCellMsg(const BaseMessageHeader* base) {
  WriteCellMessage* msg = (WriteCellMessage*)base->content();

  this->writeCell(msg->coord, msg->value);
  WriteCellReplyMessage reply;
  reply.value = msg->value;
  this->sendReply(&reply, sizeof(WriteCellReplyMessage), base);
}

void RegionMatrixProxy::processGetHostListMsg(const BaseMessageHeader* base) {
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

void RegionMatrixProxy::processUpdateHandlerChainMsg(const BaseMessageHeader* base) {
  UpdateHandlerChainMessage* msg = (UpdateHandlerChainMessage*)base->content();

  RegionDataIPtr regionData = _regionHandler->getRegionData();
  UpdateHandlerChainReplyMessage reply;

  if (regionData->type() == RegionDataTypes::REGIONDATAREMOTE) {
    reply = ((RegionDataRemote*)regionData.asPtr())->updateHandlerChain(*msg);
  } else {
    reply.dataHost = HostPid::self();
    reply.numHops = msg->numHops;
    reply.encodedPtr = 0;

    if ((msg->requester != HostPid::self()) && (reply.numHops > 1)) {
      // 1->2->3 ==> 1->3
      // This create a new RegionMatrixProxy containing RemoteObject connection with requester's host.
      RemoteHostPtr dest = RemoteHostDB::instance().host(msg->requester);
      if (!dest) {
        JASSERT(false).Text("unknown host");
      }
      reply.encodedPtr = _regionHandler->moveToRemoteHost(dest);

    } else if (msg->requester == HostPid::self()) {
      // 1->...->1 ==> use a direct pointer to regiondata
      reply.encodedPtr = reinterpret_cast<EncodedPtr>(regionData.asPtr());
    }
  }

  this->sendReply(&reply, sizeof(UpdateHandlerChainReplyMessage), base);

}

void RegionMatrixProxy::processAllocDataMsg(const BaseMessageHeader* base) {
  AllocDataReplyMessage reply;
  reply.result = _regionHandler->allocData();
  this->sendReply(&reply, sizeof(AllocDataReplyMessage), base);
}

void RegionMatrixProxy::onRecv(const void* data, size_t len) {
  const BaseMessageHeader* base = (const BaseMessageHeader*)data;
  size_t msg_len = len - base->contentOffset;

  switch(base->type) {
  case MessageTypes::READCELL:
    JASSERT(msg_len == sizeof(ReadCellMessage));
    this->processReadCellMsg(base);
    break;
  case MessageTypes::WRITECELL:
    JASSERT(msg_len == sizeof(WriteCellMessage));
    this->processWriteCellMsg(base);
    break;
  case MessageTypes::GETHOSTLIST:
    JASSERT(msg_len == sizeof(GetHostListMessage));
    this->processGetHostListMsg(base);
    break;
  case MessageTypes::UPDATEHANDLERCHAIN:
    JASSERT(msg_len == sizeof(UpdateHandlerChainMessage));
    this->processUpdateHandlerChainMsg(base);
    break;
  case MessageTypes::ALLOCDATA:
    JASSERT(msg_len == sizeof(AllocDataMessage));
    this->processAllocDataMsg(base);
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

  _regionMatrix = new RegionMatrixProxy(new RegionHandler(msg->dimensions, msg->size, msg->partOffset), this);
}

