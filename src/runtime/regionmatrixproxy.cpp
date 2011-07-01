#include "regionmatrixproxy.h"

#include "common/jassert.h"
#include "common/jconvert.h"
#include "regionmatrix.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionMatrixProxy::RegionMatrixProxy(RegionHandlerPtr regionHandler) {
  _regionHandler = regionHandler;
}

ElementT RegionMatrixProxy::readCell(const IndexT* coord) {
  return _regionHandler->readCell(coord);
}

void RegionMatrixProxy::writeCell(const IndexT* coord, ElementT value) {
  _regionHandler->writeCell(coord, value);
}

void RegionMatrixProxy::processReadCellMsg(ReadCellMessage& msg) {
  ReadCellReplyMessage reply;
  reply.header = msg.header;
  reply.value = this->readCell(msg.coord);
  _remoteObject->send(&reply, sizeof(ReadCellReplyMessage));
}

void RegionMatrixProxy::processWriteCellMsg(WriteCellMessage& msg) {
  this->writeCell(msg.coord, msg.value);
  WriteCellReplyMessage reply;
  reply.header = msg.header;
  reply.value = msg.value;
  _remoteObject->send(&reply, sizeof(WriteCellReplyMessage));
}

void RegionMatrixProxy::processGetHostListMsg(GetHostListMessage& msg) {
  DataHostList list = _regionHandler->hosts(msg.begin, msg.end);
  size_t hosts_array_size = list.size() * sizeof(DataHostListItem);
  size_t sz = sizeof(GetHostListReplyMessage) + hosts_array_size;

  char buf[sz];
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)buf;
  reply->header = msg.header;
  reply->numHosts = list.size();
  memcpy(reply->hosts, &list[0], hosts_array_size);
  _remoteObject->send(buf, sz);
}

void RegionMatrixProxy::processUpdateHandlerChainMsg(RegionDataRemoteMessage::UpdateHandlerChainMessage& msg) {
  MessageReplyHeader header;
  header = msg.header;

  RegionDataIPtr regionData = _regionHandler->getRegionData();
  UpdateHandlerChainReplyMessage reply;

  if (regionData->type() == RegionDataTypes::REGIONDATAREMOTE) {
    reply = ((RegionDataRemote*)regionData.asPtr())->updateHandlerChain(msg);
  } else {
    reply.dataHost = HostPid::self();
    reply.numHops = msg.numHops;
    reply.encodedPtr = 0;

    if ((msg.requester != HostPid::self()) && (reply.numHops > 1)) {
      // 1->2->3 ==> 1->3
      // This create a new RegionMatrixProxy containing RemoteObject connection with requester's host.
      RemoteHostPtr dest = RemoteHostDB::instance().host(msg.requester);
      if (!dest) {
        JASSERT(false).Text("unknown host");
      }
      reply.encodedPtr = _regionHandler->moveToRemoteHost(dest);

    } else if (msg.requester == HostPid::self()) {
      // 1->...->1 ==> use a direct pointer to regiondata
      reply.encodedPtr = reinterpret_cast<EncodedPtr>(regionData.asPtr());
    }
  }

  reply.header = header;
  _remoteObject->send(&reply, sizeof(UpdateHandlerChainReplyMessage));
}

void RegionMatrixProxy::onRecv(const void* data, size_t len) {
  switch(((struct MessageHeader*)data)->type) {
  case MessageTypes::READCELL:
    JASSERT(len == sizeof(ReadCellMessage));
    this->processReadCellMsg(*(ReadCellMessage*)data);
    break;
  case MessageTypes::WRITECELL:
    JASSERT(len == sizeof(WriteCellMessage));
    this->processWriteCellMsg(*(WriteCellMessage*)data);
    break;
  case MessageTypes::GETHOSTLIST:
    JASSERT(len==sizeof(GetHostListMessage));
    this->processGetHostListMsg(*(GetHostListMessage*)data);
    break;
  case MessageTypes::UPDATEHANDLERCHAIN:
    JASSERT(len==sizeof(UpdateHandlerChainMessage));
    this->processUpdateHandlerChainMsg(*(UpdateHandlerChainMessage*)data);
    break;
  default:
    JASSERT(false)(((struct MessageHeader*)data)->type).Text("Unknown RegionRemoteMsgTypes.");
  }
}

RegionMatrixProxyRemoteObjectPtr RegionMatrixProxy::genLocal() {
  _remoteObject = new RegionMatrixProxyRemoteObject(this);
  return _remoteObject;
}
