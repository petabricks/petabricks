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

ElementT RegionMatrixProxy::readCell(const IndexT*) {
  JASSERT(false).Text("Should not be called.");
  return 0;
}

void RegionMatrixProxy::writeCell(const IndexT*, ElementT) {
  JASSERT(false).Text("Should not be called.");
}

void RegionMatrixProxy::onRecv(const void* data, size_t len, int) {
  const BaseMessageHeader* base = (const BaseMessageHeader*)data;
  //size_t msg_len = len - base->contentOffset;
  switch(base->type) {
  case MessageTypes::READCELL:
    //JASSERT(msg_len == sizeof(ReadCellMessage));
    _regionHandler->processReadCellMsg(base, len, this);
    break;
  case MessageTypes::READCELLCACHE:
    //JASSERT(msg_len == sizeof(ReadCellCacheMessage));
    _regionHandler->processReadCellCacheMsg(base, len, this);
    break;
  case MessageTypes::WRITECELL:
    //JASSERT(msg_len == sizeof(WriteCellMessage));
    _regionHandler->processWriteCellMsg(base, len, this);
    break;
  case MessageTypes::WRITECELLCACHE:
    //JASSERT(msg_len == sizeof(WriteCellCacheMessage));
    _regionHandler->processWriteCellCacheMsg(base, len, this);
    break;
  case MessageTypes::GETHOSTLIST:
    //JASSERT(msg_len == sizeof(GetHostListMessage));
    _regionHandler->processGetHostListMsg(base, len, this);
    break;
  case MessageTypes::UPDATEHANDLERCHAIN:
    //JASSERT(msg_len == sizeof(UpdateHandlerChainMessage));
    _regionHandler->processUpdateHandlerChainMsg(base, len, this);
    break;
  case MessageTypes::ALLOCDATA:
    //JASSERT(msg_len == sizeof(AllocDataMessage));
    _regionHandler->processAllocDataMsg(base, len, this);
    break;
  case MessageTypes::RANDOMIZEDATA:
    //JASSERT(msg_len == sizeof(RandomizeDataMessage));
    _regionHandler->processRandomizeDataMsg(base, len, this);
    break;
  case MessageTypes::TOSCRATCHSTORAGE:
    _regionHandler->processCopyToMatrixStorageMsg(base, len, this);
    break;
  case MessageTypes::FROMSCRATCHSTORAGE:
    _regionHandler->processCopyFromMatrixStorageMsg(base, len, this);
    break;
  default:
    JASSERT(false)(base->type).Text("Unknown RegionRemoteMsgTypes.");
  }
}

void RegionMatrixProxy::sendReply(const void* msg, size_t len, const BaseMessageHeader* base, int replyType) {
  size_t dataLen = base->contentOffset + len;
  void* data = malloc(dataLen);

  memcpy(data, base, base->contentOffset);
  memcpy(((BaseMessageHeader*)data)->content(), msg, len);

  _remoteObject->send(data, dataLen, replyType);
  free(data);
}

//
// Process reply messages

void RegionMatrixProxy::processReplyMsg(const BaseMessageHeader* base, size_t baseLen, int type) {
  switch(base->type) {
  case MessageTypes::READCELL:
  case MessageTypes::WRITECELL:
  case MessageTypes::READCELLCACHE:
  case MessageTypes::WRITECELLCACHE:
  case MessageTypes::ALLOCDATA:
  case MessageTypes::RANDOMIZEDATA:
  case MessageTypes::UPDATEHANDLERCHAIN:
  case MessageTypes::GETHOSTLIST:
  case MessageTypes::TOSCRATCHSTORAGE:
  case MessageTypes::FROMSCRATCHSTORAGE:
  case MessageTypes::COPYREGIONDATASPLIT:
    this->forwardReplyMsg(base, baseLen, type);
    break;
  default:
    JASSERT(false)(base->type).Text("Unknown RegionRemoteMsgTypes.");
  }
}

void RegionMatrixProxy::forwardReplyMsg(const BaseMessageHeader* base, size_t baseLen, int type) {
  _remoteObject->send(base->next(), baseLen - sizeof(ForwardMessageHeader), type);
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
  GeneralInitialMessage* m = (GeneralInitialMessage*) buf;

  if (m->type == MessageTypes::CREATEREMOTEREGIONDATA) {
    CreateRegionDataInitialMessage* msg = (CreateRegionDataInitialMessage*) buf;
    RegionHandlerPtr handler = new RegionHandler(msg->dimensions, msg->size, true);
    _regionMatrix = new RegionMatrixProxy(handler, this);

    RemoteRegionHandler remoteRegionHandler;
    remoteRegionHandler.hostPid = HostPid::self();
    remoteRegionHandler.remoteHandler = reinterpret_cast<EncodedPtr>(handler.asPtr());
    send(&remoteRegionHandler, sizeof(RemoteRegionHandler), MessageTypes::CREATEREMOTEREGIONDATAREPLY);

  } else if (m->type == MessageTypes::INITWITHREGIONHANDLER) {
    JASSERT(len == sizeof(EncodedPtrInitialMessage))(len);
    EncodedPtrInitialMessage* msg = (EncodedPtrInitialMessage*) buf;
    RegionHandler* regionHandler = reinterpret_cast<RegionHandler*>(msg->encodedPtr);
    _regionMatrix = new RegionMatrixProxy(regionHandler, this);

  } else {
    JASSERT(false).Text("Unknown initial message type.");
  }

}

