#include "regionmatrixproxy.h"

#include "common/jassert.h"
#include "common/jconvert.h"
#include "regionmatrix.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionMatrixProxy::RegionMatrixProxy(RegionHandlerPtr regionHandler) {
  _regionHandler = regionHandler;
}

ElementT RegionMatrixProxy::readCell(const IndexT*) {
  JASSERT(false).Text("Should not be called.");
  return 0;
}

void RegionMatrixProxy::writeCell(const IndexT*, ElementT) {
  JASSERT(false).Text("Should not be called.");
}

void RegionMatrixProxy::onRecv(const void* data, size_t len, int messageType) {
  const BaseMessageHeader* base = (const BaseMessageHeader*)data;
  // JTRACE("msg")(messageType);
  switch(messageType) {
  case MessageTypes::READCELL:
    _regionHandler->processReadCellMsg(base, len, this);
    break;
  case MessageTypes::READCELLCACHE:
    _regionHandler->processReadCellCacheMsg(base, len, this);
    break;
  case MessageTypes::WRITECELL:
    _regionHandler->processWriteCellMsg(base, len, this);
    break;
  case MessageTypes::WRITECELLCACHE:
    _regionHandler->processWriteCellCacheMsg(base, len, this);
    break;
  case MessageTypes::GETHOSTLIST:
    _regionHandler->processGetHostListMsg(base, len, this);
    break;
  case MessageTypes::UPDATEHANDLERCHAIN:
    _regionHandler->processUpdateHandlerChainMsg(base, len, this);
    break;
  case MessageTypes::ALLOCDATA:
    _regionHandler->processAllocDataMsg(base, len, this);
    break;
  case MessageTypes::RANDOMIZEDATA:
    _regionHandler->processRandomizeDataMsg(base, len, this);
    break;
  case MessageTypes::TOSCRATCHSTORAGE:
    _regionHandler->processCopyToMatrixStorageMsg(base, len, this);
    break;
  case MessageTypes::FROMSCRATCHSTORAGE:
    _regionHandler->processCopyFromMatrixStorageMsg(base, len, this);
    break;
  case MessageTypes::COPYREGIONDATASPLIT:
    _regionHandler->processCopyRegionDataSplitMsg(base, len, this);
    break;
  default:
    JASSERT(false)(messageType)(base->type).Text("Unknown RegionRemoteMsgTypes.");
  }
}

void RegionMatrixProxy::sendReply(const void* msg, size_t len, const BaseMessageHeader* base, int replyType) {
  size_t dataLen = base->contentOffset + len;
  void* data = malloc(dataLen);

  memcpy(data, base, base->contentOffset);
  memcpy(((BaseMessageHeader*)data)->content(), msg, len);

  this->sendMu(data, dataLen, replyType);
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
  this->send(base->next(), baseLen - sizeof(ForwardMessageHeader), type);
}

RemoteObjectPtr RegionMatrixProxy::genRemote() {
  return new RegionMatrixProxy();
}

void RegionMatrixProxy::onRecvInitial(const void* buf, size_t len) {
  GeneralInitialMessage* m = (GeneralInitialMessage*) buf;

  if (m->type == MessageTypes::CREATEREMOTEREGIONDATA) {
    CreateRegionDataInitialMessage* msg = (CreateRegionDataInitialMessage*) buf;
    _regionHandler = new RegionHandler(msg->dimensions, msg->size, true);

    RemoteRegionHandler remoteRegionHandler;
    remoteRegionHandler.hostPid = HostPid::self();
    remoteRegionHandler.remoteHandler = reinterpret_cast<EncodedPtr>(_regionHandler.asPtr());
    send(&remoteRegionHandler, sizeof(RemoteRegionHandler), MessageTypes::CREATEREMOTEREGIONDATAREPLY);

  } else if (m->type == MessageTypes::INITWITHREGIONHANDLER) {
    JASSERT(len == sizeof(EncodedPtrInitialMessage))(len);
    EncodedPtrInitialMessage* msg = (EncodedPtrInitialMessage*) buf;
    _regionHandler = reinterpret_cast<RegionHandler*>(msg->encodedPtr);

  } else {
    JASSERT(false).Text("Unknown initial message type.");
  }

}

