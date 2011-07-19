#include "regiondataremote.h"

#include "regionmatrixproxy.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRemote::RegionDataRemote(int dimensions, IndexT* size, RegionDataRemoteObjectPtr remoteObject) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATAREMOTE;
  _remoteObject = remoteObject;

  memcpy(_size, size, sizeof(IndexT) * _D);
}

RegionDataRemote::RegionDataRemote(int dimensions, IndexT* size, IndexT* partOffset, RemoteHostPtr host, EncodedPtr remoteRegionDataPtr) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATAREMOTE;
  memcpy(_size, size, sizeof(IndexT) * _D);

  _remoteObject = new RegionDataRemoteObject();

  // InitialMsg
  RegionDataRemoteMessage::InitialMessageToRegionMatrixProxy msg;
  msg.dimensions = _D;
  if (remoteRegionDataPtr == 0) {
    msg.type = MessageTypes::INITFROMDATASPLIT;
  } else {
    msg.type = MessageTypes::INITWITHREGIONDATA;
  }
  msg.encodedPtr = remoteRegionDataPtr;
  memcpy(msg.size, size, sizeof(msg.size));
  if (partOffset) {
    memcpy(msg.partOffset, partOffset, sizeof(msg.partOffset));
  }
  int len = sizeof(RegionDataRemoteMessage::InitialMessageToRegionMatrixProxy);

  host->createRemoteObject(_remoteObject.asPtr(), &RegionMatrixProxy::genRemote, &msg, len);
}

RegionDataRemote::RegionDataRemote(const int dimensions, const IndexT* size, RemoteHost& host, const EncodedPtr remoteRegionHandlerPtr) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATAREMOTE;
  memcpy(_size, size, sizeof(IndexT) * _D);

  _remoteObject = new RegionDataRemoteObject();

  // InitialMsg
  InitialMessageToRegionMatrixProxy msg;
  msg.dimensions = _D;
  msg.type = MessageTypes::INITWITHREGIONHANDLER;
  msg.encodedPtr = remoteRegionHandlerPtr;
  int len = sizeof(RegionDataRemoteMessage::InitialMessageToRegionMatrixProxy);
  host.createRemoteObject(_remoteObject.asPtr(), &RegionMatrixProxy::genRemote, &msg, len);
}

int RegionDataRemote::allocData() {
  AllocDataMessage msg;

  void* data;
  size_t len;
  this->fetchData(&msg, MessageTypes::ALLOCDATA, sizeof(AllocDataMessage), &data, &len);
  AllocDataReplyMessage* reply = (AllocDataReplyMessage*)data;

  ElementT result = reply->result;
  free(reply);
  return result;
}

ElementT RegionDataRemote::readCell(const IndexT* coord) {
  ReadCellMessage msg;
  memcpy(msg.coord, coord, _D * sizeof(IndexT));

  void* data;
  size_t len;
  this->fetchData(&msg, MessageTypes::READCELL, sizeof(ReadCellMessage), &data, &len);
  ReadCellReplyMessage* reply = (ReadCellReplyMessage*)data;

  ElementT value = reply->value;
  free(reply);
  return value;
}

void RegionDataRemote::writeCell(const IndexT* coord, ElementT value) {
  WriteCellMessage msg;
  msg.value = value;
  memcpy(msg.coord, coord, _D * sizeof(IndexT));

  void* data;
  size_t len;
  this->fetchData(&msg, MessageTypes::WRITECELL, sizeof(WriteCellMessage), &data, &len);
  WriteCellReplyMessage* reply = (WriteCellReplyMessage*)data;

  //JASSERT(reply->value == value)(reply->value)(value);
  free(reply);
}

DataHostList RegionDataRemote::hosts(IndexT* begin, IndexT* end) {
  GetHostListMessage msg;
  memcpy(msg.begin, begin, _D * sizeof(IndexT));
  memcpy(msg.end, end, _D * sizeof(IndexT));

  void* data;
  size_t len;
  this->fetchData(&msg, MessageTypes::GETHOSTLIST, sizeof(GetHostListMessage), &data, &len);
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)data;

  DataHostList list;
  for (int i = 0; i < reply->numHosts; i++) {
    list.push_back(reply->hosts[i]);
  }

  free(reply);
  return list;
}

UpdateHandlerChainReplyMessage RegionDataRemote::updateHandlerChain(UpdateHandlerChainMessage& msg) {
  msg.numHops += 1;

  void* data;
  size_t len;
  this->fetchData(&msg, MessageTypes::UPDATEHANDLERCHAIN, sizeof(UpdateHandlerChainMessage), &data, &len);
  UpdateHandlerChainReplyMessage* _reply = (UpdateHandlerChainReplyMessage*)data;

  UpdateHandlerChainReplyMessage reply = *_reply;
  free(_reply);
  return reply;
}

UpdateHandlerChainReplyMessage RegionDataRemote::updateHandlerChain() {
  UpdateHandlerChainMessage msg;
  msg.requester = HostPid::self();
  msg.numHops = 0;
  return this->updateHandlerChain(msg);
}

void RegionDataRemote::fetchData(const void* msg, MessageType type, size_t len, void** responseData, size_t* responseLen) {
  *responseData = 0;
  *responseLen = 0;

  size_t dataLen = sizeof(GeneralMessageHeader) + len;

  GeneralMessageHeader* header = (GeneralMessageHeader*)malloc(dataLen);
  header->isForwardMessage = false;
  header->type = type;
  header->contentOffset = sizeof(GeneralMessageHeader);
  header->responseData = reinterpret_cast<EncodedPtr>(responseData);
  header->responseLen = reinterpret_cast<EncodedPtr>(responseLen);

  memcpy(header->content(), msg, len);

  _remoteObject->send(header, dataLen);
  free(header);

  // wait for the data
  while (*responseData == 0 || *responseLen == 0) {
    jalib::memFence();
    sched_yield();
  }
}

void RegionDataRemote::onRecv(const void* data, size_t len) {
  const BaseMessageHeader* base = (const BaseMessageHeader*)data;
  if (base->isForwardMessage) {
    const ForwardMessageHeader* header = (const ForwardMessageHeader*)data;
    IRegionReplyProxy* proxy = reinterpret_cast<IRegionReplyProxy*>(header->callback);
    proxy->processReplyMsg(base, len);

  } else {
    const GeneralMessageHeader* header = (const GeneralMessageHeader*)data;

    const void** responseData = reinterpret_cast<const void**>(header->responseData);
    size_t* responseLen = reinterpret_cast<size_t*>(header->responseLen);

    size_t sz = len - base->contentOffset;
    void* msg = malloc(sz);
    memcpy(msg, base->content(), sz);

    *responseData = msg;
    *responseLen = sz;
  }
}

// Process remote messages
void RegionDataRemote::forwardMessage(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  size_t len = sizeof(ForwardMessageHeader) + baseLen;

  ForwardMessageHeader* data = (ForwardMessageHeader*)malloc(len);
  data->isForwardMessage = true;
  data->type = base->type;
  data->contentOffset = sizeof(ForwardMessageHeader) + base->contentOffset;
  data->callback = reinterpret_cast<EncodedPtr>(caller);

  memcpy(data->next(), base, baseLen);

  _remoteObject->send(data, len);
  free(data);
}

void RegionDataRemote::processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller, RegionDataIPtr) {
  UpdateHandlerChainMessage* msg = (UpdateHandlerChainMessage*)base->content();
  msg->numHops++;
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

RemoteObjectPtr RegionDataRemote::genRemote() {
  return new RegionDataRemoteObject();
}

//
// RegionDataRemoteObject
//

void RegionDataRemoteObject::onRecvInitial(const void* buf, size_t len) {
  JASSERT(len == sizeof(InitialMessageToRegionDataRemote))(len)(sizeof(InitialMessageToRegionDataRemote));
  InitialMessageToRegionDataRemote* msg = (InitialMessageToRegionDataRemote*) buf;

  _regionData = new RegionDataRemote(msg->dimensions, msg->size, this);
}

