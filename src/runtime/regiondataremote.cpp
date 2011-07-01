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

RegionDataRemote::RegionDataRemote(int dimensions, IndexT* size, IndexT* partOffset, RemoteHostPtr host) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATAREMOTE;
  memcpy(_size, size, sizeof(IndexT) * _D);

  _remoteObject = new RegionDataRemoteObject();

  // InitialMsg
  RegionDataRemoteMessage::InitialMessageToRegionMatrixProxy msg;
  msg.dimensions = _D;
  memcpy(msg.size, size, sizeof(msg.size));
  memcpy(msg.partOffset, partOffset, sizeof(msg.partOffset));
  int len = sizeof(RegionDataRemoteMessage::InitialMessageToRegionMatrixProxy);

  host->createRemoteObject(_remoteObject.asPtr(), &RegionMatrixProxy::genRemote, &msg, len);
}

int RegionDataRemote::allocData() {
  AllocDataMessage msg;
  msg.header.type = MessageTypes::ALLOCDATA;

  void* data;
  size_t len;
  this->fetchData(&msg, sizeof(AllocDataMessage), &data, &len);
  AllocDataReplyMessage* reply = (AllocDataReplyMessage*)data;

  return reply->result;
}

ElementT RegionDataRemote::readCell(const IndexT* coord) {
  ReadCellMessage msg;
  msg.header.type = MessageTypes::READCELL;
  memcpy(msg.coord, coord, _D * sizeof(IndexT));

  void* data;
  size_t len;
  this->fetchData(&msg, sizeof(ReadCellMessage), &data, &len);
  ReadCellReplyMessage* reply = (ReadCellReplyMessage*)data;

  return reply->value;
}

void RegionDataRemote::writeCell(const IndexT* coord, ElementT value) {
  WriteCellMessage msg;
  msg.header.type = MessageTypes::WRITECELL;
  msg.value = value;
  memcpy(msg.coord, coord, _D * sizeof(IndexT));

  void* data;
  size_t len;
  this->fetchData(&msg, sizeof(WriteCellMessage), &data, &len);
  WriteCellReplyMessage* reply = (WriteCellReplyMessage*)data;

  JASSERT(reply->value == value);
}

DataHostList RegionDataRemote::hosts(IndexT* begin, IndexT* end) {
  GetHostListMessage msg;
  msg.header.type = MessageTypes::GETHOSTLIST;
  memcpy(msg.begin, begin, _D * sizeof(IndexT));
  memcpy(msg.end, end, _D * sizeof(IndexT));

  void* data;
  size_t len;
  this->fetchData(&msg, sizeof(GetHostListMessage), &data, &len);
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)data;

  DataHostList list;
  for (int i = 0; i < reply->numHosts; i++) {
    list.push_back(reply->hosts[i]);
  }
  return list;
}

UpdateHandlerChainReplyMessage& RegionDataRemote::updateHandlerChain(UpdateHandlerChainMessage& msg) {
  msg.numHops += 1;

  void* data;
  size_t len;
  this->fetchData(&msg, sizeof(UpdateHandlerChainMessage), &data, &len);
  UpdateHandlerChainReplyMessage* reply = (UpdateHandlerChainReplyMessage*)data;

  return *reply;
}

UpdateHandlerChainReplyMessage& RegionDataRemote::updateHandlerChain() {
  UpdateHandlerChainMessage msg;
  msg.header.type = MessageTypes::UPDATEHANDLERCHAIN;
  msg.requester = HostPid::self();
  msg.numHops = 0;
  return this->updateHandlerChain(msg);
}

void RegionDataRemote::fetchData(const void* msg, size_t len, void** responseData, size_t* responseLen) {
  *responseData = 0;
  *responseLen = 0;

  ((MessageHeader*)msg)->responseData = reinterpret_cast<EncodedPtr>(responseData);
  ((MessageHeader*)msg)->responseLen = reinterpret_cast<EncodedPtr>(responseLen);

  _remoteObject->send(msg, len);

  // wait for the data
  while (*responseData == 0 || *responseLen == 0) {
    jalib::memFence();
    sched_yield();
  }
}

void RegionDataRemote::onRecv(const void* data, size_t len) {
  const MessageReplyHeader* header = (const MessageReplyHeader*)data;
  const void** responseData = reinterpret_cast<const void**>(header->responseData);
  size_t* responseLen = reinterpret_cast<size_t*>(header->responseLen);

  *responseData = data;
  *responseLen = len;
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

