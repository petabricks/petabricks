#include "regiondataremote.h"

#include "regionmatrix.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRemote::RegionDataRemote(int dimensions, IndexT* size, RemoteObjectPtr remoteObject) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATAREMOTE;
  _size = size;
  _remoteObject = remoteObject;
}

RegionDataRemote::~RegionDataRemote() {
  JTRACE("Destruct RegionDataRemote");
}

int RegionDataRemote::allocData() {
  JASSERT(false).Text("This should not be called.");
  return -1;
}

void* RegionDataRemote::fetchData(const void* msg, size_t len) {
  void* response = 0;
  ((struct MessageHeader*)msg)->response = &response;
  _remoteObject->send(msg, len);

  // wait for the data
  while (response == 0) {
    jalib::memFence();
    sched_yield();
  }

  return response;

}

ElementT RegionDataRemote::readCell(const IndexT* coord) {
  ReadCellMessage* msg = new ReadCellMessage();
  msg->header.type = MessageTypes::READCELL;
  memcpy(msg->coord, coord, _D * sizeof(IndexT));

  ReadCellReplyMessage reply = *(ReadCellReplyMessage*)this->fetchData(msg, sizeof *msg);
  delete msg;

  return reply.value;
}

void RegionDataRemote::writeCell(const IndexT* coord, ElementT value) {
  WriteCellMessage* msg = new WriteCellMessage();
  msg->header.type = MessageTypes::WRITECELL;
  msg->value = value;
  memcpy(msg->coord, coord, _D * sizeof(IndexT));

  WriteCellReplyMessage reply = *(WriteCellReplyMessage*)this->fetchData(msg, sizeof *msg);
  delete msg;

  JASSERT(reply.value == value);
}

DataHostList RegionDataRemote::hosts(IndexT* begin, IndexT* end) {
  GetHostListMessage* msg = new GetHostListMessage();
  msg->header.type = MessageTypes::GETHOSTLIST;
  memcpy(msg->begin, begin, _D * sizeof(IndexT));
  memcpy(msg->end, end, _D * sizeof(IndexT));

  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)this->fetchData(msg, sizeof *msg);
  delete msg;

  DataHostList list;
  for (int i = 0; i < reply->numHosts; i++) {
    list.push_back(reply->hosts[i]);
  }
  return list;
}

UpdateHandlerChainReplyMessage* RegionDataRemote::updateHandlerChain(UpdateHandlerChainMessage* msg) {
  msg->numHops += 1;
  UpdateHandlerChainReplyMessage* reply =
    (UpdateHandlerChainReplyMessage*)this->fetchData(msg, sizeof *msg);
  return reply;
}

UpdateHandlerChainReplyMessage* RegionDataRemote::updateHandlerChain() {
  UpdateHandlerChainMessage* msg = new UpdateHandlerChainMessage();
  msg->header.type = MessageTypes::UPDATEHANDLERCHAIN;
  msg->requester = HostPid::self();
  msg->numHops = 0;
  UpdateHandlerChainReplyMessage* reply = this->updateHandlerChain(msg);
  delete msg;
  return reply;
}

void RegionDataRemote::onRecv(const void* data, size_t len) {
  void* x = malloc(len);
  memmove(x, data, len);
  **((void***)data) = x;
}

RemoteObjectPtr RegionDataRemote::genRemote() {
  return new RegionDataRemoteObject();
}

//
// RegionDataRemoteObject
//

void RegionDataRemoteObject::onRecvInitial(const void* buf, size_t len) {
  JASSERT(len == sizeof(InitialMessage));
  InitialMessage* msg = (InitialMessage*) buf;

  IndexT* size = (IndexT*)malloc(sizeof(IndexT) * msg->dimensions);
  memcpy(size, msg->size, sizeof(IndexT) * msg->dimensions);

  _regionData = new RegionDataRemote(msg->dimensions, size, this);

  RegionMatrixMovingBuffer::instance().addMovingBuffer((RegionDataIPtr) _regionData, msg->movingBufferIndex);
}

