#include "regiondataproxy.h"

using namespace petabricks;
using namespace petabricks::RegionDataProxyMessage;

RegionDataProxy::RegionDataProxy(int dimensions, IndexT* size, IndexT* partOffset, RemoteHostPtr host) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATAPROXY;

  _size = new IndexT[_D];
  memcpy(_size, size, sizeof(IndexT) * _D);

  _partOffset = new IndexT[_D];
  memcpy(_partOffset, partOffset, sizeof(IndexT) * _D);

  pthread_mutex_init(&_seq_mux, NULL);
  pthread_mutex_init(&_buffer_mux, NULL);
  pthread_cond_init(&_buffer_cond, NULL);
  _seq = 0;
  _recv_seq = 0;

  // Create Remote Object
  RemoteObjectPtr local = this->genLocal();

  InitialMessage* msg = new InitialMessage();
  msg->dimensions = _D;
  memcpy(msg->size, _size, sizeof(msg->size));
  memcpy(msg->partOffset, _partOffset, sizeof(msg->partOffset));

  int len = sizeof *msg;

  host->createRemoteObject(local, &RegionDataProxy::genRemote, msg, len);
  local->waitUntilCreated();
}

RegionDataProxy::~RegionDataProxy() {
  pthread_mutex_destroy(&_seq_mux);
  pthread_mutex_destroy(&_buffer_mux);
  pthread_cond_destroy(&_buffer_cond);

  delete [] _partOffset;
}

void* RegionDataProxy::fetchData(const void* msg, size_t len) {
  pthread_mutex_lock(&_seq_mux);
  _remoteObject->send(msg, len);
  uint16_t seq = ++_seq;
  pthread_mutex_unlock(&_seq_mux);

  // wait for the data
  pthread_mutex_lock(&_buffer_mux);
  while (seq > _recv_seq) {
    pthread_cond_wait(&_buffer_cond, &_buffer_mux);
  }

  void* ret = _buffer[seq];
  _buffer.erase(seq);

  pthread_mutex_unlock(&_buffer_mux);

  // wake other threads
  pthread_cond_broadcast(&_buffer_cond);

  return ret;
}

void RegionDataProxy::onRecv(const void* data, size_t len) {
  JTRACE("recv")(*(ElementT*)data)(len);
  void* x = malloc(len);
  memmove(x, data, len);
  _buffer[++_recv_seq] = x;
  pthread_cond_broadcast(&_buffer_cond);
}

int RegionDataProxy::allocData() {
  AllocDataMessage* msg = new AllocDataMessage();
  msg->type = MessageTypes::ALLOCDATA;

  int r = *(int*)this->fetchData(msg, sizeof *msg);

  delete msg;
  return r;
}

ElementT RegionDataProxy::readCell(const IndexT* coord) {
  ReadCellMessage* msg = new ReadCellMessage();
  msg->type = MessageTypes::READCELL;
  memcpy(msg->coord, coord, _D * sizeof(IndexT));

  ElementT elmt = *(ElementT*)this->fetchData(msg, sizeof *msg);

  delete msg;
  return elmt;
}

void RegionDataProxy::writeCell(const IndexT* coord, ElementT value) {
  WriteCellMessage* msg = new WriteCellMessage();
  msg->type = MessageTypes::WRITECELL;
  msg->value = value;
  memcpy(msg->coord, coord, _D * sizeof(IndexT));

  ElementT elmt = *(ElementT*)this->fetchData(msg, sizeof *msg);

  delete msg;
  JASSERT(elmt == value);
}

RemoteObjectPtr RegionDataProxy::genLocal() {
  class RegionDataProxyLocalRemoteObject : public RemoteObject {
   private:
    RegionDataProxyPtr _regionDataProxy;
   public:
    RegionDataProxyLocalRemoteObject(RegionDataProxyPtr regionDataProxy) {
      _regionDataProxy = regionDataProxy;
    }

    void onRecv(const void* data, size_t len) {
      _regionDataProxy->onRecv(data, len);
    }
  };

  _remoteObject = new RegionDataProxyLocalRemoteObject(this);
  return _remoteObject;
}

RemoteObjectPtr RegionDataProxy::genRemote() {
  return new RegionDataProxyRemoteObject();
}

// RegionDataProxyRemoteObject

void RegionDataProxyRemoteObject::onRecvInitial(const void* buf, size_t len) {
  JASSERT(len==sizeof(InitialMessage));
  InitialMessage* msg = (InitialMessage*) buf;
  _regionData = new RegionDataRaw(msg->dimensions, msg->size, msg->partOffset);
}

void RegionDataProxyRemoteObject::processReadCellMsg(ReadCellMessage* msg) {
  ElementT cell = _regionData->readCell(msg->coord);
  this->send(&cell, sizeof(ElementT));
}

void RegionDataProxyRemoteObject::processWriteCellMsg(WriteCellMessage* msg) {
  _regionData->writeCell(msg->coord, msg->value);
  this->send(&msg->value, sizeof(ElementT));
}

void RegionDataProxyRemoteObject::processAllocDataMsg(AllocDataMessage* /*msg*/) {
  int r = _regionData->allocData();
  this->send(&r, sizeof(int));
}

void RegionDataProxyRemoteObject::onRecv(const void* data, size_t len) {
  switch(*(MessageType*)data) {
  case MessageTypes::READCELL:
    JASSERT(len==sizeof(ReadCellMessage));
    this->processReadCellMsg((ReadCellMessage*)data);
    break;
  case MessageTypes::WRITECELL:
    JASSERT(len==sizeof(WriteCellMessage));
    this->processWriteCellMsg((WriteCellMessage*)data);
    break;
  case MessageTypes::ALLOCDATA:
    JASSERT(len==sizeof(AllocDataMessage));
    this->processAllocDataMsg((AllocDataMessage*)data);
    break;
  default:
    JASSERT(false)("Unknown RegionRemoteMsgTypes.");
  }
}
