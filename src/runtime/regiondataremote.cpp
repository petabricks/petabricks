#include "regiondataremote.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRemote::RegionDataRemote(int dimensions, IndexT* size, RemoteObjectPtr remoteObject) {
  _D = dimensions;
  _size = size;
  _remoteObject = remoteObject;

  pthread_mutex_init(&_seq_mux, NULL);
  pthread_mutex_init(&_buffer_mux, NULL);
  pthread_cond_init(&_buffer_cond, NULL);
  _seq = 0;
  _recv_seq = 0;
}

RegionDataRemote::~RegionDataRemote() {
  pthread_mutex_destroy(&_seq_mux);
  pthread_mutex_destroy(&_buffer_mux);
  pthread_cond_destroy(&_buffer_cond);
}

void* RegionDataRemote::fetchData(const void* msg, size_t len) {
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

ElementT RegionDataRemote::readCell(const IndexT* coord) {
  ReadCellMessage* msg = new ReadCellMessage();
  msg->type = MessageTypes::READCELL;
  memcpy(msg->coord, coord, _D * sizeof(IndexT));

  ElementT elmt = *(ElementT*)this->fetchData(msg, sizeof *msg);

  delete msg;
  return elmt;
}

void RegionDataRemote::writeCell(const IndexT* coord, ElementT value) {
  WriteCellMessage* msg = new WriteCellMessage();
  msg->type = MessageTypes::WRITECELL;
  msg->value = value;
  memcpy(msg->coord, coord, _D * sizeof(IndexT));

  ElementT elmt = *(ElementT*)this->fetchData(msg, sizeof *msg);

  delete msg;
  JASSERT(elmt == value);
}

void RegionDataRemote::onRecv(const void* data, size_t len) {
  JTRACE("recv")(*(ElementT*)data)(len);
  void* x = malloc(len);
  memmove(x, data, len);
  _buffer[++_recv_seq] = x;
  pthread_cond_broadcast(&_buffer_cond);
}

RemoteObjectPtr RegionDataRemote::genRemote() {
  return new RegionDataRemoteObject();
}

//
// RegionDataRemoteObject
//

void RegionDataRemoteObject::onRecvInitial(const void* buf, size_t len) {
  InitialMessage* msg = (InitialMessage*) buf;

  IndexT* size = (IndexT*)malloc(sizeof(IndexT) * msg->dimensions);
  memcpy(size, msg->size, sizeof(IndexT) * msg->dimensions);

  _regionData = new RegionDataRemote(msg->dimensions, size, this);

  extern RegionDataIPtr remoteRegionData;
  remoteRegionData = (RegionDataIPtr) _regionData.asPtr();
}

