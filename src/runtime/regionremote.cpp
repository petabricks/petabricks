#include "regionremote.h"

#include "remotehost.h"
#include "regionremoteproxy.h"

petabricks::RegionRemote::RegionRemote() {
  _dimension = D;

  pthread_mutex_init(&_seq_mux, NULL);
  pthread_mutex_init(&_buffer_mux, NULL);
  pthread_cond_init(&_buffer_cond, NULL);
  _seq = 0;
  _recv_seq = 0;
}

petabricks::RegionRemote::~RegionRemote() {
    pthread_mutex_destroy(&_seq_mux);
    pthread_mutex_destroy(&_buffer_mux);
    pthread_cond_destroy(&_buffer_cond);
}

petabricks::RemoteObjectPtr
petabricks::RegionRemote::genLocal(RegionRemotePtr region) {
  class RegionRemoteObject : public petabricks::RemoteObject {
  protected:
    RegionRemotePtr _region;
  public:
    RegionRemoteObject(RegionRemotePtr region) {
      _region = region;
    }

    void onRecv(const void* data, size_t len) {
      JTRACE("recv")(*(ElementT*)data)(len);
      _region->onRecv(data, len);
    }
  };
  return new RegionRemoteObject(region);
}

petabricks::RemoteObjectPtr
petabricks::RegionRemote::genRemote() {
  return new RegionRemoteProxy<D>();
}

void petabricks::RegionRemote::setRemoteObject(RemoteObjectPtr remoteObject) {
  _remoteObject = remoteObject;
}

using namespace _RegionRemoteMsgTypes;

petabricks::ElementT
petabricks::RegionRemote::readCell(const IndexT* coord) {
  ReadCellMessage<D>* msg = (ReadCellMessage<D>*) malloc(sizeof(ReadCellMessage<D>)); 
  msg->type = MessageTypes::REGIONREMOTE_READCELL;
  memmove(msg->coord, coord, (sizeof coord) * _dimension);

  pthread_mutex_lock(&_seq_mux);
  _remoteObject->send(msg, sizeof(ReadCellMessage<D>));
  uint16_t seq = ++_seq;
  pthread_mutex_unlock(&_seq_mux);
  
  delete msg;

  // wait for the data
  pthread_mutex_lock(&_buffer_mux);
  while (seq > _recv_seq) {
    pthread_cond_wait(&_buffer_cond, &_buffer_mux);
  }
  ElementT elmt = *(ElementT*)_buffer[seq];
  _buffer.erase(seq);
  pthread_mutex_unlock(&_buffer_mux);

  // wake other threads
  pthread_cond_broadcast(&_buffer_cond);
 
  return elmt;
}

void petabricks::RegionRemote::writeCell(const IndexT* coord, ElementT value) {
  WriteCellMessage<D>* msg = (WriteCellMessage<D>*) malloc(sizeof(WriteCellMessage<D>)); 
  msg->type = MessageTypes::REGIONREMOTE_WRITECELL;
  msg->value = value;
  memmove(msg->coord, coord, (sizeof coord) * _dimension);

  pthread_mutex_lock(&_seq_mux);
  _remoteObject->send(msg, sizeof(WriteCellMessage<D>));
  uint16_t seq = ++_seq;
  pthread_mutex_unlock(&_seq_mux);
  
  delete msg;

  // wait for the data
  pthread_mutex_lock(&_buffer_mux);
  while (seq > _recv_seq) {
    pthread_cond_wait(&_buffer_cond, &_buffer_mux);
  }
  JASSERT(*(ElementT*)_buffer[seq]==value);
  _buffer.erase(seq);
  pthread_mutex_unlock(&_buffer_mux);

  // wake other threads
  pthread_cond_broadcast(&_buffer_cond);
}

void petabricks::RegionRemote::markComplete() {
  _remoteObject->remoteNotify(1);
  _remoteObject->waitUntilComplete();
}

void petabricks::RegionRemote::onRecv(const void* data, size_t len) {
  JTRACE("recv")(*(ElementT*)data)(len);
  void* x = malloc(len);
  memmove(x, data, len);
  _buffer[++_recv_seq] = x;
  pthread_cond_broadcast(&_buffer_cond);
}

petabricks::RegionIPtr 
petabricks::RegionRemote::regionContiguous() {
  // To be implemented
  return NULL;
}

petabricks::ElementT*
petabricks::RegionRemote::coordToPtr(const IndexT* coord) {
  // To be implemented
  return NULL;
}
    
petabricks::RegionIPtr
petabricks::RegionRemote::splitRegion(IndexT* offset, IndexT* size) {
  // To be implemented
  return NULL;
}

petabricks::RegionIPtr
petabricks::RegionRemote::sliceRegion(int d, IndexT pos) {
  // To be implemented
  return NULL;
}

