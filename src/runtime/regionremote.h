#ifndef PETABRICKSREGIONREMOTE_H
#define PETABRICKSREGIONREMOTE_H

#include <map>
#include <pthread.h>
#include "regioni.h"
#include "remotehost.h"
#include "regionremoteproxy.h"
#include "remoteobject.h"

namespace petabricks {

  template <int D> class RegionRemote : public RegionI {
    typedef jalib::JRef<RegionRemote> RegionRemotePtr;

  protected:
    RemoteObjectPtr _remoteObject;
    pthread_mutex_t _seq_mux;
    pthread_mutex_t _buffer_mux;
    pthread_cond_t _buffer_cond;
    uint16_t _seq;
    uint16_t _recv_seq;
    std::map<uint16_t, void*> _buffer;
 
  public:
    RegionRemote();
    ~RegionRemote();

    static RemoteObjectPtr genLocal(RegionRemotePtr region);
    static RemoteObjectPtr genRemote();

    void setRemoteObject(RemoteObjectPtr remoteObject);

    void* fetchData(const void* msg, size_t len);
    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);
    void onRecv(const void* data, size_t len);
    void markComplete();

    RegionIPtr regionContiguous();
    ElementT* coordToPtr(const IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);
  };
}

/* implementation */
using namespace petabricks;

template <int D>
RegionRemote<D>::RegionRemote() {
  _dimension = D;

  pthread_mutex_init(&_seq_mux, NULL);
  pthread_mutex_init(&_buffer_mux, NULL);
  pthread_cond_init(&_buffer_cond, NULL);
  _seq = 0;
  _recv_seq = 0;
}

template <int D>
RegionRemote<D>::~RegionRemote() {
  pthread_mutex_destroy(&_seq_mux);
  pthread_mutex_destroy(&_buffer_mux);
  pthread_cond_destroy(&_buffer_cond);
}

template <int D>
RemoteObjectPtr RegionRemote<D>::genLocal(RegionRemotePtr region) {
  class RegionRemoteObject : public RemoteObject {
  protected:
    RegionRemotePtr _region;
  public:
    RegionRemoteObject(RegionRemotePtr region) {
      _region = region;
    }

    void onRecv(const void* data, size_t len) {
      _region->onRecv(data, len);
    }
  };
  return new RegionRemoteObject(region);
}

template <int D>
RemoteObjectPtr RegionRemote<D>::genRemote() {
  return new RegionRemoteProxy<D>();
}

template <int D>
void RegionRemote<D>::setRemoteObject(RemoteObjectPtr remoteObject) {
  _remoteObject = remoteObject;
}

using namespace _RegionRemoteMsgTypes;

template <int D>
void* RegionRemote<D>::fetchData(const void* msg, size_t len) {
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

template <int D>
ElementT RegionRemote<D>::readCell(const IndexT* coord) {
  ReadCellMessage<D>* msg = (ReadCellMessage<D>*) malloc(sizeof(ReadCellMessage<D>) + (sizeof coord)*D); 
  msg->type = MessageTypes::REGIONREMOTE_READCELL;
  memcpy(msg->coord, coord, (sizeof coord) * D);
  
  ElementT elmt = *(ElementT*)this->fetchData(msg, sizeof(ReadCellMessage<D>));

  delete msg;
  return elmt;
}

template <int D>
void RegionRemote<D>::writeCell(const IndexT* coord, ElementT value) {
  WriteCellMessage<D>* msg = (WriteCellMessage<D>*) malloc(sizeof(WriteCellMessage<D>) + (sizeof coord)*D); 
  msg->type = MessageTypes::REGIONREMOTE_WRITECELL;
  msg->value = value;
  memcpy(msg->coord, coord, (sizeof coord) * _dimension);

  ElementT elmt = *(ElementT*)this->fetchData(msg, sizeof(WriteCellMessage<D>));

  delete msg;
  JASSERT(elmt==value);
}

template <int D>
void RegionRemote<D>::markComplete() {
  _remoteObject->remoteNotify(1);
  _remoteObject->waitUntilComplete();
}

template <int D>
void RegionRemote<D>::onRecv(const void* data, size_t len) {
  JTRACE("recv")(*(ElementT*)data)(len);
  void* x = malloc(len);
  memmove(x, data, len);
  _buffer[++_recv_seq] = x;
  pthread_cond_broadcast(&_buffer_cond);
}

template <int D>
RegionIPtr RegionRemote<D>::regionContiguous() {
  // To be implemented
  return NULL;
}

template <int D> petabricks::ElementT*
RegionRemote<D>::coordToPtr(const IndexT* coord) {
  // To be implemented
  return NULL;
}
    
template <int D> petabricks::RegionIPtr
RegionRemote<D>::splitRegion(IndexT* offset, IndexT* size) {
  // To be implemented
  return NULL;
}

template <int D> petabricks::RegionIPtr
RegionRemote<D>::sliceRegion(int d, IndexT pos) {
  // To be implemented
  return NULL;
}

#endif
