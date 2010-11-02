#include "regionremote.h"

#include "remotehost.h"
#include "regionremoteproxy.h"

petabricks::RegionRemote::RegionRemote(RemoteObjectPtr remoteObject) {
  _remoteObject = remoteObject;
  _dimension = 3;

  pthread_mutex_init(&_seq_mux, NULL);
  _seq = 0;
  _recv_seq = 0;
}

petabricks::RegionRemote::~RegionRemote() {
    pthread_mutex_destroy(&_seq_mux);
}

petabricks::RemoteObjectPtr
petabricks::RegionRemote::genLocal() {
  class RegionRemoteObject : public petabricks::RemoteObject {
  public:


    void onRecv(const void* data, size_t len) {
      JTRACE("recv")(*(ElementT*)data)(len);
    }
  };
  return new RegionRemoteObject();
}

petabricks::RemoteObjectPtr
petabricks::RegionRemote::genRemote() {
  return new RegionRemoteProxy();
}

using namespace _RegionRemoteMsgTypes;

petabricks::ElementT
petabricks::RegionRemote::readCell(const IndexT* coord) {
  ReadCellMessage<3> msg; 
  msg.type = MessageTypes::REGIONREMOTE_READCELL;
  memmove(msg.coord, coord, (sizeof coord) * _dimension);

  pthread_mutex_lock(&_mux);
  _remoteObject->send(&msg, sizeof msg);
  uint16_t seq = ++_seq;
  pthread_mutex_unlock(&_mux);

  while () {
  }

  return 0;
}

void petabricks::RegionRemote::writeCell(const IndexT* coord, ElementT value) {
  // To be implemented
}

void petabricks::RegionRemote::markComplete() {
  _remoteObject->remoteNotify(1);
  _remoteObject->waitUntilComplete();
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

