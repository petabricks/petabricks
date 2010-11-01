#include "regionremote.h"

#include "remotehost.h"
#include "regionremoteproxy.h"

petabricks::RegionRemote::RegionRemote(RemoteObjectPtr remoteObject) {
  _remoteObject = remoteObject;
  _dimension = 3;
}

petabricks::RemoteObjectPtr
petabricks::RegionRemote::genLocal() {
  class RegionRemoteObject : public petabricks::RemoteObject {
  public:
    void onRecv(const void* data, size_t len) {
      JTRACE("recv")((char*)data)(len);
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
  // To be implemented
  ReadCellMessage<3> msg; 
  msg.type = MessageTypes::REGIONREMOTE_READCELL;
  memmove(msg.coord, coord, (sizeof coord) * _dimension); 
  void* x;
  x = &msg.type;

  char testdata[] = "this is a test string";
  //_remoteObject->send(testdata, sizeof testdata);

  _remoteObject->remoteNotify(0);

  _remoteObject->send(x, sizeof msg);
  _remoteObject->send(x, sizeof msg);
  _remoteObject->send(x, sizeof msg);

  int i = 1234;
  _remoteObject->send(&i, sizeof i);

  _remoteObject->remoteSignal();
  _remoteObject->remoteBroadcast();

  _remoteObject->remoteNotify(1);
  _remoteObject->waitUntilComplete();
  return 0;
}

void petabricks::RegionRemote::writeCell(const IndexT* coord, ElementT value) {
  // To be implemented
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

