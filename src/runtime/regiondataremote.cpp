#include "regiondataremote.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRemote::RegionDataRemote(int dimensions, RemoteObjectPtr remoteObject) {
  _D = dimensions;
  _remoteObject = remoteObject;
}

ElementT RegionDataRemote::readCell(const IndexT* coord) {
  ReadCellMessage* msg = new ReadCellMessage();
  msg->coord.insert(msg->coord.begin(), coord, coord + _D);
  
  //ElementT elmt = *(ElementT*)this->fetchData(msg, (sizeof msg));

  //  delete msg;
  //return elmt;

  return NULL;
}

void RegionDataRemote::writeCell(const IndexT* coord, ElementT value) {

}

void RegionDataRemote::onRecv(const void* data, size_t len) {

}

RemoteObjectPtr RegionDataRemote::genRemote() {
  return new RegionDataRemoteObject();
}
