#include "regionremoteproxy.h"

#include "matrixio.h"

using namespace petabricks;

petabricks::RegionRemoteProxy::RegionRemoteProxy() {
  MatrixIO* matrixio = new MatrixIO("testdata/Helmholtz3DB1", "r");
  _referenceRegion = matrixio->readToRegionI();
}

void RegionRemoteProxy::onNotify(int argc) {
  JTRACE("notify")(argc);
  if(argc==1) {
    markComplete();
  }
}

using namespace _RegionRemoteMsgTypes;

void RegionRemoteProxy::onRecv(const void* data, size_t len) {
  switch(*(MessageType*)data) {
  case MessageTypes::REGIONREMOTE_READCELL:
    readCell((ReadCellMessage<3>*)data);
    break;
  case MessageTypes::REGIONREMOTE_WRITECELL:
    writeCell((WriteCellMessage<3>*)data);
    break;
  default:
    throw("Unknown RegionRemoteMsgTypes.");
  }
}

void RegionRemoteProxy::readCell(struct ReadCellMessage<3>* msg) {
  ElementT* cell = _referenceRegion->coordToPtr(msg->coord);
  JTRACE("read")(*cell);
  send(cell, sizeof cell);
}
 
void RegionRemoteProxy::writeCell(struct WriteCellMessage<3>* msg) {
  JTRACE("write")(msg->type);
}
