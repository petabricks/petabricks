#ifndef PETABRICKSREGIONREMOTEPROXY_H
#define PETABRICKSREGIONREMOTEPROXY_H

#include "regioni.h"
#include "remoteobject.h"
#include "matrixio.h"

namespace _RegionRemoteMsgTypes {
  typedef uint16_t MessageType;

  struct MessageTypes {
    enum {
      REGIONREMOTE_READCELL = 11,
      REGIONREMOTE_WRITECELL,
      REGIONREMOTE_GETSIZE,
    };
  };

  template<int D> struct ReadCellMessage {
    MessageType type;
    petabricks::IndexT coord[D]; 
  };

  template<int D> struct WriteCellMessage {
    MessageType type;
    petabricks::ElementT value;
    petabricks::IndexT coord[D];
  };
}

using namespace _RegionRemoteMsgTypes;

namespace petabricks {
  template<int D> class RegionRemoteProxy : public RemoteObject {
  protected:
    RegionIPtr _referenceRegion;
 
  public:
    RegionRemoteProxy();

    void onRecvInitial(const void* buf, size_t len);
 
    void onNotify(int argc);
    void onRecv(const void* data, size_t len);

    void readCell(ReadCellMessage<D>* msg);
    void writeCell(WriteCellMessage<D>* msg);
  };
}

/* implementation */
using namespace petabricks;

template<int D> 
RegionRemoteProxy<D>::RegionRemoteProxy() {
}

template<int D> 
void RegionRemoteProxy<D>::onRecvInitial(const void* buf, size_t len) {
  // read from file
  if (len > 0) {
    MatrixIO* matrixio = new MatrixIO((char*)buf, "r");
    _referenceRegion = matrixio->readToRegionI();
  }
}

template<int D> 
void RegionRemoteProxy<D>::onNotify(int argc){
  JTRACE("notify")(argc);
  if(argc==1) {
    markComplete();
  }
}

template<int D> 
void RegionRemoteProxy<D>::onRecv(const void* data, size_t len) {
  switch(*(MessageType*)data) {
  case MessageTypes::REGIONREMOTE_READCELL:
    readCell((ReadCellMessage<D>*)data);
    break;
  case MessageTypes::REGIONREMOTE_WRITECELL:
    writeCell((WriteCellMessage<D>*)data);
    break;
  default:
    throw("Unknown RegionRemoteMsgTypes.");
  }
}

template<int D> 
void RegionRemoteProxy<D>::readCell(ReadCellMessage<D>* msg) {
  ElementT* cell = _referenceRegion->coordToPtr(msg->coord);
  JTRACE("read")(*cell);
  send(cell, sizeof cell);
}

template<int D> 
void RegionRemoteProxy<D>::writeCell(WriteCellMessage<D>* msg) {
  ElementT* cell = _referenceRegion->coordToPtr(msg->coord);
  *cell = msg->value;
  send(cell, sizeof cell);
}

#endif

