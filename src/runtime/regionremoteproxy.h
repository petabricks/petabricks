#ifndef PETABRICKSREGIONREMOTEPROXY_H
#define PETABRICKSREGIONREMOTEPROXY_H

#include "regioni.h"
#include "remoteobject.h"


namespace _RegionRemoteMsgTypes {
  typedef uint16_t MessageType;

  struct MessageTypes {
    enum {
      REGIONREMOTE_READCELL = 11,
      REGIONREMOTE_WRITECELL,
    };
  };

  template <int D> struct ReadCellMessage {
    MessageType type;
    petabricks::IndexT coord[D]; 
  };

  template <int D> struct WriteCellMessage {
    MessageType type;
    petabricks::ElementT value;
    petabricks::IndexT coord[D];
  };
}

using namespace _RegionRemoteMsgTypes;

namespace petabricks {
  class RegionRemoteProxy : public RemoteObject {
  protected:
    RegionIPtr _referenceRegion;
 
  public:
    RegionRemoteProxy();

    void onNotify(int argc);
    void onRecv(const void* data, size_t len);

    void readCell(ReadCellMessage<3>* msg);
    void writeCell(WriteCellMessage<3>* msg);
  };
}


#endif
