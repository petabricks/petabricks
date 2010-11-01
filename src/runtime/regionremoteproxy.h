#ifndef PETABRICKSREGIONREMOTEPROXY_H
#define PETABRICKSREGIONREMOTEPROXY_H

#include "regioni.h"
#include "remoteobject.h"

namespace petabricks {
  class RegionRemoteProxy : public RemoteObject {
  protected:
    RegionIPtr _referenceRegion;
 
  public:
    RegionRemoteProxy();

    void onNotify(int argc);
    void onRecv(const void* data, size_t len);
  };

}

namespace _RegionRemoteMsgTypes {
  typedef int MessageType;

  struct MessageTypes {
    enum {
      REGIONREMOTE_READCELL,
      REGIONREMOTE_WRITECELL,
    };
  };

  template <int D> struct ReadCellMessage {
    MessageType type;
    petabricks::ElementT coord[D]; 
  };

  template <int D> struct WriteCellMessage {
    MessageType type;
    petabricks::ElementT coord[D];
    petabricks::ElementT value;
  };

}

#endif
