#ifndef PETABRICKSREGIONREMOTE_H
#define PETABRICKSREGIONREMOTE_H

#include "regioni.h"
#include "remotehost.h"
#include "remoteobject.h"

namespace petabricks {
  class RegionRemote : public RegionI {

  protected:
    RemoteObjectPtr _remoteObject;
 
  public:
    RegionRemote(RemoteObjectPtr remoteObject);
    static RemoteObjectPtr genLocal();
    static RemoteObjectPtr genRemote();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    RegionIPtr regionContiguous();
    ElementT* coordToPtr(const IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);
  };
}


#endif
