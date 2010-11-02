#ifndef PETABRICKSREGIONREMOTE_H
#define PETABRICKSREGIONREMOTE_H

#include <pthread.h>
#include "regioni.h"
#include "remotehost.h"
#include "remoteobject.h"

namespace petabricks {
  class RegionRemote : public RegionI {
  protected:
    RemoteObjectPtr _remoteObject;
    pthread_mutex_t _seq_mux;
    uint16_t _seq;
    uint16_t _recv_seq;
 
  public:
    RegionRemote(RemoteObjectPtr remoteObject);
    ~RegionRemote();

    static RemoteObjectPtr genLocal();
    static RemoteObjectPtr genRemote();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);
    void markComplete();

    RegionIPtr regionContiguous();
    ElementT* coordToPtr(const IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);
  };
}


#endif
