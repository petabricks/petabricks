#ifndef PETABRICKSREGIONREMOTE_H
#define PETABRICKSREGIONREMOTE_H

#include <map>
#include <pthread.h>
#include "regioni.h"
#include "remotehost.h"
#include "remoteobject.h"

namespace petabricks {
  class RegionRemote;
  typedef jalib::JRef<RegionRemote> RegionRemotePtr;

  class RegionRemote : public RegionI {
  protected:
    RemoteObjectPtr _remoteObject;
    pthread_mutex_t _seq_mux;
    pthread_mutex_t _buffer_mux;
    pthread_cond_t _buffer_cond;
    uint16_t _seq;
    uint16_t _recv_seq;
    std::map<uint16_t, void*> _buffer;
 
  public:
    RegionRemote(RemoteObjectPtr remoteObject);
    ~RegionRemote();

    static RemoteObjectPtr genLocal(RegionRemotePtr region);
    static RemoteObjectPtr genRemote();

    void setRemoteObject(RemoteObjectPtr remoteObject);

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


#endif
