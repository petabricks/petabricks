#ifndef PETABRICKSREGIONDATAPROXY_H
#define PETABRICKSREGIONDATAPROXY_H

#include <map>
#include <pthread.h>
#include "regiondataraw.h"
#include "remotehost.h"
#include "remoteobject.h"

namespace petabricks {
  namespace RegionDataProxyMessage {
    typedef uint16_t MessageType;

    struct MessageTypes {
      enum {
	READCELL = 11,
	WRITECELL,
	ALLOCDATA,
      };
    };

    struct InitialMessage {
      int dimensions;
      IndexT size[MAX_DIMENSIONS];
      IndexT partOffset[MAX_DIMENSIONS];
    };

    struct AllocDataMessage {
      MessageType type;
    };

    struct ReadCellMessage {
      MessageType type;
      IndexT coord[MAX_DIMENSIONS];
    };
    
    struct WriteCellMessage {
      MessageType type;
      ElementT value;
      IndexT coord[MAX_DIMENSIONS]; 
    };
  }

  class RegionDataProxy;
  typedef jalib::JRef<RegionDataProxy> RegionDataProxyPtr;
  
  class RegionDataProxy : public RegionDataI {
    RemoteObjectPtr _remoteObject;

    IndexT* _partOffset;

    pthread_mutex_t _seq_mux;
    pthread_mutex_t _buffer_mux;
    pthread_cond_t _buffer_cond;
    uint16_t _seq;
    uint16_t _recv_seq;
    std::map<uint16_t, void*> _buffer;

  public:
    RegionDataProxy(int dimensions, IndexT* size, IndexT* partOffset, RemoteHostPtr host);
    ~RegionDataProxy();

    int allocData();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    void onRecv(const void* data, size_t len);
    void* fetchData(const void* msg, size_t len);

    RemoteObjectPtr genLocal();
    static RemoteObjectPtr genRemote();
  };

  class RegionDataProxyRemoteObject : public RemoteObject {
  protected:
    RegionDataRawPtr _regionData;
  public:
    RegionDataProxyRemoteObject() {}

    void onRecv(const void* data, size_t len);
    void onRecvInitial(const void* buf, size_t len);

  private:
    void processReadCellMsg(RegionDataProxyMessage::ReadCellMessage* msg);
    void processWriteCellMsg(RegionDataProxyMessage::WriteCellMessage* msg);
    void processAllocDataMsg(RegionDataProxyMessage::AllocDataMessage* msg);
  };


}

#endif
