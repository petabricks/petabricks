#ifndef PETABRICKSREGIONDATAREMOTE_H
#define PETABRICKSREGIONDATAREMOTE_H

#include <map>
#include <pthread.h>
#include "regiondatai.h"
#include "remoteobject.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {
  class RegionDataRemote;
  typedef jalib::JRef<RegionDataRemote> RegionDataRemotePtr;

  class RegionDataRemote : public RegionDataI {
  private:
    RemoteObjectPtr _remoteObject;
    pthread_mutex_t _seq_mux;
    pthread_mutex_t _buffer_mux;
    pthread_cond_t _buffer_cond;
    uint16_t _seq;
    uint16_t _recv_seq;
    std::map<uint16_t, void*> _buffer;

  public:
    RegionDataRemote(int dimensions, IndexT* size, RemoteObjectPtr remoteObject);
    ~RegionDataRemote();

    int allocData();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    void onRecv(const void* data, size_t len);
    void* fetchData(const void* msg, size_t len);

    static RemoteObjectPtr genRemote();
  };


  class RegionDataRemoteObject : public RemoteObject {
  protected:
    RegionDataRemotePtr _regionData;
  public:
    RegionDataRemoteObject() {}

    void onRecv(const void* data, size_t len) {
      _regionData->onRecv(data, len);
    }

    void onRecvInitial(const void* buf, size_t len);
  };


  namespace RegionDataRemoteMessage {
    typedef uint16_t MessageType;

    struct MessageTypes {
      enum {
	READCELL = 11,
	WRITECELL,
      };
    };

    struct InitialMessage {
      int dimensions;
      uint16_t movingBufferIndex;
      IndexT size[MAX_DIMENSIONS];
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
}

#endif
