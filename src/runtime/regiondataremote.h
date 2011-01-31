#ifndef PETABRICKSREGIONDATAREMOTE_H
#define PETABRICKSREGIONDATAREMOTE_H

#include "regiondatai.h"
#include "remoteobject.h"

namespace petabricks {
  class RegionDataRemote;
  typedef jalib::JRef<RegionDataRemote> RegionDataRemotePtr;

  extern RegionDataIPtr remoteRegionData;

  class RegionDataRemote : public RegionDataI {
  private:
    RemoteObjectPtr _remoteObject;

  public:
    RegionDataRemote(int dimensions, RemoteObjectPtr remoteObject);
    ~RegionDataRemote() {};

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

    void onRecv(const void* data, size_t len);

    static RemoteObjectPtr genRemote();
  };


  class RegionDataRemoteObject : public RemoteObject {
  protected:
    RegionDataRemotePtr _regionData;
  public:
    RegionDataRemoteObject() {
      printf("RegionDataRemoteObject %d\n", getpid());
    }

    void onRecv(const void* data, size_t len) {
      _regionData->onRecv(data, len);
    }

    void onRecvInitial(const void* buf, size_t len) {
      printf("initial\n");

      // TODO: dimensions
      _regionData = new RegionDataRemote(3, this);
      

      extern RegionDataIPtr remoteRegionData;
      remoteRegionData = (RegionDataIPtr) _regionData.asPtr();
    }
  };


  namespace RegionDataRemoteMessage {
    typedef uint16_t MessageType;

    struct MessageTypes {
      enum {
	READCELL = 11,
	WRITECELL,
      };
    };

    struct ReadCellMessage {
      static const MessageType type = MessageTypes::READCELL;
      std::vector<IndexT> coord; 
    };
    
    struct WriteCellMessage {
      static const MessageType type = MessageTypes::WRITECELL;
      ElementT value;
      std::vector<IndexT> coord;
    };
  }
}

#endif
