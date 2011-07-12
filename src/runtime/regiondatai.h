#ifndef PETABRICKSREGIONDATAI_H
#define PETABRICKSREGIONDATAI_H

#include "common/jrefcounted.h"
#include "matrixstorage.h"
#include "remotehost.h"

#include <vector>

namespace petabricks {
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  typedef uint8_t RegionDataType;
  struct RegionDataTypes {
    enum {
      REGIONDATA0D = 37,
      CONSTREGIONDATA0D,
      REGIONDATAPROXY,
      REGIONDATARAW,
      REGIONDATAREMOTE,
      REGIONDATASPLIT
    };
  };

  struct DataHostListItem {
    HostPid hostPid;
    double weight;
  };
  typedef std::vector<DataHostListItem> DataHostList;

  //
  // RegionDataRemoteMessage
  //

  namespace RegionDataRemoteMessage {
    typedef uint16_t MessageType;

    struct MessageTypes {
      enum {
	READCELL = 11,
	WRITECELL,
	GETHOSTLIST,
	UPDATEHANDLERCHAIN,
        ALLOCDATA,
      };
    };

    struct InitialMessageToRegionDataRemote {
      int dimensions;
      IndexT size[MAX_DIMENSIONS];
    };

    struct InitialMessageToRegionMatrixProxy {
      int dimensions;
      IndexT size[MAX_DIMENSIONS];
      IndexT partOffset[MAX_DIMENSIONS];
    };

    struct BaseMessageHeader {
      bool isForwardMessage;
      MessageType type;
      size_t contentOffset;

      char* content() const { return (char*)this + contentOffset; }
    };

    struct GeneralMessageHeader {
      bool isForwardMessage;
      MessageType type;
      size_t contentOffset;
      EncodedPtr responseData;
      EncodedPtr responseLen;

      char* content() const { return (char*)this + contentOffset; }
    };

    struct ForwardMessageHeader {
      bool isForwardMessage;
      MessageType type;
      size_t contentOffset;
      EncodedPtr callback; // RegionMatrixProxy*

      char* content() const { return (char*)this + contentOffset; }
    };

    struct ReadCellMessage {
      IndexT coord[MAX_DIMENSIONS];
    };

    struct WriteCellMessage {
      ElementT value;
      IndexT coord[MAX_DIMENSIONS];
    };

    struct GetHostListMessage {
      IndexT begin[MAX_DIMENSIONS];
      IndexT end[MAX_DIMENSIONS];
    };

    struct UpdateHandlerChainMessage {
      HostPid requester;
      int numHops;
    };

    struct AllocDataMessage {
    };

    struct ReadCellReplyMessage {
      ElementT value;
    };

    struct WriteCellReplyMessage {
      ElementT value;
    };

    struct GetHostListReplyMessage {
      int numHosts;
      DataHostListItem hosts[];
    };

    struct UpdateHandlerChainReplyMessage {
      HostPid dataHost;
      int numHops;
      EncodedPtr encodedPtr; // regiondata or remoteobject
    };

    struct AllocDataReplyMessage {
      int result;
    };
  }

  using namespace petabricks::RegionDataRemoteMessage;

  //
  // RegiondataI
  //

  class RegionDataI;
  typedef jalib::JRef<RegionDataI> RegionDataIPtr;

  class RegionDataI : public jalib::JRefCounted {
  protected:
    int _D;
    RegionDataType _type;

    // _size is the size of this part, not the entire region
    IndexT _size[MAX_DIMENSIONS];

  public:
    virtual int allocData() = 0;

    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    virtual MatrixStoragePtr storage() const {
      JASSERT(false)(_type).Text("This should not be called.");
      return NULL;
    }

    // for toLocalRegion
    virtual ElementT& value0D(const IndexT* /*coord*/) const {
      JASSERT(false)(_type).Text("This should not be called.");
      throw;
    }

    int dimensions();
    IndexT* size();

    RegionDataType type() const {
      return _type;
    }

    virtual DataHostList hosts(IndexT* begin, IndexT* end) = 0;


    // Process Remote Messages
    virtual void processReadCellMsg(ReadCellMessage* msg, ReadCellReplyMessage* reply, int* len);
    virtual void processWriteCellMsg(WriteCellMessage* msg, WriteCellReplyMessage* reply, int* len);
    virtual void processGetHostListMsg(GetHostListMessage* msg, GetHostListReplyMessage* reply, int* len);
    virtual void processAllocDataMsg(AllocDataMessage* msg, AllocDataReplyMessage* reply, int* len);

    // for tests
  private:
    int incCoord(IndexT* coord);
  public:
    virtual void print();
  };
}

#endif
