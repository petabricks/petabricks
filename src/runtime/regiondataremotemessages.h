#ifndef PETABRICKSREGIONDATAREMOTEMESSAGE_H
#define PETABRICKSREGIONDATAREMOTEMESSAGE_H

#include <vector>

#include "remotehost.h"

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
        INITFROMDATASPLIT,
        INITWITHREGIONDATA,
        INITWITHREGIONHANDLER,
      };
    };

    struct InitialMessageToRegionDataRemote {
      int dimensions;
      IndexT size[MAX_DIMENSIONS];
    };

    struct InitialMessageToRegionMatrixProxy {
      MessageType type;
      int dimensions;
      EncodedPtr encodedPtr;
      IndexT size[MAX_DIMENSIONS];
      IndexT partOffset[MAX_DIMENSIONS];
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
      char* next() const { return (char*)this + sizeof(ForwardMessageHeader); }
    };

    struct BaseMessageHeader {
      bool isForwardMessage;
      MessageType type;
      size_t contentOffset;

      char* content() const { return (char*)this + contentOffset; }

      char* next() const {
        if (isForwardMessage) {
          return (char*)this + sizeof(ForwardMessageHeader);
        } else {
          return (char*)this + sizeof(GeneralMessageHeader);
        }
      }
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
}

#endif
