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
  } PACKED;

  struct DataHostPidListItem {
    HostPid hostPid;
    double weight;
  } PACKED;
  typedef std::vector<DataHostPidListItem> DataHostPidList;

  //
  // RegionDataRemoteMessage
  //

  namespace RegionDataRemoteMessage {
    typedef uint8_t MessageType;

    struct MessageTypes {
      enum {
	READCELL = 11,
	WRITECELL,
	READCELLCACHE,
	WRITECELLCACHE,
	GETHOSTLIST,
	UPDATEHANDLERCHAIN,
        ALLOCDATA,
        RANDOMIZEDATA,
        CREATEREGIONDATA,
        CREATEREGIONDATAPART,
        INITWITHREGIONDATA,
        INITWITHREGIONHANDLER,
      };
    } PACKED;

    struct GeneralInitialMessage {
      MessageType type;
    } PACKED;

    struct CreateRegionDataInitialMessage {
      MessageType type;
      int dimensions;
      IndexT size[];
    } PACKED;

    struct CreateRegionDataPartInitialMessage {
      MessageType type;
      int dimensions;
      IndexT size[MAX_DIMENSIONS];
      IndexT partOffset[MAX_DIMENSIONS];
    } PACKED;

    struct EncodedPtrInitialMessage {
      MessageType type;
      EncodedPtr encodedPtr;
    } PACKED;

    struct GeneralMessageHeader {
      bool isForwardMessage;
      MessageType type;
      size_t contentOffset;
      EncodedPtr responseData;
      EncodedPtr responseLen;

      char* content() const { return (char*)this + contentOffset; }
    } PACKED;

    struct ForwardMessageHeader {
      bool isForwardMessage;
      MessageType type;
      size_t contentOffset;
      EncodedPtr callback; // RegionMatrixProxy*

      char* content() const { return (char*)this + contentOffset; }
      char* next() const { return (char*)this + sizeof(ForwardMessageHeader); }
    } PACKED;

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
    } PACKED;

    struct ReadCellMessage {
      IndexT coord[];
    } PACKED;

    struct WriteCellMessage {
      ElementT value;
      IndexT coord[];
    } PACKED;

    struct ReadCellCacheMessage {
      size_t cacheLineSize;
      IndexT coord[];
    } PACKED;

    struct WriteCellCacheMessage {
      size_t cacheLineSize;
      ElementT value;
      IndexT coord[];
    } PACKED;

    struct GetHostListMessage {
      IndexT begin[MAX_DIMENSIONS];
      IndexT end[MAX_DIMENSIONS];
    } PACKED;

    struct UpdateHandlerChainMessage {
      HostPid requester;
      int numHops;
    } PACKED;

    struct AllocDataMessage {
    } PACKED;

    struct RandomizeDataMessage {
    } PACKED;

    struct ReadCellReplyMessage {
      ElementT value;
    } PACKED;

    struct WriteCellReplyMessage {
      ElementT value;
    } PACKED;

    struct ReadCellCacheReplyMessage {
      IndexT start;
      IndexT end;
      ElementT values[];
    } PACKED;

    struct WriteCellCacheReplyMessage {
      IndexT start;
      IndexT end;
      ElementT values[];
    } PACKED;

    struct GetHostListReplyMessage {
      int numHosts;
      DataHostPidListItem hosts[];
    } PACKED;

    struct UpdateHandlerChainReplyMessage {
      HostPid dataHost;
      int numHops;
      EncodedPtr encodedPtr; // regiondata or remoteobject
    } PACKED;

    struct AllocDataReplyMessage {
      int result;
    } PACKED;

    struct RandomizeDataReplyMessage {
    } PACKED;

  }
}

#endif
