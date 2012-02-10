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
        INITWITHREGIONDATA,
        INITWITHREGIONHANDLER,
        TOSCRATCHSTORAGE,
        FROMSCRATCHSTORAGE,
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

    struct MatrixRegionMetadata {
      int dimensions;
      IndexT startOffset;
      IndexT multipliers[];
      IndexT* size() const {
        return (IndexT*)((char*)this + sizeof(int) +
                         ((this->dimensions + 1) * sizeof(IndexT)));
      }
    } PACKED;

    struct RegionMatrixMetadata {
      int dimensions;
      int numSliceDimensions;
      IndexT splitOffset[];
      IndexT* size() const {
        return (IndexT*)((char*)this + sizeof(int) * 2 +
                         (sizeof(IndexT) * this->dimensions));
      }
      int* sliceDimensions() const {
        return (int*)((char*)size() +
                      (sizeof(IndexT) * this->dimensions));
      }
      IndexT* slicePositions() const {
        return (IndexT*)((char*)sliceDimensions() +
                         (sizeof(int) * this->numSliceDimensions));
      }
      static int len(int d, int numSlices) {
        return sizeof(int) * 2 + sizeof(IndexT) * 2 * d +
          (sizeof(int) + sizeof(IndexT)) * numSlices;
      }
    } PACKED;

    struct CopyToMatrixStorageMessage {
      struct RegionMatrixMetadata srcMetadata;
    } PACKED;

    struct CopyFromMatrixStorageMessage {
      struct RegionMatrixMetadata srcMetadata;
      ElementT* storage() const {
        return (ElementT*)((char*)this + RegionMatrixMetadata::len(srcMetadata.dimensions, srcMetadata.numSliceDimensions));
      }
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

    struct CopyToMatrixStorageReplyMessage {
      size_t count;
      ElementT storage[];
    } PACKED;

    struct CopyFromMatrixStorageReplyMessage {
    } PACKED;

    struct AllocDataReplyMessage {
      int result;
    } PACKED;

    struct RandomizeDataReplyMessage {
    } PACKED;

  }
}

#endif
