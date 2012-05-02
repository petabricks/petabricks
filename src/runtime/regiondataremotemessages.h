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

  typedef int RegionDataDistribution;
  struct RegionDataDistributions {
    enum {
      LOCAL = 0,
      ONE_REMOTE,
      N_BY_ROW,
      N_BY_COL,
      N_BY_BLOCK
    };
  } PACKED;

  typedef int RegionDataMigrationType;
  struct RegionDataMigrationTypes {
    enum {
      NONE = 0,
      IGNORE,
      COPY_ENTIRE_DATA
    };
  } PACKED;

  struct DataHostPidListItem {
    HostPid hostPid;
    size_t weight;
  } PACKED;
  typedef std::vector<DataHostPidListItem> DataHostPidList;

  struct RemoteRegionHandler {
    HostPid hostPid;
    EncodedPtr remoteHandler;
  } PACKED;

  //
  // RegionDataRemoteMessage
  //

  namespace RegionDataRemoteMessage {
    typedef int MessageType;

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
        CREATEREMOTEREGIONDATA,
        CREATEREMOTEREGIONDATAREPLY,
        INITWITHREGIONHANDLER,
        TOSCRATCHSTORAGE,
        FROMSCRATCHSTORAGE,
        COPYREGIONDATASPLIT,
      };
    } PACKED;

    struct GeneralInitialMessage {
      MessageType type;
    } PACKED;

    struct CreateRegionDataInitialMessage {
      MessageType type;
      int dimensions;
      IndexT size[];
    private:
      CreateRegionDataInitialMessage() {}
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
      EncodedPtr responseType;
      EncodedPtr responseCounter;

      char* content() const { return (char*)this + contentOffset; }
    } PACKED;

    struct ForwardMessageHeader {
      bool isForwardMessage;
      MessageType type;
      size_t contentOffset;
      EncodedPtr callback; // RegionMatrixProxy*

      char* content() const { return (char*)this + contentOffset; }
      char* next() const { return (char*)this + sizeof(ForwardMessageHeader); }
    private:
      ForwardMessageHeader() {}
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
    private:
      BaseMessageHeader() {}
    } PACKED;

    struct ReadCellMessage {
      IndexT coord[];
    private:
      ReadCellMessage() {}
    } PACKED;

    struct WriteCellMessage {
      ElementT value;
      IndexT coord[];
    private:
      WriteCellMessage() {}
    } PACKED;

    struct ReadCellCacheMessage {
      size_t cacheLineSize;
      IndexT coord[];
    private:
      ReadCellCacheMessage() {}
    } PACKED;

    struct WriteCellCacheMessage {
      size_t cacheLineSize;
      ElementT value;
      IndexT coord[];
    private:
      WriteCellCacheMessage() {}
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
    private:
      MatrixRegionMetadata() {}
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
    private:
      RegionMatrixMetadata() {}
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

    struct CopyRegionDataSplitMessage {
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
    private:
      ReadCellCacheReplyMessage() {}
    } PACKED;

    struct WriteCellCacheReplyMessage {
      IndexT start;
      IndexT end;
      ElementT values[];
    private:
      WriteCellCacheReplyMessage() {}
    } PACKED;

    struct GetHostListReplyMessage {
      int numHosts;
      DataHostPidListItem hosts[];
    private:
      GetHostListReplyMessage() {}
    } PACKED;

    struct UpdateHandlerChainReplyMessage {
      HostPid dataHost;
      int numHops;
      EncodedPtr encodedPtr; // regiondata or regionhandler
      bool isDataSplit;
    } PACKED;

    struct CopyToMatrixStorageReplyMessage {
      size_t count;
      ElementT storage[];
    private:
      CopyToMatrixStorageReplyMessage() {}
    } PACKED;

    struct CopyFromMatrixStorageReplyMessage {
    } PACKED;

    struct AllocDataReplyMessage {
      int result;
    } PACKED;

    struct RandomizeDataReplyMessage {
    } PACKED;

    struct CopyRegionDataSplitReplyMessage {
      int dimensions;
      IndexT numParts;
      IndexT splitSize[];
      RemoteRegionHandler* handlers() const {
        return (RemoteRegionHandler*)((char*)this + sizeof(int) + sizeof(IndexT) + (dimensions * sizeof(IndexT)));
      }
      static int len(int d, int numParts) {
        return sizeof(int) + sizeof(IndexT) + sizeof(IndexT) * d +
          sizeof(RemoteRegionHandler) * numParts;
      }
    private:
      CopyRegionDataSplitReplyMessage() {}
    } PACKED;
  }
}

#endif
