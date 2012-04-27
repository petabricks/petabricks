#ifndef PETABRICKSREGIONHANDLER_H
#define PETABRICKSREGIONHANDLER_H

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "regiondatai.h"
#include "remotehost.h"
#include "subregioncachemanager.h"

#include <map>

#define NUM_CACHE_ITEMS 3

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class RegionHandler;
  typedef jalib::JRef<RegionHandler> RegionHandlerPtr;

  class RegionHandlerCacheItem;
  typedef jalib::JRef<RegionHandlerCacheItem> RegionHandlerCacheItemPtr;

  class RegionHandlerCacheItem : public jalib::JRefCounted {
  private:
    // ban
    RegionHandlerCacheItem();
    RegionHandlerCacheItem(const RegionHandlerCacheItem& that);

  public:
    RegionHandlerCacheItem(const char* buf, size_t len, long version, RegionHandlerPtr handler) {
      _buf = (char*)malloc(len);
      memcpy(_buf, buf, len);
      _len = len;
      _version = version;
      _handler = handler;
    }
    ~RegionHandlerCacheItem() {
      free(_buf);
    }
    bool isEqual(const char* buf, size_t len) const {
      if (_len != len) return false;
      return (memcmp(_buf, buf, len) == 0);
    }
    bool isValid(const char* buf, size_t len) const {
      if (isEqual(buf, len)) {
        return SubRegionCacheManager::isValid(_version);
      }
      return false;
    }
    RegionHandlerPtr handler() const {
      return _handler;
    }
    static jalib::Hash hash(const char* buf, size_t len) {
      jalib::HashGenerator hg;
      hg.update(buf, len);
      return hg.final();
    }

  private:
    char* _buf;
    size_t _len;
    long _version;
    RegionHandlerPtr _handler;
  };

  class RegionHandler : public jalib::JRefCounted {

  private:
    RegionHandler(const petabricks::RegionHandler&);

  public:
    RegionHandler(const int dimensions);
    RegionHandler(const int dimensions, const IndexT* size, const bool alloc);
    RegionHandler(const RegionDataIPtr regionData, int dataMigrationType);
    // ~RegionHandler() { JTRACE("destruct handler")(this); };

    void init();

    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);
    void invalidateCache();
    void randomize();

    int allocData();
    int allocData(const IndexT* size, int distributedCutoff, int distributionType, int distributionSize, int migrationType);

    bool isSizeLargerThanDistributedCutoff(const IndexT* size, int distributedCutoff) const;
    int allocDataLocal(const IndexT* size);
    int allocDataOneRemoteNode(const IndexT* size);
    int allocDataRoundRobin(const IndexT* size);
    int allocDataNBySlice(const IndexT* size, int distributionSize, int sliceDimension);
    int allocDataNByBlock(const IndexT* size, int distributionSize);

    RegionDataIPtr regionData() const;
    void updateRegionData(RegionDataIPtr regionData);

    void hosts(const IndexT* begin, const IndexT* end, DataHostPidList& list);
    RemoteHostPtr dataHost();
    bool isDataSplit() const;

    int dimensions() const;
    const IndexT* size() const;
    RegionDataType type() const;

    int migrationType() const { return _migrationType; }
    bool shouldCopyEntireData() const {
      return _migrationType == RegionDataMigrationTypes::COPY_ENTIRE_DATA;
    }
    bool shouldIgnoreDuringScheduling() const {
      return (_migrationType == RegionDataMigrationTypes::IGNORE)
        || (_migrationType == RegionDataMigrationTypes::COPY_ENTIRE_DATA);
    }

    // Migration
    void updateHandlerChain();
    RemoteRegionHandler remoteRegionHandler() const;
    bool isHandlerChainUpdated(); // for testing

    // Copy MatrixStorage
    void copyToScratchMatrixStorage(CopyToMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize, RegionDataI** newScratchRegionData);
    RegionHandlerPtr copyToScratchMatrixStorageCache(CopyToMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize, RegionHandlerPtr scratchHandler, RegionDataI** newScratchRegionData);
    void copyRegionDataToLocal();
    void copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize);

    // RegionDataSplit
    void splitData(int dimensions, const IndexT* sizes, const IndexT* splitSize);
    void createDataPart(int partIndex, RemoteHostPtr host);
    void copyRegionDataSplit();

    // Process Remote Messages
    void processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processReadCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processWriteCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processCopyToMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processCopyFromMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processRandomizeDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processCopyRegionDataSplitMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);

  private:
    RegionDataIPtr _regionData;
    jalib::JMutex _regionDataMux;
    jalib::JMutex _copyRegionDataToLocalMux;
    int _D;

    /* int cacheLastestIndex; */
    /* std::vector<double> _cacheHashes; */
    /* std::vector<RegionHandlerCacheMetadataPtr> _cacheMetadata; */
    /* std::vector<RegionHandlerPtr> _cacheHandlers; */
    typedef std::map<jalib::Hash, RegionHandlerCacheItemPtr> RegionHandlerCacheMap;
    RegionHandlerCacheMap _cache;
    jalib::JMutex _cacheMux;

    int _migrationType;

  };

  typedef std::map<EncodedPtr, RegionHandlerPtr> LocalRegionHandlerMap;

  class RegionHandlerDB {
  public:
    RegionHandlerDB() {}

    static RegionHandlerDB& instance();

    RegionHandlerPtr getLocalRegionHandler(const HostPid& hostPid, const EncodedPtr remoteHandler, const int dimensions, const IndexT* size, bool isDataSplit, int dataMigrationType);

  private:
    jalib::JMutex _mapMux;
    std::map<HostPid, LocalRegionHandlerMap> _map;
    std::map<HostPid, jalib::JMutex*> _localMapMux;
  };
}

#endif
