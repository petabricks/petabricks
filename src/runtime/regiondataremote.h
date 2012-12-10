#ifndef PETABRICKSREGIONDATAREMOTE_H
#define PETABRICKSREGIONDATAREMOTE_H

#include <map>
#include <pthread.h>

#include "iregioncache.h"
#include "regiondatai.h"
#include "regiondataremotecache.h"
#include "regiondatasplit.h"
#include "remoteobject.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

namespace petabricks {
  using namespace petabricks::RegionDataRemoteMessage;

  class RegionDataRemote;
  typedef jalib::JRef<RegionDataRemote> RegionDataRemotePtr;

  class RegionDataRemote : public RegionDataI, public RemoteObject, IRegionCacheable {
  private:
    RemoteRegionHandler _remoteRegionHandler;
    bool _isRemoteRegionHandlerReady;
    bool _isDataSplit;

    jalib::JMutex _localRegionDataSplitMux;
    RegionDataSplitPtr _localRegionDataSplit;
    bool _isLocalRegionDataSplitReady;

  public:
    RegionDataRemote(const int dimensions, const IndexT* size, RemoteHostPtr host);
    RegionDataRemote(const int dimensions, const IndexT* size, const HostPid& hostPid, const EncodedPtr remoteHandler, bool isDataSplit);
    ~RegionDataRemote() {
      //JTRACE("Destruct RegionDataRemote")(this);
    }

    long refCount() const { return RemoteObject::refCount(); }
    void incRefCount() const { RemoteObject::incRefCount(); }
    void decRefCount() const { RemoteObject::decRefCount(); }

    void init(const int dimensions, const IndexT* size);

    int allocData();
    void allocDataNonBlock(jalib::AtomicT* responseCounter);
    void randomize();
    void randomizeNonBlock(jalib::AtomicT* responseCounter);

    const RemoteRegionHandler* remoteRegionHandler() const;

    ElementT readCell(const IndexT* coord) const;
    ElementT readNoCache(const IndexT* coord) const;

    void writeCell(const IndexT* coord, ElementT value);
    void writeNoCache(const IndexT* coord, ElementT value);

    // cache
    IRegionCachePtr cacheGenerator() const;
    IRegionCachePtr cache() const;
    void readByCache(void* request, size_t request_len, void* reply, size_t &reply_len) const;
    void writeByCache(const IndexT* coord, ElementT value) const;

    // scratch
    RegionDataIPtr copyToScratchMatrixStorage(CopyToMatrixStorageMessage* origMetadata, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize, RegionDataI** newScratchRegionData);
    void copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* origMetadata, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize);

    RegionDataIPtr hosts(const IndexT* begin, const IndexT* end, DataHostPidList& list);
    RemoteHostPtr dataHost();
    bool isDataSplit() const;

    // Update long chain of RegionHandlers
    UpdateHandlerChainReplyMessage updateHandlerChain();
    UpdateHandlerChainReplyMessage updateHandlerChain(UpdateHandlerChainMessage& msg);

    void onRecv(const void* data, size_t len, int type);
    void* allocRecv(size_t len, int);
    void freeRecv(void* buf, size_t , int arg);
    void fetchData(const void* msg, MessageType type, size_t len, void** responseData, size_t* responseLen, int* responseType) const;
    void fetchDataNonBlock(const void* msg, MessageType type, size_t len, jalib::AtomicT* responseCounter) const;

    // Process remote messages
    void forwardMessage(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);

    void processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processGetMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processRandomizeDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller, EncodedPtr regionHandlerPtr);
    void processCopyToMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processCopyFromMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);
    void processCopyRegionDataSplitMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller);

  private:
    void createRemoteObject() const;

    const RegionDataRemote* remoteObject() const {
      if (!this->isInitiator()) {
        createRemoteObject();
      }
      waitUntilCreated();
      return this;
    }

    RegionDataSplitPtr copyRegionDataSplit();
  };
}

#endif
