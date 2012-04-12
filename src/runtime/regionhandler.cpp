#include "regionhandler.h"

#include "petabricksruntime.h"
#include "regiondataraw.h"
#include "regiondataremote.h"
#include "regiondatasplit.h"
#include "regionmatrixproxy.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionHandler::RegionHandler(const int dimensions) {
  _D = dimensions;
  _shouldReplicateToAllNodes = false;
  init();
}

RegionHandler::RegionHandler(const int dimensions, const IndexT* size, const bool alloc = false) {
  _regionData = new RegionDataRaw(dimensions, size);
  _D = dimensions;
  if (alloc) {
    _regionData->allocData();
  }
  _shouldReplicateToAllNodes = false;
  init();
}

RegionHandler::RegionHandler(const RegionDataIPtr regionData, bool shouldReplicateToAllNodes = false) {
  _regionData = regionData;
  _D = _regionData->dimensions();
  _shouldReplicateToAllNodes = shouldReplicateToAllNodes;
  init();
}

void RegionHandler::init() {
}

ElementT RegionHandler::readCell(const IndexT* coord) {
  return regionData()->readCell(coord);
}

void RegionHandler::writeCell(const IndexT* coord, ElementT value) {
  regionData()->writeCell(coord, value);
}

void RegionHandler::invalidateCache() {
  return regionData()->invalidateCache();
}

void RegionHandler::randomize() {
  regionData()->randomize();
}

int RegionHandler::allocData() {
  return _regionData->allocData();
}

bool RegionHandler::isSizeLargerThanDistributedCutoff(const IndexT* size, int distributedCutoff) const {
  for (int i = 0; i < _D; ++i) {
    if (size[i] >= distributedCutoff) {
      return true;
    }
  }
  return false;
}

int RegionHandler::allocData(const IndexT* size, int distributedCutoff, int distributionType, int distributionSize) {
  if (_regionData) {
    JASSERT(type() == RegionDataTypes::REGIONDATASPLIT);
    _regionData->allocData();
    return 1;
  }

#ifdef REGIONMATRIX_TEST
  _regionData = new RegionDataRemote(_D, size, RemoteHostDB::instance().host(0));

#else
  if (distributionType == RegionDataDistributions::LOCAL) {
    allocDataLocal(size);

  } else if (distributionType == RegionDataDistributions::ONE_REMOTE) {
    if (isSizeLargerThanDistributedCutoff(size, distributedCutoff)) {
      allocDataOneRemoteNode(size);
    } else {
      allocDataLocal(size);
    }

  } else if (distributionType == RegionDataDistributions::N_BY_ROW) {
    if (_D >= 2 && size[1] >= distributedCutoff) {
      allocDataNBySlice(size, distributionSize, 1);
    } else {
      allocDataLocal(size);
    }

  } else if (distributionType == RegionDataDistributions::N_BY_COL) {
    if (size[0] >= distributedCutoff) {
      allocDataNBySlice(size, distributionSize, 0);
    } else {
      allocDataLocal(size);
    }

  } else if (distributionType == RegionDataDistributions::N_BY_BLOCK) {
    if (isSizeLargerThanDistributedCutoff(size, distributedCutoff)) {
      allocDataNByBlock(size, distributionSize);
    } else {
      allocDataLocal(size);
    }

  } else if (distributionType == RegionDataDistributions::ALL_NODES) {
    _shouldReplicateToAllNodes = true;
    allocDataLocal(size);

  } else {
    JASSERT(false).Text("Unknown distribution type.");

  }

#endif
  return 1;
}

//
// put data on current node
//
int RegionHandler::allocDataLocal(const IndexT* size) {
  if (_regionData) {
    JASSERT(type() == RegionDataTypes::REGIONDATASPLIT);
    _regionData->allocData();
    return 1;
  }

  // Create local data
  _regionData = new RegionDataRaw(_D, size);
  _regionData->allocData();

  return 1;
}

//
// pick random node and put data there
//
int RegionHandler::allocDataOneRemoteNode(const IndexT* size) {
  JTRACE("one remote");

  static int numHosts = RemoteHostDB::instance().size();
  if (numHosts > 0) {
    _regionData = new RegionDataRemote(_D, size, RemoteHostDB::instance().host(PetabricksRuntime::randInt(0, numHosts)));

  } else {
    allocDataLocal(size);
  }

  return 1;
}

//
// pick random N nodes and put data there
//
int RegionHandler::allocDataNBySlice(const IndexT* size, int distributionSize, int sliceDimension) {
  JTRACE("slice");

  static int numRemoteHosts = RemoteHostDB::instance().size();
  static int numHosts = numRemoteHosts + 1;

  if (distributionSize > numHosts) {
    distributionSize = numHosts;
  }

  // split data
  IndexT splitSize[_D];
  memcpy(splitSize, size, sizeof(IndexT) * _D);
  splitSize[sliceDimension] = splitSize[sliceDimension] / distributionSize;
  if (splitSize[sliceDimension] == 0) {
    splitSize[sliceDimension] = 1;
  }
  splitData(_D, size, splitSize);

  // create parts
  RegionDataSplit* regionDataSplit = (RegionDataSplit*)_regionData.asPtr();
  int numParts = regionDataSplit->numParts();
  int r = 0;
  for (int i = 0; i < numParts; ++i) {
    //int r = PetabricksRuntime::randInt(0, numHosts);
    if (r == numRemoteHosts) {
      // local
      regionDataSplit->createPart(i, NULL);
      r = 0;
    } else {
      regionDataSplit->createPart(i, RemoteHostDB::instance().host(r));
      ++r;
    }
  }

  regionDataSplit->allocData();
  return 1;
}

//
// split data to N^D pieces
//
int RegionHandler::allocDataNByBlock(const IndexT* size, int distributionSize) {
  // same as allocDataNBySlice, except splitSize
  JTRACE("block");

  static int numRemoteHosts = RemoteHostDB::instance().size();
  static int numHosts = numRemoteHosts + 1;

  if (distributionSize > numHosts) {
    distributionSize = numHosts;
  }

  // split data
  IndexT splitSize[_D];
  for (int i = 0; i < _D; ++i) {
    splitSize[i] = size[i] / distributionSize;
    if (splitSize[i] == 0) {
      splitSize[i] = 1;
    }
  }
  splitData(_D, size, splitSize);

  // create parts
  RegionDataSplit* regionDataSplit = (RegionDataSplit*)_regionData.asPtr();
  int numParts = regionDataSplit->numParts();
  int r = 0;
  for (int i = 0; i < numParts; ++i) {
    if (r == numRemoteHosts) {
      // local
      regionDataSplit->createPart(i, NULL);
      r = 0;
    } else {
      regionDataSplit->createPart(i, RemoteHostDB::instance().host(r));
      ++r;
    }
  }

  regionDataSplit->allocData();
  return 1;
}

//
// round-robin placement
//
int RegionHandler::allocDataRoundRobin(const IndexT* size) {
  static int numHosts = RemoteHostDB::instance().size();
  static int currentIndex = 0;

  int i = (currentIndex + 1) % (numHosts + 1);
  currentIndex = i;

  if (i < numHosts) {
    _regionData = new RegionDataRemote(_D, size, RemoteHostDB::instance().host(i));

  } else if (i == numHosts) {
    _regionData = new RegionDataRaw(_D, size);
    _regionData->allocData();

  } else {
    JASSERT(false)(i)(numHosts);
  }
  return 1;
}

RegionDataIPtr RegionHandler::regionData() const {
  JLOCKSCOPE(_regionDataMux);
  return _regionData;
}

void RegionHandler::updateRegionData(RegionDataIPtr regionData) {
  JLOCKSCOPE(_regionDataMux);
  _regionData = regionData;
}

void RegionHandler::hosts(const IndexT* begin, const IndexT* end, DataHostPidList& list) {
  RegionDataIPtr newRegionData = regionData()->hosts(begin, end, list);
  if (newRegionData) {
    updateRegionData(newRegionData);
  }
}

RemoteHostPtr RegionHandler::dataHost() {
  return regionData()->dataHost();
}

bool RegionHandler::isDataSplit() const {
  RegionDataType type = this->type();
  if (type == RegionDataTypes::REGIONDATASPLIT) {
    return true;

  } else if (type == RegionDataTypes::REGIONDATAREMOTE) {
    RegionDataIPtr regionData = this->regionData();
    return ((RegionDataRemote*)regionData.asPtr())->isDataSplit();

  } else {
    return false;
  }
}

int RegionHandler::dimensions() const {
  return _D;
}

const IndexT* RegionHandler::size() const {
  return this->regionData()->size();
}

RegionDataType RegionHandler::type() const {
  return this->regionData()->type();
}

//
// Migration
//

void RegionHandler::updateHandlerChain() {
  RegionDataIPtr regionData = this->regionData();

  if (type() == RegionDataTypes::REGIONDATAREMOTE) {
    RegionDataRemoteMessage::UpdateHandlerChainReplyMessage reply =
      ((RegionDataRemote*)regionData.asPtr())->updateHandlerChain();
    //JTRACE("done updatehandler")(reply.dataHost)(reply.numHops);

    if (reply.dataHost == HostPid::self()) {
      // Data is in the same process. Update handler to point directly to the data.
      RegionDataI* newRegionData = reinterpret_cast<RegionDataI*>(reply.encodedPtr);
      updateRegionData(newRegionData);

    } else if (reply.numHops > 1) {
      // Multiple network hops to data. Create a direct connection to data.
      RegionDataI* newRegionData = new RegionDataRemote(regionData->dimensions(), regionData->size(), reply.dataHost, reply.encodedPtr, reply.isDataSplit);
      updateRegionData(newRegionData);
    }
  }
}

// For testing.
bool RegionHandler::isHandlerChainUpdated() {
  RegionDataIPtr regionData = this->regionData();

  if (regionData->type() == RegionDataTypes::REGIONDATAREMOTE) {
    RegionDataRemoteMessage::UpdateHandlerChainReplyMessage reply =
      ((RegionDataRemote*)regionData.asPtr())->updateHandlerChain();

    JTRACE("isHandlerChainUpdated")(reply.dataHost)(reply.numHops);
    if (reply.dataHost == HostPid::self()) {
      return false;
    } else if (reply.numHops > 1) {
      return false;
    }
  }
  return true;
}

RemoteRegionHandler RegionHandler::remoteRegionHandler() const {
  RegionDataIPtr regionData = this->regionData();

  if (type() == RegionDataTypes::REGIONDATAREMOTE) {

    return *(((RegionDataRemote*)regionData.asPtr())->remoteRegionHandler());

  } else {
    RemoteRegionHandler remoteRegionHandler;
    remoteRegionHandler.hostPid = HostPid::self();
    remoteRegionHandler.remoteHandler = reinterpret_cast<EncodedPtr>(this);
    return remoteRegionHandler;
  }
}

void RegionHandler::copyToScratchMatrixStorage(CopyToMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize) {
  RegionDataIPtr regionData = this->regionData();

#ifdef DEBUG
  if (regionData->type() == RegionDataTypes::REGIONDATARAW && scratchMetadata == 0) {
    JASSERT(false).Text("This is inefficient. Use _regionData->storage() instead.");
  }
#endif

  RegionDataIPtr newRegionData =
    regionData->copyToScratchMatrixStorage(origMsg, len, scratchStorage, scratchMetadata, scratchStorageSize);
  if (newRegionData) {
    updateRegionData(newRegionData);
  }
}

RegionHandlerPtr RegionHandler::copyToScratchMatrixStorageCache(CopyToMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize, RegionHandlerPtr scratchHandler) {
  jalib::Hash hash = RegionHandlerCacheItem::hash((char*)origMsg, len);

  _cacheMux.lock();
  RegionHandlerCacheMap::iterator it = _cache.find(hash);
  if (it != _cache.end()) {
    // found in cache
    RegionHandlerCacheItemPtr t = it->second;
    _cacheMux.unlock();

    if (t->isEqual((char*)origMsg, len)) {
      return t->handler();
    }
  } else {
    _cacheMux.unlock();
  }

  copyToScratchMatrixStorage(origMsg, len, scratchStorage, scratchMetadata, scratchStorageSize);

  // store in cache
  _cacheMux.lock();
  _cache[hash] = new RegionHandlerCacheItem((char*)origMsg, len, scratchHandler);
  _cacheMux.unlock();
  return NULL;
}

void RegionHandler::copyRegionDataToLocal() {
  if (type() == RegionDataTypes::REGIONDATARAW) {
    return;
  }
  JDEBUGASSERT(!isDataSplit());

  JLOCKSCOPE(_copyRegionDataToLocalMux);

  RegionDataIPtr regionData = this->regionData();
  if (regionData->type() == RegionDataTypes::REGIONDATARAW) {
    return;
  }
  JDEBUGASSERT(regionData->type() == RegionDataTypes::REGIONDATAREMOTE)((int)regionData->type());

  RegionDataIPtr newRegionData = new RegionDataRaw(_D, regionData->size());
  newRegionData->allocData();

  size_t len = RegionMatrixMetadata::len(_D, 0);
  char buf[len];
  CopyToMatrixStorageMessage* msg = (CopyToMatrixStorageMessage*) buf;
  RegionMatrixMetadata& metadata = msg->srcMetadata;
  metadata.dimensions = _D;
  metadata.numSliceDimensions = 0;
  memset(metadata.splitOffset, 0, sizeof(IndexT) * _D);
  memcpy(metadata.size(), newRegionData->size(), sizeof(IndexT) * _D);

  regionData->copyToScratchMatrixStorage(msg, len, newRegionData->storage(), &metadata, newRegionData->size());

  updateRegionData(newRegionData);
  JTRACE("copy all regiondata");
}

void RegionHandler::copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* origMsg, size_t len, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize) {
  RegionDataIPtr regionData = this->regionData();

#ifdef DEBUG
  if (regionData->type() == RegionDataTypes::REGIONDATARAW && scratchMetadata == 0) {
    JASSERT(false).Text("This is inefficient. Use _regionData->storage() instead.");
  }
#endif

  regionData->copyFromScratchMatrixStorage(origMsg, len, scratchStorage, scratchMetadata, scratchStorageSize);
}


//
// RegionDataSplit
//

void RegionHandler::splitData(int dimensions, const IndexT* sizes, const IndexT* splitSize) {
  JASSERT((!_regionData) || type() == RegionDataTypes::REGIONDATARAW);
  RegionDataIPtr newRegionData =
    new RegionDataSplit(dimensions, sizes, splitSize);
  updateRegionData(newRegionData);
}

void RegionHandler::createDataPart(int partIndex, RemoteHostPtr host) {
  RegionDataIPtr regionData = this->regionData();
  JASSERT(regionData->type() == RegionDataTypes::REGIONDATASPLIT);
  ((RegionDataSplit*)regionData.asPtr())->createPart(partIndex, host);
}

// Process Remote Messages
void RegionHandler::processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processReadCellMsg(base, baseLen, caller);
}

void RegionHandler::processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processWriteCellMsg(base, baseLen, caller);
}

void RegionHandler::processReadCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processReadCellCacheMsg(base, baseLen, caller);
}

void RegionHandler::processWriteCellCacheMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processWriteCellCacheMsg(base, baseLen, caller);
}

void RegionHandler::processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processGetHostListMsg(base, baseLen, caller);
}

void RegionHandler::processCopyFromMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processCopyFromMatrixStorageMsg(base, baseLen, caller);
}

void RegionHandler::processCopyToMatrixStorageMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processCopyToMatrixStorageMsg(base, baseLen, caller);
}

void RegionHandler::processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processAllocDataMsg(base, baseLen, caller);
}

void RegionHandler::processRandomizeDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processRandomizeDataMsg(base, baseLen, caller);
}

void RegionHandler::processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processUpdateHandlerChainMsg(base, baseLen, caller, reinterpret_cast<EncodedPtr>(this));
}

void RegionHandler::processCopyRegionDataSplitMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->regionData()->processCopyRegionDataSplitMsg(base, baseLen, caller);
}

//
// RegionHandlerDB
//

RegionHandlerDB& RegionHandlerDB::instance() {
  static RegionHandlerDB db;
  return db;
}

RegionHandlerPtr RegionHandlerDB::getLocalRegionHandler(const HostPid& hostPid, const EncodedPtr remoteHandler, const int dimensions, const IndexT* size, bool isDataSplit, bool shouldReplicateToAllNodes) {
  if (hostPid == HostPid::self()) {
    return reinterpret_cast<RegionHandler*>(remoteHandler);
  }

  _mapMux.lock();
  if (_map.count(hostPid) == 0) {
    _map[hostPid] = LocalRegionHandlerMap();
    _localMapMux[hostPid] = new jalib::JMutex();
  }
  LocalRegionHandlerMap& localMap = _map[hostPid];
  jalib::JMutex* localMux = _localMapMux[hostPid];
  _mapMux.unlock();

  localMux->lock();
  if (localMap.count(remoteHandler) == 0) {
    // create a new one
    RegionDataIPtr regionData = new RegionDataRemote(dimensions, size, hostPid, remoteHandler, isDataSplit);
    localMap[remoteHandler] = new RegionHandler(regionData, shouldReplicateToAllNodes);
  }

  RegionHandlerPtr localHandler = localMap[remoteHandler];
  localMux->unlock();

  return localHandler;
}

