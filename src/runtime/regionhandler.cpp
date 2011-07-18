#include "regionhandler.h"

#include "regiondataraw.h"
#include "regiondataremote.h"
#include "regiondatasplit.h"
#include "regionmatrixproxy.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionHandler::RegionHandler(const int dimensions, const IndexT* size) {
  _regionData = new RegionDataRaw(dimensions, size);
}

RegionHandler::RegionHandler(const int dimensions, const IndexT* size, const IndexT* partOffset) {
  _regionData = new RegionDataRaw(dimensions, size, partOffset);
}

RegionHandler::RegionHandler(const RegionDataIPtr regionData) {
  _regionData = regionData;
}

RegionHandler::RegionHandler(const EncodedPtr remoteObjPtr) {
  RegionDataRemoteObject* remoteObj = reinterpret_cast<RegionDataRemoteObject*>(remoteObjPtr);
  _regionData = remoteObj->regionData();
}

ElementT RegionHandler::readCell(const IndexT* coord) {
  //  JTRACE("read")(this);
  ElementT x = _regionData->readCell(coord);
  //  JTRACE("done read")(this);
  return x;
}

void RegionHandler::writeCell(const IndexT* coord, ElementT value) {
  //  JTRACE("write")(this);
  _regionData->writeCell(coord, value);
  //  JTRACE("done write")(this);
}

int RegionHandler::allocData() {
  return _regionData->allocData();
}

RegionDataIPtr RegionHandler::getRegionData() {
  return _regionData;
}

void RegionHandler::updateRegionData(RegionDataIPtr regionData) {
  _regionData = regionData;
}

DataHostList RegionHandler::hosts(IndexT* begin, IndexT* end) {
  return _regionData->hosts(begin, end);
}

int RegionHandler::dimensions() {
  return _regionData->dimensions();
}

IndexT* RegionHandler::size() {
  return _regionData->size();
}

RegionDataType RegionHandler::type() const {
  return _regionData->type();
}

//
// Migration
//

EncodedPtr RegionHandler::moveToRemoteHost(RemoteHostPtr host) {
  //if (_remoteObjects.count(host->id()) == 0) {
  //  if(1) {
    // Create remoteobjects

    RegionMatrixProxyPtr proxy = new RegionMatrixProxy(this);
    RegionMatrixProxyRemoteObjectPtr local = proxy->genLocal();

    // InitialMsg
    RegionDataRemoteMessage::InitialMessageToRegionDataRemote msg;
    msg.dimensions = dimensions();
    memcpy(msg.size, size(), sizeof(msg.size));
    int len = sizeof(RegionDataRemoteMessage::InitialMessageToRegionDataRemote);

    host->createRemoteObject(local.asPtr(), &RegionDataRemote::genRemote, &msg, len);
    local->waitUntilCreated();

    //    _remoteObjects[host->id()] = local->remoteObjPtr();
    //  } else {
    //    JTRACE("remoteobjects exist");
    //  }

    return local->remoteObjPtr();

  //return _remoteObjects[host->id()];
}

void RegionHandler::removeRemoteObject(const HostPid& hostPid) {
  // _remoteObjects.erase(hostPid);
}

void RegionHandler::updateHandlerChain() {
  if (type() == RegionDataTypes::REGIONDATAREMOTE) {
    RegionDataRemoteMessage::UpdateHandlerChainReplyMessage reply =
      ((RegionDataRemote*)_regionData.asPtr())->updateHandlerChain();
    //JTRACE("done updatehandler")(reply.dataHost)(reply.numHops);

    if (reply.dataHost == HostPid::self()) {
      // Data is in the same process. Update handler to point directly to the data.
      RegionDataI* regionData = reinterpret_cast<RegionDataI*>(reply.encodedPtr);
      updateRegionData(regionData);
    } else if (reply.numHops > 1) {
      // Multiple network hops to data. Create a direct connection to data.

      RemoteHostPtr dest = RemoteHostDB::instance().host(reply.dataHost);
      if (!dest) {
        JASSERT(false).Text("unknown host");
      }

      RegionDataIPtr newRegionData = new RegionDataRemote(_regionData->dimensions(), _regionData->size(), 0, dest, reply.encodedPtr);
      updateRegionData(newRegionData);
    }
  }
}

// For testing.
bool RegionHandler::isHandlerChainUpdated() {
  if (type() == RegionDataTypes::REGIONDATAREMOTE) {
    RegionDataRemoteMessage::UpdateHandlerChainReplyMessage reply =
      ((RegionDataRemote*)_regionData.asPtr())->updateHandlerChain();

    JTRACE("isHandlerChainUpdated")(reply.dataHost)(reply.numHops);
    if (reply.dataHost == HostPid::self()) {
      return false;
    } else if (reply.numHops > 1) {
      return false;
    }
  }
  return true;
}

//
// RegionDataSplit
//

void RegionHandler::splitData(IndexT* splitSize) {
  JASSERT(type() == RegionDataTypes::REGIONDATARAW);
  RegionDataIPtr newRegionData =
    new RegionDataSplit((RegionDataRaw*)_regionData.asPtr(), splitSize);
  updateRegionData(newRegionData);
}

void RegionHandler::createDataPart(int partIndex, RemoteHostPtr host) {
  JASSERT(type() == RegionDataTypes::REGIONDATASPLIT);
  ((RegionDataSplit*)_regionData.asPtr())->createPart(partIndex, host);
}

// Process Remote Messages
void RegionHandler::processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  _regionData->processReadCellMsg(base, baseLen, caller);
}

void RegionHandler::processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  _regionData->processWriteCellMsg(base, baseLen, caller);
}

void RegionHandler::processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  _regionData->processGetHostListMsg(base, baseLen, caller);
}

void RegionHandler::processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  _regionData->processAllocDataMsg(base, baseLen, caller);
}

void RegionHandler::processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  _regionData->processUpdateHandlerChainMsg(base, baseLen, caller);
}
