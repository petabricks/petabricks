#include "regiondataremote.h"

#include "regionmatrixproxy.h"
#include "workerthread.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRemote::RegionDataRemote(const int dimensions, const IndexT* size, const RegionDataRemoteObjectPtr remoteObject) {
  init(dimensions, size, remoteObject);
}

RegionDataRemote::RegionDataRemote(const int dimensions, const IndexT* size, RemoteHostPtr host) {
  init(dimensions, size, new RegionDataRemoteObject());


  // InitialMsg
  size_t size_sz = _D * sizeof(IndexT);
  size_t msg_len = sizeof(CreateRegionDataInitialMessage) + size_sz;

  char buf[msg_len];
  CreateRegionDataInitialMessage* msg = (CreateRegionDataInitialMessage*)buf;
  msg->type = MessageTypes::CREATEREGIONDATA;
  msg->dimensions = _D;
  memcpy(msg->size, size, size_sz);

  host->createRemoteObject(_remoteObject.asPtr(), &RegionMatrixProxy::genRemote, buf, msg_len);
}

RegionDataRemote::RegionDataRemote(const int dimensions, const IndexT* size, const IndexT* partOffset, RemoteHostPtr host) {
  init(dimensions, size, new RegionDataRemoteObject());

  // InitialMsg
  CreateRegionDataPartInitialMessage msg;
  msg.type = MessageTypes::CREATEREGIONDATAPART;
  msg.dimensions = _D;
  memcpy(msg.size, size, sizeof(msg.size));
  if (partOffset) {
    memcpy(msg.partOffset, partOffset, sizeof(msg.partOffset));
  }

  int len = sizeof(CreateRegionDataPartInitialMessage);
  host->createRemoteObject(_remoteObject.asPtr(), &RegionMatrixProxy::genRemote, &msg, len);
}

RegionDataRemote::RegionDataRemote(const int dimensions, const IndexT* size, RemoteHost& host, const MessageType initialMessageType, const EncodedPtr encodePtr) {
  init(dimensions, size, new RegionDataRemoteObject());

  // InitialMsg
  EncodedPtrInitialMessage msg;
  msg.type = initialMessageType;
  msg.encodedPtr = encodePtr;
  int len = sizeof(EncodedPtrInitialMessage);
  host.createRemoteObject(_remoteObject.asPtr(), &RegionMatrixProxy::genRemote, &msg, len);
}

void RegionDataRemote::init(const int dimensions, const IndexT* size, const RegionDataRemoteObjectPtr remoteObject) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATAREMOTE;
  _remoteObject = remoteObject;

  memcpy(_size, size, sizeof(IndexT) * _D);
}

int RegionDataRemote::allocData() {
  void* data;
  size_t len;
  this->fetchData(0, MessageTypes::ALLOCDATA, 0, &data, &len);
  AllocDataReplyMessage* reply = (AllocDataReplyMessage*)data;

  ElementT result = reply->result;
  free(reply);
  return result;
}

void RegionDataRemote::randomize() {
  void* data;
  size_t len;
  this->fetchData(0, MessageTypes::RANDOMIZEDATA, 0, &data, &len);

  free(data);
}

IRegionCachePtr RegionDataRemote::cacheGenerator() const {
  IndexT multipliers[_D];
  multipliers[0] = 1;
  for (int i = 1; i < _D; i++) {
    multipliers[i] = multipliers[i - 1] * _size[i - 1];
  }

  return new RegionDataRemoteCache(this, _D, multipliers);
}

IRegionCachePtr RegionDataRemote::cache() const {
#ifdef DISTRIBUTED_CACHE
  return WorkerThread::self()->cache()->get(this);
#else
  return NULL;
#endif
}

void RegionDataRemote::invalidateCache() {
#ifdef DISTRIBUTED_CACHE
  cache()->invalidate();
#endif
}

ElementT RegionDataRemote::readCell(const IndexT* coord) const {
#ifdef DISTRIBUTED_CACHE
  return cache()->readCell(coord);
#else
  return readNoCache(coord);
#endif
}

ElementT RegionDataRemote::readNoCache(const IndexT* coord) const {
  size_t coord_sz = _D * sizeof(IndexT);
  size_t msg_len = sizeof(ReadCellMessage) + coord_sz;

  char buf[msg_len];
  ReadCellMessage* msg = (ReadCellMessage*)buf;
  memcpy(msg->coord, coord, coord_sz);

  void* data;
  size_t len;
  this->fetchData(buf, MessageTypes::READCELL, msg_len, &data, &len);
  ReadCellReplyMessage* reply = (ReadCellReplyMessage*)data;
  ElementT value = reply->value;
  free(reply);
  return value;
}

void RegionDataRemote::readByCache(void* request, size_t request_len, void* reply, size_t &/*reply_len*/) const {
  void* data;
  size_t len;
  this->fetchData(request, MessageTypes::READCELLCACHE, request_len, &data, &len);
  ReadCellCacheReplyMessage* r = (ReadCellCacheReplyMessage*)data;

  RegionDataRemoteCacheLine* cacheLine = (RegionDataRemoteCacheLine*)reply;
  cacheLine->start = r->start;
  cacheLine->end = r->end;
  memcpy(cacheLine->base, r->values, sizeof(ElementT) * (r->end + 1));
  free(r);
}

void RegionDataRemote::writeCell(const IndexT* coord, ElementT value) {
#ifdef DISTRIBUTED_CACHE
  cache()->writeCell(coord, value);
#else
  writeNoCache(coord, value);
#endif
}

void RegionDataRemote::writeNoCache(const IndexT* coord, ElementT value) {
  size_t coord_sz = _D * sizeof(IndexT);
  size_t msg_len = sizeof(WriteCellMessage) + coord_sz;

  char buf[msg_len];
  WriteCellMessage* msg = (WriteCellMessage*)buf;
  msg->value = value;
  memcpy(msg->coord, coord, coord_sz);

  JTRACE("write")(_D)(sizeof(IndexT))(coord_sz)(msg_len);

  void* data;
  size_t len;
  this->fetchData(buf, MessageTypes::WRITECELL, msg_len, &data, &len);
  WriteCellReplyMessage* reply = (WriteCellReplyMessage*)data;

  free(reply);
}

void RegionDataRemote::writeByCache(const IndexT* coord, ElementT value) const {
  size_t coord_sz = _D * sizeof(IndexT);
  size_t msg_len = sizeof(WriteCellMessage) + coord_sz;

  char buf[msg_len];
  WriteCellMessage* msg = (WriteCellMessage*)buf;
  msg->value = value;
  memcpy(msg->coord, coord, coord_sz);

  void* data;
  size_t len;
  this->fetchData(msg, MessageTypes::WRITECELL, msg_len, &data, &len);
  WriteCellReplyMessage* reply = (WriteCellReplyMessage*)data;

  free(reply);
}

DataHostPidList RegionDataRemote::hosts(IndexT* begin, IndexT* end) {
  GetHostListMessage msg;
  memcpy(msg.begin, begin, _D * sizeof(IndexT));
  memcpy(msg.end, end, _D * sizeof(IndexT));

  void* data;
  size_t len;
  this->fetchData(&msg, MessageTypes::GETHOSTLIST, sizeof(GetHostListMessage), &data, &len);
  GetHostListReplyMessage* reply = (GetHostListReplyMessage*)data;

  DataHostPidList list;
  for (int i = 0; i < reply->numHosts; i++) {
    list.push_back(reply->hosts[i]);
  }

  free(reply);
  return list;
}

RemoteHostPtr RegionDataRemote::host() {
  return _remoteObject->host();
}

UpdateHandlerChainReplyMessage RegionDataRemote::updateHandlerChain(UpdateHandlerChainMessage& msg) {
  msg.numHops += 1;

  void* data;
  size_t len;
  this->fetchData(&msg, MessageTypes::UPDATEHANDLERCHAIN, sizeof(UpdateHandlerChainMessage), &data, &len);
  UpdateHandlerChainReplyMessage* _reply = (UpdateHandlerChainReplyMessage*)data;

  UpdateHandlerChainReplyMessage reply = *_reply;
  free(_reply);
  return reply;
}

UpdateHandlerChainReplyMessage RegionDataRemote::updateHandlerChain() {
  UpdateHandlerChainMessage msg;
  msg.requester = HostPid::self();
  msg.numHops = 0;
  return this->updateHandlerChain(msg);
}

void RegionDataRemote::fetchData(const void* msg, MessageType type, size_t len, void** responseData, size_t* responseLen) const {
  *responseData = 0;
  *responseLen = 0;

  size_t dataLen = sizeof(GeneralMessageHeader) + len;

  GeneralMessageHeader* header = (GeneralMessageHeader*)malloc(dataLen);
  header->isForwardMessage = false;
  header->type = type;
  header->contentOffset = sizeof(GeneralMessageHeader);
  header->responseData = reinterpret_cast<EncodedPtr>(responseData);
  header->responseLen = reinterpret_cast<EncodedPtr>(responseLen);

  memcpy(header->content(), msg, len);

  _remoteObject->send(header, dataLen);
  free(header);

  // wait for the data
  while (*responseData == 0 || *responseLen == 0) {
    jalib::memFence();
    sched_yield();
  }
}

void RegionDataRemote::onRecv(const void* data, size_t len) {
  const BaseMessageHeader* base = (const BaseMessageHeader*)data;
  if (base->isForwardMessage) {
    const ForwardMessageHeader* header = (const ForwardMessageHeader*)data;
    IRegionReplyProxy* proxy = reinterpret_cast<IRegionReplyProxy*>(header->callback);
    proxy->processReplyMsg(base, len);

  } else {
    const GeneralMessageHeader* header = (const GeneralMessageHeader*)data;

    const void** responseData = reinterpret_cast<const void**>(header->responseData);
    size_t* responseLen = reinterpret_cast<size_t*>(header->responseLen);

    size_t sz = len - base->contentOffset;
    void* msg = malloc(sz);
    memcpy(msg, base->content(), sz);

    *responseData = msg;
    *responseLen = sz;
  }
}

// Process remote messages
void RegionDataRemote::forwardMessage(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  size_t len = sizeof(ForwardMessageHeader) + baseLen;

  ForwardMessageHeader* data = (ForwardMessageHeader*)malloc(len);
  data->isForwardMessage = true;
  data->type = base->type;
  data->contentOffset = sizeof(ForwardMessageHeader) + base->contentOffset;
  data->callback = reinterpret_cast<EncodedPtr>(caller);

  memcpy(data->next(), base, baseLen);

  _remoteObject->send(data, len);
  free(data);
}

void RegionDataRemote::processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processAllocDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processRandomizeDataMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processUpdateHandlerChainMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller, RegionDataIPtr) {
  UpdateHandlerChainMessage* msg = (UpdateHandlerChainMessage*)base->content();
  msg->numHops++;
  this->forwardMessage(base, baseLen, caller);
}

void RegionDataRemote::processGetHostListMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  this->forwardMessage(base, baseLen, caller);
}

RemoteObjectPtr RegionDataRemote::genRemote() {
  return new RegionDataRemoteObject();
}

