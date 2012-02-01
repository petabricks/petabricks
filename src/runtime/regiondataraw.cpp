#include "regiondataraw.h"

#include "matrixio.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRaw::RegionDataRaw(const int dimensions, const IndexT* size) {
  init(dimensions, size, NULL);
}

RegionDataRaw::RegionDataRaw(const int dimensions, const IndexT* size, const ElementT* data) {
  init(dimensions, size, data);
}

RegionDataRaw::RegionDataRaw(const char* filename) {
  distributed::MatrixIO matrixio(filename, "r");
  MatrixReaderScratch o = matrixio.readToMatrixReaderScratch();
  init(o.dimensions, o.sizes, o.storage->data());
}

void RegionDataRaw::init(const int dimensions, const IndexT* size, const ElementT* data) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATARAW;

  memcpy(_size, size, sizeof(IndexT) * _D);

  if (data) {
    int numData = allocData();
    memcpy(_storage->data(), data, sizeof(ElementT) * numData);
  }

  _multipliers[0] = 1;
  for (int i = 1; i < _D; i++) {
    _multipliers[i] = _multipliers[i - 1] * _size[i - 1];
  }
}

int RegionDataRaw::allocData() {
  int numData = 1;
  for (int i = 0; i < _D; i++) {
    numData *= _size[i];
  }

  _storage = new MatrixStorage(numData);
  return numData;
}

ElementT* RegionDataRaw::coordToPtr(const IndexT* coord) const {

  return _storage->data() + coordOffset(coord);
}

IndexT RegionDataRaw::coordOffset(const IndexT* coord) const {
  IndexT offset = 0;
  for(int i = 0; i < _D; i++){
    offset += _multipliers[i] * coord[i];
  }
  return offset;
}

ElementT& RegionDataRaw::value0D(const IndexT* coord) const {
  return *this->coordToPtr(coord);
}

ElementT RegionDataRaw::readCell(const IndexT* coord) const {
  return *this->coordToPtr(coord);
}

void RegionDataRaw::writeCell(const IndexT* coord, ElementT value) {
  ElementT* cell = this->coordToPtr(coord);
  *cell = value;
}

DataHostPidList RegionDataRaw::hosts(IndexT* /*begin*/, IndexT* /*end*/) {
  DataHostPidListItem item = {HostPid::self(), 1};
  return DataHostPidList(1, item);
}

RemoteHostPtr RegionDataRaw::host() {
  // local
  return NULL;
}

void RegionDataRaw::processReadCellCacheMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  ReadCellCacheMessage* msg = (ReadCellCacheMessage*)base->content();

  IndexT coordOffset = this->coordOffset(msg->coord);
  IndexT startOffset = coordOffset - (coordOffset % msg->cacheLineSize);

  size_t numValues = _storage->count() - startOffset;
  if (numValues > msg->cacheLineSize) {
    numValues = msg->cacheLineSize;
  }

  size_t values_sz = sizeof(ElementT) * numValues;
  size_t sz = sizeof(ReadCellCacheReplyMessage) + values_sz;

  char buf[sz];
  ReadCellCacheReplyMessage* reply = (ReadCellCacheReplyMessage*)buf;

  reply->start = 0;
  reply->end = numValues - 1;
  memcpy(reply->values, _storage->data() + startOffset, values_sz);

  caller->sendReply(buf, sz, base);
}

void RegionDataRaw::processWriteCellCacheMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  WriteCellCacheMessage* msg = (WriteCellCacheMessage*)base->content();

  IndexT coordOffset = this->coordOffset(msg->coord);
  IndexT startOffset = coordOffset - (coordOffset % msg->cacheLineSize);

  size_t values_sz = sizeof(ElementT) * msg->cacheLineSize;
  size_t sz = sizeof(WriteCellCacheReplyMessage) + values_sz;

  char buf[sz];
  WriteCellCacheReplyMessage* reply = (WriteCellCacheReplyMessage*)buf;

  reply->start = 0;
  reply->end = msg->cacheLineSize - 1;
  memcpy(reply->values, _storage->data() + startOffset, values_sz);

  caller->sendReply(buf, sz, base);
}

//
// Copy MatrixStorage and send it to the requested node. Only send what the
// requester needs.
void RegionDataRaw::processCopyToMatrixStorageMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  CopyToMatrixStorageMessage* msg = (CopyToMatrixStorageMessage*)base->content();

  int d = msg->dimensions;
  IndexT* size = msg->size();

  size_t storage_count = 1;
  for (int i = 0; i < d; i++) {
    storage_count *= size[i];
  }

  size_t sz = sizeof(CopyToMatrixStorageReplyMessage) + (sizeof(ElementT) * storage_count);

  char buf[sz];
  CopyToMatrixStorageReplyMessage* reply = (CopyToMatrixStorageReplyMessage*)buf;

  reply->count = storage_count;

  IndexT n = 0;
  IndexT coord[d];
  memset(coord, 0, sizeof coord);
  do {
    IndexT index = coordToIndex(d, msg->startOffset, msg->multipliers, coord);
    reply->storage[n] = _storage->data()[index];
    n++;
  } while(incCoord(d, size, coord) >= 0);

  caller->sendReply(buf, sz, base);
}

void RegionDataRaw::processCopyFromMatrixStorageMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  CopyFromMatrixStorageMessage* msg = (CopyFromMatrixStorageMessage*)base->content();

  int d = msg->dimensions;
  IndexT* size = msg->size();
  ElementT* storage = msg->storage();

  size_t sz = sizeof(CopyFromMatrixStorageReplyMessage);
  char buf[sz];

  IndexT n = 0;
  IndexT coord[d];
  memset(coord, 0, sizeof coord);

  do {
    unsigned int index = coordToIndex(d, msg->startOffset, msg->multipliers, coord);
    #ifdef DEBUG
    JASSERT(index <= _storage->count())(index)(_storage->count())(d)(msg->startOffset)(msg->multipliers[0])(coord[0]);
    #endif
    _storage->data()[index] = storage[n];
    n++;
  } while(incCoord(d, size, coord) >= 0);
  caller->sendReply(buf, sz, base);
}

int RegionDataRaw::incCoord(int dimensions, IndexT* size, IndexT* coord) const{
  if (dimensions == 0)
    return -1;
  int i;
  coord[0]++;
  for(i = 0; i < dimensions - 1; ++i){
    if(coord[i] >= size[i]){
      coord[i]=0;
      coord[i+1]++;
    }else{
      return i;
    }
  }
  if(coord[dimensions - 1] >= size[dimensions - 1]){
    return -1;
  }else{
    return dimensions - 1;
  }
}

IndexT RegionDataRaw::coordToIndex(int dimensions, IndexT startOffset, IndexT* multipliers, IndexT* coord) const {
  IndexT rv = startOffset;
  for(int i = 0; i < dimensions; ++i){
    rv +=  multipliers[i] * coord[i];
  }
  return rv;
}
