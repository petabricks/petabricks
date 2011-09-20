#include "regiondataraw.h"

#include "matrixio.h"

using namespace petabricks;
using namespace petabricks::RegionDataRemoteMessage;

RegionDataRaw::RegionDataRaw(const int dimensions, const IndexT* size) {
  init(dimensions, size, NULL, NULL);
}

RegionDataRaw::RegionDataRaw(const int dimensions, const IndexT* size, const ElementT* data) {
  init(dimensions, size, data, NULL);
}

RegionDataRaw::RegionDataRaw(const int dimensions, const IndexT* size, const IndexT* partOffset) {
  init(dimensions, size, NULL, partOffset);
}

RegionDataRaw::RegionDataRaw(const char* filename) {
  MatrixIO matrixio(filename, "r");
  MatrixReaderScratch o = matrixio.readToMatrixReaderScratch();
  init(o.dimensions, o.sizes, o.storage->data(), NULL);
}

void RegionDataRaw::init(const int dimensions, const IndexT* size, const ElementT* data, const IndexT* partOffset) {
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

  if (partOffset) {
    _isPart = true;
    memcpy(_partOffset, partOffset, sizeof(IndexT) * _D);
  } else {
    _isPart = false;
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

  if (_isPart) {
    // this is a part of a region
    // convert original coord to this part coord before calculating offset

    for(int i = 0; i < _D; i++){
      offset += _multipliers[i] * (coord[i] - _partOffset[i]);
    }
  } else {
    for(int i = 0; i < _D; i++){
      offset += _multipliers[i] * coord[i];
    }
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
  if (_isPart) {
    UNIMPLEMENTED();
  }

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
  if (_isPart) {
    UNIMPLEMENTED();
  }

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

