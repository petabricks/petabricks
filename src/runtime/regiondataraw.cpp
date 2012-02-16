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
  if (_storage) {
    return 0;
  }
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

RegionDataIPtr RegionDataRaw::copyToScratchMatrixStorage(CopyToMatrixStorageMessage* origMsg, size_t, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize) const {
  RegionMatrixMetadata* origMetadata = &(origMsg->srcMetadata);
#ifdef DEBUG
  JASSERT(origMetadata->dimensions == scratchMetadata->dimensions);
#endif

  int d = origMetadata->dimensions;
  IndexT* size = origMetadata->size();

  IndexT coord[d];
  memset(coord, 0, sizeof coord);
  IndexT scratchMultipliers[d];
  sizeToMultipliers(scratchMetadata->dimensions, scratchStorageSize, scratchMultipliers);

  do {
    IndexT origIndex = toRegionDataIndex(d, coord, origMetadata->numSliceDimensions, origMetadata->splitOffset, origMetadata->sliceDimensions(), origMetadata->slicePositions(), _multipliers);
    IndexT scratchIndex = toRegionDataIndex(d, coord, scratchMetadata->numSliceDimensions, scratchMetadata->splitOffset, scratchMetadata->sliceDimensions(), scratchMetadata->slicePositions(), scratchMultipliers);
    scratchStorage->data()[scratchIndex] = _storage->data()[origIndex];
  } while(incCoord(d, size, coord) >= 0);

  return NULL;
}

void RegionDataRaw::copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* origMsg, size_t, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* scratchStorageSize) {
  RegionMatrixMetadata* origMetadata = &(origMsg->srcMetadata);

  int d = origMetadata->dimensions;
  IndexT* size = origMetadata->size();

  IndexT coord[d];
  memset(coord, 0, sizeof coord);
  IndexT scratchMultipliers[d];
  sizeToMultipliers(d, scratchStorageSize, scratchMultipliers);

  do {
    IndexT origIndex = toRegionDataIndex(d, coord, origMetadata->numSliceDimensions, origMetadata->splitOffset, origMetadata->sliceDimensions(), origMetadata->slicePositions(), _multipliers);
    IndexT scratchIndex = toRegionDataIndex(d, coord, scratchMetadata->numSliceDimensions, scratchMetadata->splitOffset, scratchMetadata->sliceDimensions(), scratchMetadata->slicePositions(), scratchMultipliers);
    _storage->data()[origIndex] = scratchStorage->data()[scratchIndex];
  } while(incCoord(d, size, coord) >= 0);
}

DataHostPidList RegionDataRaw::hosts(const IndexT* /*begin*/, const IndexT* /*end*/) const {
  DataHostPidListItem item = {HostPid::self(), 1};
  return DataHostPidList(1, item);
}

RemoteHostPtr RegionDataRaw::dataHost() {
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
  RegionMatrixMetadata* metadata = &(msg->srcMetadata);

  int d = metadata->dimensions;
  IndexT* size = metadata->size();

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
    IndexT index = toRegionDataIndex(d, coord, metadata->numSliceDimensions, metadata->splitOffset, metadata->sliceDimensions(), metadata->slicePositions(), _multipliers);
    reply->storage[n] = _storage->data()[index];
    n++;
  } while(incCoord(d, size, coord) >= 0);

  caller->sendReply(buf, sz, base, MessageTypes::TOSCRATCHSTORAGE);
}

void RegionDataRaw::processCopyFromMatrixStorageMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  CopyFromMatrixStorageMessage* msg = (CopyFromMatrixStorageMessage*)base->content();
  RegionMatrixMetadata* metadata = &(msg->srcMetadata);

  int d = metadata->dimensions;
  IndexT* size = metadata->size();
  ElementT* storage = msg->storage();

  size_t sz = sizeof(CopyFromMatrixStorageReplyMessage);
  char buf[sz];

  IndexT n = 0;
  IndexT coord[d];
  memset(coord, 0, sizeof coord);

  do {
    unsigned int index = toRegionDataIndex(d, coord, metadata->numSliceDimensions, metadata->splitOffset, metadata->sliceDimensions(), metadata->slicePositions(), _multipliers);
    #ifdef DEBUG
    JASSERT(index <= _storage->count())(index)(_storage->count());
    #endif
    _storage->data()[index] = storage[n];
    n++;
  } while(incCoord(d, size, coord) >= 0);
  caller->sendReply(buf, sz, base, MessageTypes::FROMSCRATCHSTORAGE);
}
