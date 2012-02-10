#include "regiondatasplit.h"

#include <map>
#include <string.h>
#include "regiondataremote.h"

using namespace petabricks;

RegionDataSplit::RegionDataSplit(int dimensions, const IndexT* sizes, const IndexT* splitSize) {
  init(dimensions, sizes, splitSize);
}

void RegionDataSplit::init(int dimensions, const IndexT* sizes, const IndexT* splitSize) {
  _D = dimensions;
  _type = RegionDataTypes::REGIONDATASPLIT;

  memcpy(_size, sizes, sizeof(IndexT) * _D);
  memcpy(_splitSize, splitSize, sizeof(IndexT) * _D);

  // create parts
  _numParts = 1;

  for (int i = 0; i < _D; i++) {
    _partsSize[i] = _size[i] / _splitSize[i];

    if (_size[i] % _splitSize[i]) {
      _partsSize[i]++;
    }

    _numParts *= _partsSize[i];
  }

  _partsMultipliers[0] = 1;
  for (int i = 1; i < _D; i++) {
    _partsMultipliers[i] = _partsMultipliers[i - 1] * _partsSize[i - 1];
  }

  _parts.resize(_numParts, NULL);
}

void RegionDataSplit::createPart(int partIndex, RemoteHostPtr host) {
  JASSERT(!_parts[partIndex]);

  IndexT partsCoord[_D];
  int tmp = partIndex;
  for (int i = 0; i < _D; i++) {
    partsCoord[i] = tmp % _partsSize[i];
    tmp = tmp / _partsSize[i];
  }

  IndexT size[_D];
  IndexT partOffset[_D];
  for (int i = 0; i < _D; i++){
    partOffset[i] = _splitSize[i] * partsCoord[i];
    if (partOffset[i] + _splitSize[i] > _size[i]) {
      size[i] = _size[i] - partOffset[i];
    } else {
      size[i] = _splitSize[i];
    }
  }

  if (host == NULL) {
    _parts[partIndex] = new RegionHandler(new RegionDataRaw(_D, size));
  } else {
    _parts[partIndex] = new RegionHandler(new RegionDataRemote(_D, size, host));
  }
}

void RegionDataSplit::setPart(int partIndex, const RemoteRegionHandler& remoteRegionHandler) {
  JASSERT(!_parts[partIndex]);

  IndexT partsCoord[_D];
  int tmp = partIndex;
  for (int i = 0; i < _D; i++) {
    partsCoord[i] = tmp % _partsSize[i];
    tmp = tmp / _partsSize[i];
  }

  IndexT size[_D];
  IndexT partOffset[_D];
  for (int i = 0; i < _D; i++){
    partOffset[i] = _splitSize[i] * partsCoord[i];
    if (partOffset[i] + _splitSize[i] > _size[i]) {
      size[i] = _size[i] - partOffset[i];
    } else {
      size[i] = _splitSize[i];
    }
  }

  RemoteHostPtr host = RemoteHostDB::instance().host(remoteRegionHandler.hostPid);

  _parts[partIndex] = RegionHandlerDB::instance().getLocalRegionHandler(*host, remoteRegionHandler.remoteHandler, _D, size);
  _parts[partIndex]->updateHandlerChain();
}

int RegionDataSplit::allocData() {
  for (int i = 0; i < _numParts; i++) {
    if (!_parts[i]) {
      this->createPart(i, NULL);
    }

    _parts[i]->allocData();
  }
  return 0;
}

ElementT RegionDataSplit::readCell(const IndexT* coord) const {
  IndexT coordPart[_D];
  return this->coordToPart(coord, coordPart)->readCell(coordPart);
}

void RegionDataSplit::writeCell(const IndexT* coord, ElementT value) {
  IndexT coordPart[_D];
  this->coordToPart(coord, coordPart)->writeCell(coordPart, value);
}

void RegionDataSplit::copyHelper(bool isCopyTo, RegionMatrixMetadata* origMetadata, MatrixStoragePtr scratchStorage) const {
  // find begin & end
  IndexT begin[_D];
  IndexT end[_D];
  IndexT sliceIndex = 0;
  IndexT splitIndex = 0;
  for (int d = 0; d < _D; ++d) {
    if (sliceIndex < origMetadata->numSliceDimensions && d == origMetadata->sliceDimensions()[sliceIndex]) {
      begin[d] = origMetadata->slicePositions()[sliceIndex];
      end[d] = origMetadata->slicePositions()[sliceIndex];
      ++sliceIndex;
    } else {
      begin[d] = origMetadata->splitOffset[splitIndex];
      end[d] = origMetadata->splitOffset[splitIndex] + origMetadata->size()[splitIndex];
      ++splitIndex;
    }
  }

  size_t len = RegionMatrixMetadata::len(origMetadata->dimensions, origMetadata->numSliceDimensions);
  if (!isCopyTo) {
    int count = 1;
    for (int i = 0; i < origMetadata->dimensions; ++i) {
      count *= origMetadata->size()[i];
    }
    int partCount = 1;
    for (int i = 0; i < _D; ++i) {
      sliceIndex = 0;
      if (sliceIndex < origMetadata->numSliceDimensions && i == origMetadata->sliceDimensions()[sliceIndex]) {
        ++sliceIndex;
      } else {
        partCount *= _splitSize[i];
      }

    }
    if (partCount < count) {
      count = partCount;
    }
    len += (count * sizeof(ElementT));
  }
  char newOrigMetadataBuf[len];
  memcpy(newOrigMetadataBuf, origMetadata, len);
  RegionMatrixMetadata* newOrigMetadata = (RegionMatrixMetadata*)newOrigMetadataBuf;

  size_t scratchLen = RegionMatrixMetadata::len(origMetadata->dimensions, 0);
  char newScratchMetadataBuf[scratchLen];
  RegionMatrixMetadata* newScratchMetadata = (RegionMatrixMetadata*)newScratchMetadataBuf;
  newScratchMetadata->dimensions = origMetadata->dimensions;
  newScratchMetadata->numSliceDimensions = 0;

  IndexT newBegin[_D];
  IndexT coord[_D];
  for (int d = 0; d < _D; ++d) {
    coord[d] = begin[d] - (begin[d] % _splitSize[d]);
    newBegin[d] = coord[d];
  }

  IndexT partBegin[_D];
  do {
    sliceIndex = 0;
    splitIndex = 0;

    for (int i = 0; i < _D; ++i) {
      if (coord[i] < begin[i]) {
        partBegin[i] = begin[i];
      } else {
        partBegin[i] = coord[i];
      }
    }

    IndexT newOrigSplitOffset[_D];
    RegionHandlerPtr part = this->coordToPart(partBegin, newOrigSplitOffset);

    for (int i = 0; i < _D; ++i) {
      if (sliceIndex < origMetadata->numSliceDimensions && i == origMetadata->sliceDimensions()[sliceIndex]) {
        newOrigMetadata->slicePositions()[sliceIndex] = origMetadata->slicePositions()[sliceIndex] - newBegin[i];
        ++sliceIndex;

      } else {
        if (coord[i] + _splitSize[i] <= end[i]) {
          newOrigMetadata->size()[splitIndex] = coord[i] + _splitSize[i] - partBegin[i];
        } else {
          newOrigMetadata->size()[splitIndex] = end[i] - partBegin[i];
        }

        newScratchMetadata->splitOffset[splitIndex] = partBegin[i] - begin[i];
        newOrigMetadata->splitOffset[splitIndex] = newOrigSplitOffset[i];
        ++splitIndex;
      }
    }

    memcpy(newScratchMetadata->size(), newOrigMetadata->size(), sizeof(IndexT) * newOrigMetadata->dimensions);

    if (isCopyTo) {
      part->copyToScratchMatrixStorage((CopyToMatrixStorageMessage*) newOrigMetadata, len, scratchStorage, newScratchMetadata, origMetadata->size());
    } else {
      part->copyFromScratchMatrixStorage((CopyFromMatrixStorageMessage*) newOrigMetadata, len, scratchStorage, newScratchMetadata, origMetadata->size());
    }

  } while (incPartCoord(coord, newBegin, end) >= 0);
}

RegionDataIPtr RegionDataSplit::copyToScratchMatrixStorage(CopyToMatrixStorageMessage* origMsg, size_t /*len*/, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* /*scratchStorageSize*/) const {
  JASSERT(scratchMetadata == 0).Text("split data must be top-level");

  RegionMatrixMetadata* origMetadata = &(origMsg->srcMetadata);
  copyHelper(true, origMetadata, scratchStorage);
  return NULL;
}

void RegionDataSplit::copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* origMsg, size_t /*len*/, MatrixStoragePtr scratchStorage, RegionMatrixMetadata* scratchMetadata, const IndexT* /*scratchStorageSize*/) {
  JASSERT(scratchMetadata == 0).Text("split data must be top-level");

  RegionMatrixMetadata* origMetadata = &(origMsg->srcMetadata);
  copyHelper(false, origMetadata, scratchStorage);
}

void RegionDataSplit::processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  ReadCellMessage* msg = (ReadCellMessage*)base->content();
  this->coordToPart(msg->coord, msg->coord)->processReadCellMsg(base, baseLen, caller);
}

void RegionDataSplit::processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  WriteCellMessage* msg = (WriteCellMessage*)base->content();
  this->coordToPart(msg->coord, msg->coord)->processWriteCellMsg(base, baseLen, caller);
}

void RegionDataSplit::processCopyToMatrixStorageMsg(const BaseMessageHeader* base, size_t, IRegionReplyProxy* caller) {
  // Return a copy of this regiondatasplit
  size_t sz = CopyRegionDataSplitReplyMessage::len(_D, _numParts);
  char buf[sz];
  CopyRegionDataSplitReplyMessage* reply = (CopyRegionDataSplitReplyMessage*) buf;
  reply->dimensions = _D;
  reply->numParts = _numParts;
  memcpy(reply->splitSize, _splitSize, _D * sizeof(IndexT));
  RemoteRegionHandler* handler = reply->handlers();
  for (int i = 0; i < _numParts; i++) {
    // TODO (yod): optimize this by sending info of real host
    handler->hostPid = HostPid::self();
    handler->remoteHandler = reinterpret_cast<EncodedPtr>(_parts[i].asPtr());
    ++handler;
  }

  caller->sendReply(buf, sz, base, MessageTypes::COPYREGIONDATASPLIT);
}

void RegionDataSplit::processCopyFromMatrixStorageMsg(const BaseMessageHeader* /*base*/, size_t /*baseLen*/, IRegionReplyProxy* /*caller*/) {
  JASSERT(false).Text("copy RegionDataSplit to local before copying");
}

DataHostPidList RegionDataSplit::hosts(const IndexT* begin, const IndexT* end) const {
  std::map<HostPid, int> hosts;

  IndexT newBegin[_D];
  IndexT coord[_D];
  for (int i = 0; i < _D; i++) {
    newBegin[i] = begin[i] - (begin[i] % _splitSize[i]);
    coord[i] = newBegin[i];
  }

  int count = 0;

  const IndexT partBegin[] = {0,0,0};
  const IndexT* partEnd = _splitSize;

  IndexT junk[_D];
  do {
    DataHostPidList tmp = this->coordToPart(coord, junk)->hosts(partBegin, partEnd);
    hosts[tmp[0].hostPid] += 1;
    count++;

  } while (incPartCoord(coord, newBegin, end) >= 0);

  DataHostPidList list;
  std::map<HostPid, int>::iterator it;
  for (it = hosts.begin(); it != hosts.end(); it++) {
    DataHostPidListItem item;
    item.hostPid = (*it).first;
    item.weight = ((double)((*it).second))/count;
    list.push_back(item);
  }

  return list;
}

RegionHandlerPtr RegionDataSplit::coordToPart(const IndexT* coord, IndexT* coordPart) const {
  IndexT index = 0;
  for (int i = 0; i < _D; i++){
    index += (coord[i] / _splitSize[i]) * _partsMultipliers[i];
    coordPart[i] = coord[i] % _splitSize[i];
  }
  return _parts[index];
}

// begin (inclusive), end (exclusive)
// begin must be at the boundary
int RegionDataSplit::incPartCoord(IndexT* coord, const IndexT* begin, const IndexT* end) const {
  #ifdef DEBUG
  for (int i = 0; i < _D; ++i) {
    JASSERT((begin[i] % _splitSize[i]) == 0);
  }
  #endif

  coord[0] += _splitSize[0];
  for (int i = 0; i < _D - 1; ++i) {
    if (coord[i] >= end[i]){
      coord[i] = begin[i];
      coord[i+1] += _splitSize[i+1];
    } else {
      return i;
    }
  }
  if (coord[_D-1] < end[_D-1]){
    return _D-1;
  }
  return -1;
}

void RegionDataSplit::print() {
  printf("%d parts\n", _numParts);
}

