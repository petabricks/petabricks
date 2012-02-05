#include "regiondatasplit.h"

#include <map>
#include <string.h>
#include "regiondataremote.h"

using namespace petabricks;

RegionDataSplit::RegionDataSplit(int dimensions, IndexT* sizes, IndexT* splitSize) {
  init(dimensions, sizes, splitSize);
}

void RegionDataSplit::init(int dimensions, IndexT* sizes, IndexT* splitSize) {
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
    _parts[partIndex] = new RegionDataRaw(_D, size);
  } else {
    _parts[partIndex] = new RegionDataRemote(_D, size, host);
  }
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

MatrixStoragePtr RegionDataSplit::copyToScratchMatrixStorage(CopyToMatrixStorageMessage* /*metadata*/, size_t /*size*/) const {
  UNIMPLEMENTED();
  return NULL;
}

void RegionDataSplit::copyFromScratchMatrixStorage(CopyFromMatrixStorageMessage* /*metadata*/, size_t /*size*/) const {
  UNIMPLEMENTED();
}

void RegionDataSplit::processReadCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  ReadCellMessage* msg = (ReadCellMessage*)base->content();
  this->coordToPart(msg->coord, msg->coord)->processReadCellMsg(base, baseLen, caller);
}

void RegionDataSplit::processWriteCellMsg(const BaseMessageHeader* base, size_t baseLen, IRegionReplyProxy* caller) {
  WriteCellMessage* msg = (WriteCellMessage*)base->content();
  this->coordToPart(msg->coord, msg->coord)->processWriteCellMsg(base, baseLen, caller);
}

void RegionDataSplit::processCopyFromMatrixStorageMsg(const BaseMessageHeader* /*base*/, size_t /*baseLen*/, IRegionReplyProxy* /*caller*/) {
  UNIMPLEMENTED();
}

void RegionDataSplit::processCopyToMatrixStorageMsg(const BaseMessageHeader* /*base*/, size_t /*baseLen*/, IRegionReplyProxy* /*caller*/) {
  UNIMPLEMENTED();
}

DataHostPidList RegionDataSplit::hosts(IndexT* begin, IndexT* end) {
  std::map<HostPid, int> hosts;

  IndexT coord[_D];
  for (int i = 0; i < _D; i++) {
    begin[i] = begin[i] - (begin[i] % _splitSize[i]);
    coord[i] = begin[i];
  }

  int count = 0;
  bool hasNextPart;

  // (yod) TODO: compute real begin/end
  IndexT newBegin[] = {0,0,0};
  IndexT* newEnd = _splitSize;

  IndexT junk[_D];
  do {
    DataHostPidList tmp = this->coordToPart(coord, junk)->hosts(newBegin, newEnd);
    hosts[tmp[0].hostPid] += 1;
    count++;

    // move to the next part
    hasNextPart = false;
    coord[0] += _splitSize[0];
    for (int i = 0; i < _D - 1; i++) {
      if (coord[i] > end[i]){
	coord[i] = begin[i];
	coord[i+1] += _splitSize[i];
      } else {
	hasNextPart = true;
	break;
      }
    }
    if (coord[_D-1] <= end[_D-1]){
      hasNextPart = true;
    }
  } while (hasNextPart);

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

RegionDataIPtr RegionDataSplit::coordToPart(const IndexT* coord, IndexT* coordPart) const {
  IndexT index = 0;

  for (int i = 0; i < _D; i++){
    index += (coord[i] / _splitSize[i]) * _partsMultipliers[i];
    coordPart[i] = coord[i] % _splitSize[i];
  }

  return _parts[index];
}

void RegionDataSplit::print() {
  printf("%d parts\n", _numParts);
}

