#include "regiondatasplit.h"

#include <map>
#include <string.h>
#include "regiondataproxy.h"

using namespace petabricks;

//
// Assume that the original regiondata is empty.
//
RegionDataSplit::RegionDataSplit(RegionDataRawPtr originalRegionData, IndexT* splitSize) {
  _D = originalRegionData->dimensions();
  _type = RegionDataTypes::REGIONDATASPLIT;
  _size = new IndexT[_D];
  memcpy(_size, originalRegionData->size(), sizeof(IndexT) * _D);

  _splitSize = new IndexT[_D];
  memcpy(_splitSize, splitSize, sizeof(IndexT) * _D);

  // create parts
  _partsSize = new IndexT[_D];
  _numParts = 1;

  for (int i = 0; i < _D; i++) {
    _partsSize[i] = _size[i] / _splitSize[i];

    if (_size[i] % _splitSize[i]) {
      _partsSize[i]++;
    }

    _numParts *= _partsSize[i];
  }

  _partsMultipliers = new IndexT[_D];
  _partsMultipliers[0] = 1;
  for (int i = 1; i < _D; i++) {
    _partsMultipliers[i] = _partsMultipliers[i - 1] * _partsSize[i - 1];
  }

  _parts = new RegionDataIPtr[_numParts];
  memset(_parts, 0, (sizeof _parts) * _numParts);
}

RegionDataSplit::~RegionDataSplit() {
  delete [] _parts;
  delete [] _partsSize;
  delete [] _splitSize;
}

void RegionDataSplit::createPart(int partIndex, RemoteHostPtr host) {
  JASSERT(!_parts[partIndex]);

  IndexT* partsCoord = new IndexT[_D];
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
    _parts[partIndex] = new RegionDataRaw(_D, size, partOffset);
  } else {
    _parts[partIndex] = new RegionDataProxy(_D, size, partOffset, host);
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

ElementT RegionDataSplit::readCell(const IndexT* coord) {
  return this->coordToPart(coord)->readCell(coord);
}

void RegionDataSplit::writeCell(const IndexT* coord, ElementT value) {
  this->coordToPart(coord)->writeCell(coord, value);
}

DataHostList RegionDataSplit::hosts() {
  std::map<HostPid, int> hosts;

  for (int i = 0; i < _numParts; i++) {
    DataHostList tmp = _parts[i]->hosts();
    hosts[tmp[0].hostPid] += 1;
  }

  DataHostList list;
  std::map<HostPid, int>::iterator it;
  for (it = hosts.begin(); it != hosts.end(); it++) {
    DataHostListItem item;
    item.hostPid = (*it).first;
    item.weight = ((double)((*it).second))/_numParts;
    list.push_back(item);
  }

  return list;
}

RegionDataIPtr RegionDataSplit::coordToPart(const IndexT* coord) {
  IndexT index = 0;

  for (int i = 0; i < _D; i++){
    index += (coord[i] / _splitSize[i]) * _partsMultipliers[i];
  }

  return _parts[index];
}

void RegionDataSplit::print() {
  printf("%d parts\n", _numParts);
}

