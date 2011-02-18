#include "regiondatasplit.h"

#include <string.h>

using namespace petabricks;

RegionDataSplit::RegionDataSplit(RegionDataRawPtr originalRegionData, IndexT* splitSize) {
  _D = originalRegionData->dimensions();
  _size = new IndexT[_D];
  memcpy(_size, originalRegionData->size(), sizeof(IndexT) * _D);

  _splitSize = splitSize;

  // create parts
  IndexT partsSize[_D];
  _numParts = 1;

  for (int i = 0; i < _D; i++) {
    partsSize[i] = (_size[i] / _splitSize[i]) + 1;
    _numParts *= partsSize[i];
  }

  _partsMultipliers = new IndexT[_D];
  _partsMultipliers[0] = 1;
  for (int i = 1; i < _D; i++) {
    _partsMultipliers[i] = _partsMultipliers[i - 1] * partsSize[i - 1];
  }

  _parts = new RegionDataIPtr[_numParts];

  IndexT* partsCoord = new IndexT[_D];
  memset(partsCoord, 0, (sizeof partsCoord) * _D);

  for (int j = 0; j < _numParts; j++) {
    // increment partCoord
    partsCoord[0]++;
    for (int i = 0; i < _D - 1; i++){
      if (partsCoord[i] >= partsSize[i]){
	partsCoord[i]=0;
	partsCoord[i+1]++;
      } else{
	break;
      }
    }

    // calculate size + offset
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
    
    _parts[j] = new RegionDataRaw(_D, size, partOffset);
  }

  // move data to parts + remove old data + update handle

  delete partsSize;
  delete partsCoord;
}

RegionDataSplit::~RegionDataSplit() {
  delete _parts;
  delete _splitSize;
}

ElementT RegionDataSplit::readCell(const IndexT* coord) {
  return this->coordToPart(coord)->readCell(coord);
}

void RegionDataSplit::writeCell(const IndexT* coord, ElementT value) {
  this->coordToPart(coord)->writeCell(coord, value);
}

RegionDataIPtr RegionDataSplit::coordToPart(const IndexT* coord) {
  IndexT index = 0;

  for (int i = 0; i < _D; i++){
    index += (coord[i] % _splitSize[i]) * _partsMultipliers[i];
  }

  return _parts[index];
}

void RegionDataSplit::print() {
  printf("%d parts\n", _numParts);
}

