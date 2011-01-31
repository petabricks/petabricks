#include "regionmatrix.h"

#include <string.h>

using namespace petabricks;

RegionMatrix::RegionMatrix(RegionDataIPtr regionData) {
  _regionHandler = new RegionHandler(regionData);

  _D = _regionHandler->dimensions();

  _size = new IndexT[_D];
  memcpy(_size, regionData->size(), sizeof(IndexT) * _D);

  _splitOffset = new IndexT[_D];
  memset(_splitOffset, 0, sizeof(IndexT) * _D);

  _numSliceDimensions = 0;
  _sliceDimensions = 0;
  _slicePositions = 0;
}

//
// Called by split & slice
//
RegionMatrix::RegionMatrix(RegionHandlerPtr handler, int dimensions, IndexT* size,
			   IndexT* splitOffset, int numSliceDimensions,
			   int* sliceDimensions, IndexT* slicePositions) {
  _regionHandler = handler;
  _D = dimensions;
  _size = size;
  _splitOffset = splitOffset;
  _numSliceDimensions = numSliceDimensions;
  if (_numSliceDimensions > 0) {
    _sliceDimensions = sliceDimensions; 
    _slicePositions = slicePositions;
  }
}

RegionMatrix::~RegionMatrix() {
  delete _size;
  delete _splitOffset;
  delete _sliceDimensions;
  delete _slicePositions;
}

ElementT RegionMatrix::readCell(const IndexT* coord) {
  IndexT* rd_coord = this->getRegionDataCoord(coord);
  ElementT elmt = _regionData->readCell(rd_coord);
  delete rd_coord;
  return elmt;
}

void RegionMatrix::writeCell(const IndexT* coord, ElementT value) {
  IndexT* rd_coord = this->getRegionDataCoord(coord);
  _regionData->writeCell(rd_coord, value);
  delete rd_coord;
}

RegionMatrixPtr RegionMatrix::splitRegion(IndexT* offset, IndexT* size) {
  IndexT* offset_new = this->getRegionDataCoord(offset);

  IndexT* size_copy = new IndexT[_D];
  memcpy(size_copy, size, sizeof(IndexT) * _D);
  
  int* sliceDimensions = new int[_numSliceDimensions];
  memcpy(sliceDimensions, _sliceDimensions,
	 sizeof(int) * _numSliceDimensions);
  IndexT* slicePositions = new IndexT[_numSliceDimensions];
  memcpy(slicePositions, _slicePositions,
	 sizeof(IndexT) * _numSliceDimensions);

  return new RegionMatrix(_regionHandler, _D, size_copy, offset_new, 
			  _numSliceDimensions, sliceDimensions, slicePositions);
}

RegionMatrixPtr RegionMatrix::sliceRegion(int d, IndexT pos){
  int dimensions = _D - 1;
  IndexT* size = new IndexT[dimensions];
  memcpy(size, _size, sizeof(IndexT) * d);
  memcpy(size + d, _size + d + 1, sizeof(IndexT) * (dimensions - d));

  IndexT* offset = new IndexT[dimensions];
  memcpy(offset, _splitOffset, sizeof(IndexT) * d);
  memcpy(offset + d, _splitOffset + d + 1, sizeof(IndexT) * (dimensions - d));

  // maintain ordered array of _sliceDimensions + update d as necessary  
  int numSliceDimensions = _numSliceDimensions + 1;
  int* sliceDimensions = new int[numSliceDimensions];
  IndexT* slicePositions = new IndexT[numSliceDimensions];

  if (_numSliceDimensions == 0) {
    sliceDimensions[0] = d;
    slicePositions[0] = pos + _splitOffset[d];
  } else {
    bool isAddedNewD = false;
    for (int i = 0; i < numSliceDimensions; i++) {
      if (isAddedNewD) {
	sliceDimensions[i] = _sliceDimensions[i-1];
	slicePositions[i] = _slicePositions[i-1];
      } else if (d >= _sliceDimensions[i]) {
	sliceDimensions[i] = _sliceDimensions[i];
	slicePositions[i] = _slicePositions[i];
	d++;
      } else {
	sliceDimensions[i] = d;
	slicePositions[i] = pos + _splitOffset[d];
	isAddedNewD = true;
      }
    }
  }
  
  return new RegionMatrix(_regionHandler, dimensions, size, offset,
			  numSliceDimensions, sliceDimensions, slicePositions);
}

//
// Convert a coord to the one in _regionData
//
IndexT* RegionMatrix::getRegionDataCoord(const IndexT* coord_orig) {
  IndexT slice_index = 0;
  IndexT split_index = 0;

  IndexT* coord_new = new IndexT[_regionHandler->dimensions()];
  for (int d = 0; d < _regionHandler->dimensions(); d++) {
    if (slice_index < _numSliceDimensions &&
	d == _sliceDimensions[slice_index]) {
      // slice
      coord_new[d] = _slicePositions[slice_index];
      slice_index++;
    } else {
      // split
      coord_new[d] = coord_orig[split_index] + _splitOffset[split_index];
      split_index++;
    }
  }
  return coord_new;
}

///////////////////////////

int RegionMatrix::incCoord(IndexT* coord) {
  if (_D == 0) { 
    return -1;
  }

  coord[0]++;
  for (int i = 0; i < _D - 1; ++i){
    if (coord[i] >= _size[i]){
      coord[i]=0;
      coord[i+1]++;
    } else{
      return i;
    }
  }
  if (coord[_D - 1] >= _size[_D - 1]){
    return -1;
  }else{
    return _D - 1;
  }
}

void RegionMatrix::print() {
  printf("RegionMatrix: SIZE");
  for (int d = 0; d < _D; d++) {
    printf(" %d", _size[d]);
  }
  printf("\n");

  IndexT* coord = new IndexT[_D];
  memset(coord, 0, (sizeof coord) * _D);

  this->acquireRegionData();

  while (true) {
    printf("%4.8g ", this->readCell(coord));

    int z = this->incCoord(coord);

    if (z == -1) {
      break;
    }

    while (z > 0) {
      printf("\n");
      z--;
    }
  }

  this->releaseRegionData();

  printf("\n\n");
  delete(coord);
}
