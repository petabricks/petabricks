#import "regiontransform.h"
#import <string.h>

petabricks::RegionTransform::
RegionTransform(RegionIPtr parent, int dimension, IndexT* size,
		IndexT* splitOffset, int numSliceDimensions,
		int* sliceDimensions, IndexT* slicePositions) {
  _regionContiguous = parent;
  _dimension = dimension;
  _size = new IndexT[_dimension];
  memcpy(_size, size, (sizeof _size)*_dimension);

  _splitOffset = new IndexT[_dimension];
  memcpy(_splitOffset, splitOffset, (sizeof _splitOffset)*_dimension);

  _numSliceDimensions = numSliceDimensions;
  if (_numSliceDimensions > 0) {
    _sliceDimensions = new int[_numSliceDimensions];
    memcpy(_sliceDimensions, sliceDimensions,
	   (sizeof _sliceDimensions) * _numSliceDimensions);
    _slicePositions = new IndexT[_numSliceDimensions];
    memcpy(_slicePositions, slicePositions,
	   (sizeof _slicePositions)*_numSliceDimensions);
  }
}

petabricks::RegionIPtr
petabricks::RegionTransform::splitRegion(IndexT* offset, IndexT* size) {
  IndexT* offset_new = this->getContiguousOffset(offset);
  petabricks::RegionIPtr ret =
    new RegionTransform(_regionContiguous, _dimension, size, offset_new,
			_numSliceDimensions, _sliceDimensions, _slicePositions);
  delete(offset_new);
  return ret;
}

petabricks::RegionIPtr
petabricks::RegionTransform::sliceRegion(int d, IndexT pos){
  int dimension = _dimension - 1;
  IndexT* size = new IndexT[dimension];
  memcpy(size, _size, (sizeof size) * d);
  memcpy(size + d, _size + d + 1, (sizeof size) * (dimension - d));

  IndexT* offset = new IndexT[dimension];
  memcpy(offset, _splitOffset, (sizeof offset) * d);
  memcpy(offset + d, _splitOffset + d + 1, (sizeof offset) * (dimension - d));

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

  petabricks::RegionIPtr ret =
    new RegionTransform(_regionContiguous, dimension, size, offset,
			 numSliceDimensions, sliceDimensions, slicePositions);

  delete(size);
  delete(offset);
  delete(sliceDimensions);
  delete(slicePositions);

  return ret;
}

//
// Convert an offset to the one in _regionContiguous
//
petabricks::IndexT*
petabricks::RegionTransform::getContiguousOffset(const IndexT* offset_orig){
  IndexT slice_index = 0;
  IndexT split_index = 0;

  IndexT* offset_new = new IndexT[_regionContiguous->dimension()];
  for (int d; d < _regionContiguous->dimension(); d++) {
    if (slice_index < _numSliceDimensions &&
	d == _sliceDimensions[slice_index]) {
      // slice
      offset_new[d] = _slicePositions[slice_index];
      slice_index++;
    } else {
      // split
      offset_new[d] = offset_orig[split_index] + _splitOffset[split_index];
      split_index++;
    }
  }
  return offset_new;
}

petabricks::ElementT*
petabricks::RegionTransform::coordToPtr(const IndexT* coord){
  IndexT* coord_new = this->getContiguousOffset(coord);

  #ifdef DEBUG
  printf("<");
  for (int i = 0; i < _regionContiguous->dimension(); i++) {
    printf("%d", coord_new[i]);
  }
  printf(">");
  #endif

  petabricks::ElementT* ret = _regionContiguous->coordToPtr(coord_new);
  delete(coord_new);
  return ret;
}

petabricks::RegionIPtr
petabricks::RegionTransform::regionContiguous() {
  return _regionContiguous;
}
