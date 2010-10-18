#include "regioncontiguous.h"
#include "regionslice.h"
#include "regionsplit.h"

petabricks::RegionContiguous::RegionContiguous(int dimension, IndexT* size, ElementT* data) {
  _dimension = dimension;
  _size = size;
  _data = data;

  _multipliers = new IndexT[_dimension];
  _multipliers[0] = 1;
  for (int i = 1; i < _dimension; i++) {
    _multipliers[i] = _multipliers[i-1] * _size[i-1];
  }
}

petabricks::ElementT*
petabricks::RegionContiguous::coordToPtr(const IndexT* coord){
  IndexT offset = 0;
  for(int i = 0; i < _dimension; i++){
    offset += _multipliers[i] * coord[i];
  }
  return _data + offset;
}

petabricks::RegionIPtr
petabricks::RegionContiguous::splitRegion(IndexT* offset, IndexT* size) {
  return new RegionSplit(this, offset, size);
}

petabricks::RegionIPtr
petabricks::RegionContiguous::sliceRegion(int d, IndexT pos){
  return new RegionSlice(this, d, pos);
}
