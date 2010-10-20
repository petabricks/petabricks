#include "regioncontiguous.h"
#include "regiontransform.h"

#include <string.h>

petabricks::RegionContiguous::RegionContiguous(int dimension, IndexT* size, ElementT* data) {
  _dimension = dimension;
  _size = new IndexT[_dimension];
  memcpy(_size, size, (sizeof _size)*_dimension);
 
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
  return new RegionTransform(this, _dimension, size, offset, 0, 0, 0);
}

petabricks::RegionIPtr
petabricks::RegionContiguous::sliceRegion(int d, IndexT pos){
  int splitDim[] = {d};
  IndexT splitPos[] = {pos};

  int dimension = _dimension - 1;

  IndexT* size = new IndexT[dimension];
  memcpy(size, _size, (sizeof size) * d);
  memcpy(size + d, _size + d + 1, (sizeof size) * (dimension - d));

  IndexT* offset = new IndexT[dimension];
  memset(offset, 0, (sizeof offset) * dimension);

  RegionIPtr ret = new RegionTransform(this, dimension, _size, offset, 1, splitDim, splitPos); 
  delete(size);
  delete(offset);
  return ret;
}

petabricks::RegionIPtr
petabricks::RegionContiguous::regionContiguous() {
  return this;
}
