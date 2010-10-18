#include "regionslice.h"

#include <string.h>
#include "regioncontiguous.h"

petabricks::RegionSlice::RegionSlice(RegionContiguousPtr regionContiguous, int sliceDimension, IndexT pos) {
  _regionContiguous = regionContiguous;
  _sliceDimension = sliceDimension;
  _pos = pos;

  _dimension = _regionContiguous->dimension() - 1;

  _size = new IndexT[_dimension];
  for (int i = 0; i < _sliceDimension; i++) {
    _size[i] = _regionContiguous->sizeOfDimension(i);
  }
  for (int i = _sliceDimension; i < _dimension; i++) {
    _size[i] = _regionContiguous->sizeOfDimension(i+1);
  }
  
}

petabricks::ElementT*
petabricks::RegionSlice::coordToPtr(const IndexT* coord){
  IndexT* coord_new = this->getContiguousOffset(coord);
  petabricks::ElementT* ret = _regionContiguous->coordToPtr(coord_new);
  delete(coord_new);
  return ret;
}

petabricks::RegionIPtr
petabricks::RegionSlice::splitRegion(IndexT* offset, IndexT* size) {
  return NULL;
}

petabricks::RegionIPtr
petabricks::RegionSlice::sliceRegion(int d, IndexT pos){
  return NULL;
}

petabricks::IndexT*
petabricks::RegionSlice::getContiguousOffset(const IndexT* offset_orig){
  IndexT* offset_new = new IndexT[_regionContiguous->dimension()];
  memcpy(offset_new, offset_orig, (sizeof offset_new) * _sliceDimension);
  offset_new[_sliceDimension] = _pos;
  memcpy(offset_new + _sliceDimension + 1, offset_orig + _sliceDimension, (sizeof offset_new) * (_regionContiguous->dimension() - _sliceDimension));
  return offset_new;
}
