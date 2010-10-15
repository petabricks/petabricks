#import "regionsplit.h"
#import <string.h>

petabricks::RegionSplit::RegionSplit (RegionContiguousPtr regionContiguous, IndexT* offset, IndexT* size) {
  _regionContiguous = regionContiguous;
  _dimension = _regionContiguous->dimension();

  _offset = new IndexT[_dimension];
  memcpy(_offset, offset, (sizeof _offset)*_dimension);

  _size = new IndexT[_dimension];
  memcpy(_size, size, (sizeof _size)*_dimension);
}

petabricks::ElementT*
petabricks::RegionSplit::coordToPtr(const IndexT* coord){
  IndexT* coord_new = this->getContiguousOffset(coord);
  petabricks::ElementT* ret = _regionContiguous->coordToPtr(coord_new);
  delete(coord_new);
  return ret;
}

petabricks::RegionIPtr
petabricks::RegionSplit::splitRegion(IndexT* offset, IndexT* size) {
  IndexT* offset_new = this->getContiguousOffset(offset);
  petabricks::RegionIPtr ret = new RegionSplit(_regionContiguous, offset_new, size);
  delete(offset_new);
  return ret;
}

petabricks::RegionIPtr
petabricks::RegionSplit::sliceRegion(int d, IndexT pos){
  return NULL;
}

petabricks::IndexT*
petabricks::RegionSplit::getContiguousOffset(const IndexT* offset_orig){
  IndexT* offset_new = new IndexT[_dimension];
  for (int d; d < _dimension; d++) {
    offset_new[d] = offset_orig[d] + _offset[d];
  }
  return offset_new;
}
