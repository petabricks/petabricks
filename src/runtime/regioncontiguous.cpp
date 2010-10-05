#include "regioncontiguous.h"
#include "regionsplit.h"

petabricks::RegionContiguous::RegionContiguous(int dimension, IndexT* size) {
  _dimension = dimension;
  _size = size;
}

petabricks::ElementT*
petabricks::RegionContiguous::coordToPtr(IndexT* coord){
  return NULL;
}

petabricks::RegionIPtr
petabricks::RegionContiguous::splitRegion(IndexT* offset, IndexT* size) {
  return new RegionSplit(this, offset, size);
}

petabricks::RegionIPtr
petabricks::RegionContiguous::sliceRegion(int d, IndexT pos){
  return NULL;
}
