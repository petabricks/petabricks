#import "regionsplit.h"

petabricks::RegionSplit::RegionSplit (RegionContiguousPtr regionContiguous, IndexT* offset, IndexT* size) {
  _regionContiguous = regionContiguous;
  _offset = offset;
  _size = size;
}

petabricks::ElementT*
petabricks::RegionSplit::coordToPtr(IndexT* coord){
  return NULL;
}

petabricks::RegionIPtr
petabricks::RegionSplit::splitRegion(IndexT* offset, IndexT* size) {
  return NULL;
}

petabricks::RegionIPtr
petabricks::RegionSplit::sliceRegion(int d, IndexT pos){
  return NULL;
}

