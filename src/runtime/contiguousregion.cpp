#include "contiguousregion.h"
#include "splitregion.h"

void petabricks::ContiguousRegion::allocate(){
  
}

ElementT* petabricks::ContiguousRegion::coordToPtr(IndexT* coord){
  return NULL;
}

petabricks::IRegionPtr
petabricks::ContiguousRegion::region(IndexT* start, IndexT* end){
  return new SplitRegion(this, start, end);
}

petabricks::IRegionPtr
petabricks::ContiguousRegion::slice(int d, IndexT pos){
  return NULL;
}
