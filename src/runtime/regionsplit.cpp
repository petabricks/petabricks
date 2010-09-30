#import "splitregion.h"

petabricks::SplitRegion::SplitRegion(IRegion<D, ElementT> parent, IndexT start[D], IndexT end[D]) {  
  _parent = parent;
  _start = start;
  _end = end;
}

ContiguousRegion<D, ElementT>
petabricks::SplitRegion:toContiguousRegion() {
  return NULL;
}
