#include "iregion.h"
#include "splitregion.h"
   
IRegion<D, ElementT>
petabricks::IRegion::region(IndexT start[D], IndexT end[D]) {  
  return SplitRegion(this, start, end);
}
