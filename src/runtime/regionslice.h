#ifndef PETABRICKSREGIONSLICE_H
#define PETABRICKSREGIONSLICE_H

#include "regioncontiguous.h"

namespace petabricks {
  class RegionSlice : public RegionI {

  private:
    RegionContiguousPtr _regionContiguous;
    int _sliceDimension;
    IndexT _pos;

  public:
    RegionSlice(RegionContiguousPtr regionContiguous, int sliceDimension, IndexT pos);

    ElementT* coordToPtr(const IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);

  private:
    IndexT* getContiguousOffset(const IndexT* offset_orig);

  };

}

#endif
