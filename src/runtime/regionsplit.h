#ifndef PETABRICKSREGIONSPLIT_H
#define PETABRICKSREGIONSPLIT_H

#include "regioncontiguous.h"

namespace petabricks {
  class RegionSplit : public RegionI {

  private:
    RegionContiguousPtr _regionContiguous;
    IndexT* _offset;

  public:
    RegionSplit(RegionContiguousPtr regionContiguous, IndexT* offset, IndexT* size);

    ElementT* coordToPtr(const IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);

  private:
    IndexT* getContiguousOffset(const IndexT* offset_orig);

  };

}

#endif
