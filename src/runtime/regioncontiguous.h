#ifndef PETABRICKSREGIONCONTUGUOUS_H
#define PETABRICKSREGIONCONTUGUOUS_H

#include "regioni.h"

namespace petabricks {

  class RegionContiguous;
  typedef jalib::JRef<RegionContiguous> RegionContiguousPtr;

  class RegionContiguous : public RegionI {
  private:
    ElementT* _data;

  public:
    RegionContiguous(int dimension, IndexT* size);
    ~RegionContiguous();

    ElementT* coordToPtr(IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);
    
    //void allocate();
  };
}

#endif
