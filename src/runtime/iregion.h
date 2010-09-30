#ifndef PETABRICKSIREGION_H
#define PETABRICKSIREGION_H

#include "common/jrefcounted.h"

namespace petabricks {
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  class IRegion;
  typedef jalib::JRef<IRegion> IRegionPtr;

  class IRegion : public jalib::JRefCounted {
  public:
    virtual ElementT* coordToPtr(IndexT* coord) = 0;
    virtual IRegion region(IndexT* start, IndexT* end) = 0;
    virtual IRegion slice(int d, IndexT pos) = 0;
  };

}

#endif
