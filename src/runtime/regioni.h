#ifndef PETABRICKSREGIONI_H
#define PETABRICKSREGIONI_H

#include "common/jrefcounted.h"

namespace petabricks {
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  class RegionI;
  typedef jalib::JRef<RegionI> RegionIPtr;

  class RegionI : public jalib::JRefCounted {
  protected:
    int _dimension;
    IndexT* _size;

  public:
    virtual ~RegionI(){}
    RegionIPtr regionContiguous();
    virtual ElementT* coordToPtr(const IndexT* coord) = 0;
    virtual RegionIPtr splitRegion(IndexT* offset, IndexT* size) = 0;
    virtual RegionIPtr sliceRegion(int d, IndexT pos) = 0;


    int incCoord(IndexT* coord);
    int dimension() const;
    IndexT sizeOfDimension(int d) const;

    void print();
  };
}

#endif
