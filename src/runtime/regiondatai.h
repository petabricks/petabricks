#ifndef PETABRICKSREGIONDATAI_H
#define PETABRICKSREGIONDATAI_H

#include "common/jrefcounted.h"

namespace petabricks {
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  class RegionDataI;
  typedef jalib::JRef<RegionDataI> RegionDataIPtr;

  class RegionDataI : public jalib::JRefCounted {
  protected:
    int _D;

    // _size is the size of this part, not the entire region
    IndexT* _size;

  public:
    ~RegionDataI();

    virtual int allocData() = 0;

    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    int dimensions();
    IndexT* size();

    // for tests
    int incCoord(IndexT* coord);
    virtual void print();
  };
}

#endif
