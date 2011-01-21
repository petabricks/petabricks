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
    IndexT* _size;

  public:
    ~RegionDataI();

    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    int dimensions();
    IndexT* size();

    // for tests
    int incCoord(IndexT* coord);
    void print();
  };
}

#endif
