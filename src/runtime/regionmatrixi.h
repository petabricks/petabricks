#ifndef PETABRICKSREGIONMATRIXI_H
#define PETABRICKSREGIONMATRIXI_H

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "cellproxy.h"
#include "regiondatai.h"
#include "regionhandler.h"

namespace petabricks {
  class RegionMatrixI;
  typedef jalib::JRef<RegionMatrixI> RegionMatrixIPtr;

  class RegionMatrixI : public jalib::JRefCounted {
  protected:
    int _D;
    RegionHandlerPtr _regionHandler;

  public:
    virtual MATRIX_ELEMENT_T readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    RegionHandlerPtr getRegionHandler() const;

    int dimensions() const { return _D; }

    virtual CellProxy& cell(IndexT x, ...) const;
    virtual CellProxy& cell(IndexT* coord) const;

    INLINE CellProxy& cell() const {
      IndexT c1[0];
      return this->cell(c1);
    }
  };
}

#endif
