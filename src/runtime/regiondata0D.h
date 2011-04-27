#ifndef PETABRICKSREGIONDATA0D_H
#define PETABRICKSREGIONDATA0D_H

#include "regiondatai.h"

namespace petabricks {
  class RegionData0D : public RegionDataI {

  private:
    ElementT _value;

  public:
    RegionData0D() {
      _D = 0;
    }

    RegionData0D(ElementT value) {
      _D = 0;
      _value = value;
    }

    int allocData() {}

    ElementT readCell(const IndexT* coord) {
      return _value;
    }

    void writeCell(const IndexT* coord, ElementT value) {
      _value = value;
    }

    void print() {
      printf("%d\n", _value);
    }
  };
}

#endif
