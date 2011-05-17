#ifndef PETABRICKSREGIONDATA0D_H
#define PETABRICKSREGIONDATA0D_H

#include "common/jassert.h"
#include "regiondatai.h"

namespace petabricks {
  class RegionData0D : public RegionDataI {

  private:
    ElementT _value;

  public:
    RegionData0D() {
      _D = 0;
      _type = RegionDataTypes::REGIONDATA0D;
      _size = new IndexT[0];
    }

    RegionData0D(ElementT value) {
      _D = 0;
      _size = new IndexT[0];
      _value = value;
    }

    int allocData() {
      return 0;
    }

    ElementT readCell(const IndexT* /*coord*/) {
      return _value;
    }

    void writeCell(const IndexT* /*coord*/, ElementT value) {
      _value = value;
    }

    DataHostList hosts(IndexT* /*begin*/, IndexT* /*end*/) {
      DataHostListItem item = {HostPid::self(), 1};
      return DataHostList(1, item);
    }

    void print() {
      printf("%e\n", _value);
    }
  };
}

#endif
