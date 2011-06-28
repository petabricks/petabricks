#ifndef PETABRICKSREGIONDATA0D_H
#define PETABRICKSREGIONDATA0D_H

#include "common/jassert.h"
#include "regiondatai.h"

namespace petabricks {
  class RegionData0D : public RegionDataI {

  private:
    ElementT* _value;
    bool _shouldDeleteValue;

  public:
    RegionData0D() {
      _D = 0;
      _type = RegionDataTypes::REGIONDATA0D;
      _size = new IndexT[0];
      _value = (ElementT*)malloc(sizeof(ElementT));
      _shouldDeleteValue = false;
    }

    RegionData0D(ElementT& value) {
      _D = 0;
      _type = RegionDataTypes::REGIONDATA0D;
      _size = new IndexT[0];
      _value = &value;
      _shouldDeleteValue = false;
    }

    ~RegionData0D() {
      if (_shouldDeleteValue) delete _value;
    }

    int allocData() {
      return 0;
    }

    ElementT readCell(const IndexT* /*coord*/) {
      return *_value;
    }

    void writeCell(const IndexT* /*coord*/, ElementT value) {
      *_value = value;
    }

    DataHostList hosts(IndexT* /*begin*/, IndexT* /*end*/) {
      DataHostListItem item = {HostPid::self(), 1};
      return DataHostList(1, item);
    }

    void print() {
      printf("%e\n", this->readCell(NULL));
    }
  };

  class ConstRegionData0D : public RegionDataI {

  private:
    ElementT _value;

  public:
    ConstRegionData0D() {
      _D = 0;
      _type = RegionDataTypes::CONSTREGIONDATA0D;
      _size = new IndexT[0];
    }

    ConstRegionData0D(ElementT value) {
      _D = 0;
      _type = RegionDataTypes::CONSTREGIONDATA0D;
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
      // (yod) TODO: uncomment this (problem with MatrixRegion::randomize())
      // JASSERT(false);
      _value = value;
    }

    DataHostList hosts(IndexT* /*begin*/, IndexT* /*end*/) {
      DataHostListItem item = {HostPid::self(), 1};
      return DataHostList(1, item);
    }

    void print() {
      printf("%e\n", this->readCell(NULL));
    }
  };
}

#endif
