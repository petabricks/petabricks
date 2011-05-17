#ifndef PETABRICKSREGIONDATAI_H
#define PETABRICKSREGIONDATAI_H

#include "common/jrefcounted.h"
#include "matrixstorage.h"
#include "remotehost.h"

#include <vector>

namespace petabricks {
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  typedef uint8_t RegionDataType;
  struct RegionDataTypes {
    enum {
      REGIONDATA0D = 37,
      REGIONDATAPROXY,
      REGIONDATARAW,
      REGIONDATAREMOTE,
      REGIONDATASPLIT
    };
  };

  struct DataHostListItem {
    HostPid hostPid;
    double weight;
  };
  typedef std::vector<DataHostListItem> DataHostList;

  class RegionDataI;
  typedef jalib::JRef<RegionDataI> RegionDataIPtr;

  class RegionDataI : public jalib::JRefCounted {
  protected:
    int _D;
    RegionDataType _type;

    // _size is the size of this part, not the entire region
    IndexT* _size;

  public:
    ~RegionDataI();

    virtual int allocData() = 0;

    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    virtual MatrixStoragePtr storage() const {
      JASSERT(false)("This should not be called.");
      return NULL;
    }

    int dimensions();
    IndexT* size();

    RegionDataType type() const {
      return _type;
    }

    virtual DataHostList hosts(IndexT* begin, IndexT* end) = 0;

    // for tests
    int incCoord(IndexT* coord);
    virtual void print();
  };
}

#endif
