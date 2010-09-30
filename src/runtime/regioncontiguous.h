#ifndef PETABRICKSCONTUGUOUSREGION_H
#define PETABRICKSCONTUGUOUSREGION_H

#include "iregion.h"

namespace petabricks {

template<int D, typename ElementT>
class ContiguousRegion : public IRegion <D, ElementT> {
public:
  typedef MATRIX_INDEX_T IndexT;

private:
  ElementT* _data;
  IndexT _size[D];

public:
  ContiguousRegion();
  ~ContiguousRegion();

  void allocate();
};

}

#endif
