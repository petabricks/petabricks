#ifndef PETABRICKSPLITREGION_H
#define PETABRICKSPLITREGION_H

#include "contiguousregion.h"

namespace petabricks {

template<int D, typename ElementT>
class SplitRegion : public IRegion <D, ElementT> {
public:
  typedef MATRIX_INDEX_T IndexT;

private:
  IRegion<D, ElementT> _parent;
  IndexT _start[D];
  IndexT _end[D];

public:
  SplitRegion(IRegion<D, ElementT> parent, IndexT start[D], IndexT end[D]);
  ~SplitRegion();

  ContiguousRegion<D, ElementT> toContiguousRegion();
};

}

#endif
