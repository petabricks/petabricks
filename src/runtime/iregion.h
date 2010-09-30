#ifndef PETABRICKSIREGION_H
#define PETABRICKSIREGION_H

#define MATRIX_INDEX_T int

namespace petabricks {

template<int D, typename ElementT> class IRegion;
//template<int D, typename ElementT> class SplitRegion;

template<int D, typename ElementT>
class IRegion {
public:
  typedef MATRIX_INDEX_T IndexT;

  virtual ElementT* coordToPtr(IndexT coord[D]) = 0;

  IRegion<D, ElementT> region(IndexT start[D], IndexT end[D]);
  // SpliceRegion<D, ElementT> slice(int d, IndexT pos) = 0;
};

}

#endif
