#ifndef PETABRICKSREGIONTRANSFORM_H
#define PETABRICKSREGIONTRANSFORM_H

#include "regioncontiguous.h"

namespace petabricks {
  class RegionTransform : public RegionI {

  protected:
    RegionContiguousPtr _regionContiguous;
    IndexT* _splitOffset;
    int _numSliceDimensions;
    int* _sliceDimensions;
    IndexT* _slicePositions;

  public:
    RegionTransform(RegionContiguousPtr parent, int dimension, IndexT* size,
		    IndexT* splitOffset, int numSliceDimensions,
		    int* sliceDimensions, IndexT* slicePositions);

    RegionContiguousPtr regionContiguous() const;

    ElementT* coordToPtr(const IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);

  private:
    IndexT* getContiguousOffset(const IndexT* offset_orig);

  };

}

#endif
