#ifndef PETABRICKSREGIONTRANSFORM_H
#define PETABRICKSREGIONTRANSFORM_H

#include "regioncontiguous.h"

namespace petabricks {
  class RegionTransform : public RegionI {

  protected:
    RegionIPtr _regionContiguous;
    IndexT* _splitOffset;
    int _numSliceDimensions;
    int* _sliceDimensions;
    IndexT* _slicePositions;

  public:
    RegionTransform(RegionIPtr parent, int dimension, IndexT* size,
		    IndexT* splitOffset, int numSliceDimensions,
		    int* sliceDimensions, IndexT* slicePositions);

    RegionIPtr regionContiguous();

    ElementT* coordToPtr(const IndexT* coord);
    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);

  private:
    IndexT* getContiguousOffset(const IndexT* offset_orig);

  };

}

#endif
