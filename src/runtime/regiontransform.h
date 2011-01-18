#ifndef PETABRICKSREGIONTRANSFORM_H
#define PETABRICKSREGIONTRANSFORM_H

#include "regioni.h"

namespace petabricks {
  class RegionTransform : public RegionI {

  protected:
    RegionIPtr _baseRegion;
    IndexT* _splitOffset;
    int _numSliceDimensions;
    int* _sliceDimensions;
    IndexT* _slicePositions;

  public:
    RegionTransform(RegionIPtr parent, int dimension, IndexT* size,
		    IndexT* splitOffset, int numSliceDimensions,
		    int* sliceDimensions, IndexT* slicePositions);

    RegionIPtr baseRegion();

    RegionIPtr splitRegion(IndexT* offset, IndexT* size);
    RegionIPtr sliceRegion(int d, IndexT pos);

    ElementT* coordToPtr(const IndexT* coord);
    ElementT readCell(const IndexT* coord);
    void writeCell(const IndexT* coord, ElementT value);

  private:
    IndexT* getBaseRegionOffset(const IndexT* offset_orig);

  };

}

#endif
