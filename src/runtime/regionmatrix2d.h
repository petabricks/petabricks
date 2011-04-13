#ifndef PETABRICKSREGIONMATRIX2D_H
#define PETABRICKSREGIONMATRIX2D_H

#include "petabricksruntime.h"
#include "regionmatrix.h"

namespace petabricks {

  class RegionMatrix2D : public RegionMatrix {
  public:
    RegionMatrix2D() : RegionMatrix(2) {}
    RegionMatrix2D(IndexT* size) : RegionMatrix(2, size) {}
 
    RegionMatrix2D(ElementT* data, IndexT* size) : RegionMatrix(2, size) {
      IndexT coord[_D];
      memset(coord, 0, sizeof coord);
      this->acquireRegionData();
      _regionData->allocData();

      IndexT i = 0;
      do {
	this->writeCell(coord, data[i]);
	i++;
      } while (this->incCoord(coord) >= 0);
      this->releaseRegionData();
    }

    static RegionMatrix2D allocate(IndexT w, IndexT h) {
      IndexT size[] = {w,h};
      return RegionMatrix2D(size);
    }

    bool isSize(IndexT size1, IndexT size2) {
      IndexT size[] = {size1,size2};
      for (int i = 0; i < 2; i++) {
	if (_size[i] != size[i]) {
	  return false;
	}
      }
      return true;
    }

    ElementT rand(){
      return PetabricksRuntime::randDouble(-2147483648, 2147483648);
    }

    void randomize() {
      IndexT coord[_D];
      memset(coord, 0, sizeof coord);
      this->acquireRegionData();      
      do {
	this->writeCell(coord, this->rand());
      } while (this->incCoord(coord) >= 0);
      this->releaseRegionData();
    }

    void hash(jalib::HashGenerator& gen) {
      IndexT coord[_D];
      memset(coord, 0, sizeof coord);
      this->acquireRegionData();      
      do {
	ElementT v = this->readCell(coord);
	gen.update(&v, sizeof(ElementT));
      } while (this->incCoord(coord) >= 0);
      this->releaseRegionData();
    }
  };

  typedef RegionMatrix2D ConstRegionMatrix2D;

}

#endif
