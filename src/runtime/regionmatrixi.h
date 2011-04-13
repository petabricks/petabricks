#ifndef PETABRICKSREGIONMATRIXI_H
#define PETABRICKSREGIONMATRIXI_H

#include "common/jrefcounted.h"
#include "regiondatai.h"
#include "regionhandler.h"

#include <string.h>

namespace petabricks {
  class RegionMatrixI;
  typedef jalib::JRef<RegionMatrixI> RegionMatrixIPtr;
  
  class CellProxy;

  class RegionMatrixI : public jalib::JRefCounted {
  protected:
    int _D;
    RegionHandlerPtr _regionHandler;
    RegionDataIPtr _regionData;
  
  public:
    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    virtual void acquireRegionData();
    virtual void releaseRegionData();

    RegionHandlerPtr getRegionHandler() const; 
 
    int dimensions() const {return _D;}

    CellProxy& cell(IndexT x, ...);
    CellProxy& cell(IndexT* coord);
  };

  class CellProxy {
  private:
    RegionMatrixIPtr _region;
    IndexT* _index;
    
  public:
    CellProxy(RegionMatrixIPtr region, IndexT* coord) {
      _region = region;

      int D = _region->dimensions();
      _index = new IndexT[D];
      memcpy(_index, coord, sizeof(IndexT) * D);
    }
    
    operator double () const {
      return _region->readCell(_index);
    }
    
    CellProxy operator=(double val) {
      _region->writeCell(_index, val);
      return *this;
    }
    
    CellProxy operator=(const CellProxy& val) { 
      *this = (double)val;
      return *this;
    }
    
    friend double operator+(const CellProxy& a,  const CellProxy& b) {
      return (double)a + (double)b;
    }
  };
}

#endif
