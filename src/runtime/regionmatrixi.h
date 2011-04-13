#ifndef PETABRICKSREGIONMATRIXI_H
#define PETABRICKSREGIONMATRIXI_H

#include "common/jrefcounted.h"
#include "regiondatai.h"
#include "regionhandler.h"

namespace petabricks {
  class RegionMatrixI;
  typedef jalib::JRef<RegionMatrixI> RegionMatrixIPtr;
  
  class CellProxy;

  class RegionMatrixI : public jalib::JRefCounted {
  protected:
    RegionHandlerPtr _regionHandler;
    RegionDataIPtr _regionData;
  
  public:
    virtual ElementT readCell(const IndexT* coord) = 0;
    virtual void writeCell(const IndexT* coord, ElementT value) = 0;

    virtual void acquireRegionData();
    virtual void releaseRegionData();

    RegionHandlerPtr getRegionHandler() const; 

    CellProxy& cell(IndexT x, IndexT y);
    CellProxy& cell(IndexT* coord);
  };

  class CellProxy {
  private:
    RegionMatrixIPtr _region;
    int* _index;
    
  public:
    CellProxy(RegionMatrixIPtr region, int x, int y) {
      _region = region;
      _index = new IndexT[2];
      _index[0] = x;
      _index[1] = y;
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
