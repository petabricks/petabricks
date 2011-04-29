#ifndef PETABRICKSREGIONMATRIXI_H
#define PETABRICKSREGIONMATRIXI_H

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "regiondata0D.h"
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

    virtual CellProxy& cell(IndexT x, ...) const;
    virtual CellProxy& cell(IndexT* coord) const;
 
    INLINE CellProxy& cell() const {
      IndexT c1[0];
      return this->cell(c1);
    }
  };

  class CellProxy {
  public:
    RegionHandlerPtr _handler;
    IndexT* _index;

  public:
    CellProxy(RegionHandlerPtr handler, IndexT* coord) {
      _handler = handler;

      int D = _handler->dimensions();
      _index = new IndexT[D];
      memcpy(_index, coord, sizeof(IndexT) * D);
    }
    
    CellProxy(const CellProxy& that) {
      _handler = that._handler;

      int D = _handler->dimensions();
      _index = new IndexT[D];
      memcpy(_index, that._index, sizeof(IndexT) * D);
    }
    
    CellProxy(ElementT val) {
      _handler = new RegionHandler(new RegionData0D(val));
      _index = new IndexT[0];
    }
    
    ~CellProxy() {
      delete [] _index;
    }
    
    operator double () const {
      double val = _handler->acquireRegionData(this)->readCell(_index);
      _handler->releaseRegionData(this);
      return val;
    }
    
    CellProxy operator=(double val) {
      _handler->acquireRegionData(this)->writeCell(_index, val);      
      _handler->releaseRegionData(this);
      return *this;
    }
    
    CellProxy operator=(const CellProxy& val) {
      *this = (double)val;
      return *this;
    }

    CellProxy operator+=(const CellProxy& val) { 
      *this = (double)*this + (double)val;
      return *this;
    }

    CellProxy operator+=(const double val) { 
      *this = (double)*this + val;
      return *this;
    }

    CellProxy operator-() const {
      return (-1) * (double)*this;
    }
    
    friend double operator+(const CellProxy& a, const CellProxy& b) {
      return (double)a + (double)b;
    }
    friend double operator+(const CellProxy& a,  const double b) {
      return (double)a + b;
    }
    friend double operator+(const double a,  const CellProxy& b) {
      return a + (double)b;
    }
    friend double operator+(const CellProxy& a,  const long int b) {
      return (double)a + (double)b;
    }
    friend double operator+(const long int a,  const CellProxy& b) {
      return (double)a + (double)b;
    }
    friend double operator+(const CellProxy& a,  const int b) {
      return (double)a + (double)b;
    }
    friend double operator+(const int a,  const CellProxy& b) {
      return (double)a + (double)b;
    }


    friend double operator*(const CellProxy& a,  const CellProxy& b) {
      return (double)a * (double)b;
    }
    friend double operator*(const CellProxy& a,  const double b) {
      return (double)a * b;
    }
    friend double operator*(const double a,  const CellProxy& b) {
      return a * (double)b;
    }
  };
}

#endif
