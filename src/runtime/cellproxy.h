#ifndef PETABRICKSCELLPROXY_H
#define PETABRICKSCELLPROXY_H

#include "regiondata0D.h"
#include "regionhandler.h"

namespace petabricks {
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
      return (double)a + b;
    }
    friend double operator+(const int a,  const CellProxy& b) {
      return a + (double)b;
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
    friend double operator*(const CellProxy& a,  const int b) {
      return (double)a * b;
    }
    friend double operator*(const int a,  const CellProxy& b) {
      return a * (double)b;
    }

  };
}

#endif
