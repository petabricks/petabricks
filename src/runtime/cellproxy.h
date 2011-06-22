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
      _handler = new RegionHandler(new RegionData0D());
      _handler->writeCell(NULL, val);
      _index = new IndexT[0];
    }

    ~CellProxy() {
      delete [] _index;
    }

    operator double () const {
      return _handler->readCell(_index);
    }

    CellProxy operator=(double val) {
      _handler->writeCell(_index, val);
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

    void print(std::ostream& o) const {
      o << "{" << _index[0];
      for (int i = 1; i < _handler->dimensions(); i++) {
        o << " , " << _index[i];
      }
      o << "}";
    }
    void print() const { print(std::cout); }
    std::string toString() const;
  };
}

std::ostream& operator<< (std::ostream& o, const petabricks::CellProxy& cell);

#endif
