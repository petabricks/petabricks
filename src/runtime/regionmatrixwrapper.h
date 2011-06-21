#ifndef PETABRICKSREGIONMATRIXWRAPPER_H
#define PETABRICKSREGIONMATRIXWRAPPER_H

#include "regionmatrix.h"

namespace petabricks {

  template< int D_, typename ElementT>
  class RegionMatrixWrapper : public RegionMatrix<D_, ElementT> {
  public:
    enum { D = D_ };
    typedef RegionMatrix<D, ElementT> Base;
    typedef const RegionMatrix<D, ElementT> ConstBase;

    RegionMatrixWrapper() : Base(D) {}
    RegionMatrixWrapper(IndexT* size) : Base(D, size) {}

    RegionMatrixWrapper(ElementT* data, IndexT* size) : Base(D, size) {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      Base::_regionHandler->allocData();

      IndexT i = 0;
      do {
        this->writeCell(coord, data[i]);
        i++;
      } while (this->incCoord(coord) >= 0);
    }

    RegionMatrixWrapper(const RegionMatrix<D, double>& that) : Base(that) {}

    // for testing
    void copyDataFromRegion(RegionMatrixWrapper in) {
      this->allocData();
      IndexT* coord = new IndexT[D];
      memset(coord, 0, sizeof(IndexT) * D);

      while (true) {
        this->writeCell(coord, in.readCell(coord));

        int z = this->incCoord(coord);
        if (z == -1) {
          break;
        }
      }
      delete [] coord;
    }
  };


  template<typename ElementT>
    class RegionMatrixWrapper<0, ElementT> : public RegionMatrix<0, ElementT> {
  private:
    int _sourceDimension;
    IndexT* _sourceIndex;

  public:
    enum { D = 0 };
    typedef RegionMatrix<D, ElementT> Base;

    RegionMatrixWrapper() : Base(0, (ElementT)0) {
      _sourceDimension = 0;
    }

    RegionMatrixWrapper(Base val) : Base(0, val.readCell(NULL)) {
      _sourceDimension = 0;
    }

    RegionMatrixWrapper(ElementT* data, IndexT* size) : Base(D, *data) {
     _sourceDimension = 0;
    }

    RegionMatrixWrapper(const RegionMatrixWrapper& that) : Base(0) {
      Base::_regionHandler = that.getRegionHandler();

      _sourceDimension = that._sourceDimension;
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, that._sourceIndex, sizeof(IndexT) * _sourceDimension);
    }

    ///
    /// Implicit conversion from ElementT/CellProxy
    RegionMatrixWrapper(ElementT value) : Base(0, value) {
      _sourceDimension = 0;

    }
    RegionMatrixWrapper(CellProxy& value) : Base(0) {
      Base::_regionHandler = value._handler;

      _sourceDimension = value._handler->dimensions();
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, value._index, sizeof(IndexT) * _sourceDimension);
    }
    RegionMatrixWrapper(const CellProxy& value) : Base(0) {
      Base::_regionHandler = value._handler;

      _sourceDimension = value._handler->dimensions();
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, value._index, sizeof(IndexT) * _sourceDimension);
    }

    ///
    /// Allow implicit conversion to CellProxy
    operator CellProxy& () const { return this->cell(); }

    RegionMatrixWrapper operator=(Base val) {
      this->cell() = val.readCell(NULL);
      return *this;
    }

    bool isSize() const{
      // TODO: what's this method suppossed to do??
      return true;
    }

    CellProxy& cell(IndexT x, ...) const {
      return cell();
    }
    CellProxy& cell(IndexT* coord) const {
      return cell();
    }
    INLINE CellProxy& cell() const {
      return Base::cell(_sourceIndex);
    }
  };


  namespace distributed {
    typedef RegionMatrixWrapper<0, double> MatrixRegion0D;
    typedef RegionMatrixWrapper<1, double> MatrixRegion1D;
    typedef RegionMatrixWrapper<2, double> MatrixRegion2D;
    typedef RegionMatrixWrapper<3, double> MatrixRegion3D;

    typedef RegionMatrixWrapper<0, double> ConstMatrixRegion0D;
    typedef RegionMatrixWrapper<1, double> ConstMatrixRegion1D;
    typedef RegionMatrixWrapper<2, double> ConstMatrixRegion2D;
    typedef RegionMatrixWrapper<3, double> ConstMatrixRegion3D;
  }

}

#endif
