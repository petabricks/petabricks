#ifndef PETABRICKSREGIONMATRIXWRAPPER_H
#define PETABRICKSREGIONMATRIXWRAPPER_H

#include "petabricksruntime.h"
#include "matrixstorage.h"
#include "regionmatrix.h"

namespace petabricks {

  template< int D, typename ElementT>
  class RegionMatrixWrapper : public RegionMatrix {
  public:
    RegionMatrixWrapper() : RegionMatrix(D) {}
    RegionMatrixWrapper(IndexT* size) : RegionMatrix(D, size) {}

    RegionMatrixWrapper(ElementT* data, IndexT* size) : RegionMatrix(D, size) {
      IndexT coord[D];
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

    RegionMatrixWrapper(const RegionMatrix& that) : RegionMatrix(that) {}

    // For 0D
    RegionMatrixWrapper(ElementT value) : RegionMatrix(D, value) {
      JASSERT(D==0)("This constrictor is for 0D.");
    }

    static RegionMatrixWrapper allocate(IndexT* size) {
      RegionMatrixWrapper region = RegionMatrixWrapper<D, ElementT>(size);
      region.allocData();
      return region;
    }

    static RegionMatrixWrapper allocate(IndexT x, ...) {
      IndexT c1[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      va_end(ap);
      return allocate(c1);
    }

    inline static RegionMatrixWrapper allocate() {
      IndexT c1[D];
      return allocate(c1);
    }

    bool isSize(const IndexT size[D]) const{
      if (!_size) {
	return false;
      }
      for(int i=0; i<D; ++i){
	if(this->size()[i] != size[i]){
	  return false;
	}
      }
      return true;
    }

    bool isSize(IndexT x, ...) const{
      IndexT c1[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      va_end(ap);
      return isSize(c1);
    }

    ///
    /// Number of elements in this region
    ssize_t count() const {
      ssize_t s=1;
      for(int i=0; i<D; ++i)
	s*=this->size()[i];
      return s;
    }

    RegionMatrixWrapper region(const IndexT c1[D], const IndexT c2[D]) const{
      IndexT newSizes[D];
      for(int i=0; i<D; ++i){
        #ifdef DEBUG
	JASSERT(c1[i]<=c2[i])(c1[i])(c2[i])
	  .Text("region has negative size");
	JASSERT(c2[i]<=size(i))(c2[i])(size(i))
	  .Text("region goes out of bounds");
        #endif
	newSizes[i]=c2[i]-c1[i];
      }
      return RegionMatrixWrapper((const RegionMatrix&)this->splitRegion(c1, newSizes));
    }

    RegionMatrixWrapper region(IndexT x, ...) const{
      IndexT c1[D], c2[D];
      va_list ap;
      va_start(ap, x);
      c1[0]=x;
      for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
      for(int i=0; i<D; ++i) c2[i]=va_arg(ap, IndexT);
      va_end(ap);
      return region(c1,c2);
    }

    RegionMatrixWrapper slice(int d, IndexT pos) const{
      return RegionMatrixWrapper<D-1, ElementT>((const RegionMatrix&)this->sliceRegion(d, pos));
    }

    RegionMatrixWrapper transposed() const {
      return RegionMatrixWrapper((const RegionMatrix&)this->transposedRegion());
    }

    RegionMatrixWrapper col(IndexT x) const{ return slice(0, x); }
    RegionMatrixWrapper column(IndexT x) const{ return slice(0, x); }
    RegionMatrixWrapper row(IndexT y) const{  return slice(1, y); }

    IndexT width() const { return size(0); }
    IndexT height() const { return size(1); }
    IndexT depth() const { return size(2); }

    RegionMatrixWrapper forceMutable() {
      return RegionMatrixWrapper((const RegionMatrix&) *this);
    }

    ElementT rand(){
      return PetabricksRuntime::randDouble(-2147483648, 2147483648);
    }

    void randomize() {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      this->acquireRegionData();
      do {
	this->writeCell(coord, this->rand());
      } while (this->incCoord(coord) >= 0);
      this->releaseRegionData();
    }

    void hash(jalib::HashGenerator& gen) {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      this->acquireRegionData();
      do {
	ElementT v = this->readCell(coord);
	gen.update(&v, sizeof(ElementT));
      } while (this->incCoord(coord) >= 0);
      this->releaseRegionData();
    }

    //
    // Cast to MatrixRegion
    operator MatrixRegion<D, ElementT> () const{
      RegionDataIPtr regionData = this->acquireRegionDataConst();
      JASSERT(regionData->type() == RegionDataTypes::REGIONDATARAW).Text("Cannot cast to MatrixRegion.");

      IndexT startOffset = 0;
      IndexT multipliers[D];

      IndexT mult = 1;
      int last_slice_index = 0;
      for(int i = 0; i < regionData->dimensions(); i++){
	if ((last_slice_index < _numSliceDimensions) &&
	    (i == _sliceDimensions[last_slice_index])) {
	  startOffset += mult * _slicePositions[last_slice_index];
	  last_slice_index++;
	} else {
	  multipliers[i - last_slice_index] = mult;

	  if (_splitOffset) {
	    startOffset += mult * _splitOffset[i - last_slice_index];
	  }
	}

	mult *= regionData->size()[i];
      }

      MatrixRegion<D, ElementT> matrixRegion =
	MatrixRegion<D, ElementT>(regionData->storage(), regionData->storage()->data() + startOffset, _size, multipliers);


      this->releaseRegionDataConst();

      if (_isTransposed) {
	matrixRegion = matrixRegion.transposed();
      }

      return matrixRegion;
    }

  };


  template<typename ElementT>
  class RegionMatrixWrapper0D : public RegionMatrixWrapper<0, ElementT> {
  private:
    int _sourceDimension;
    IndexT* _sourceIndex;

  public:
    enum { D = 0 };
    typedef RegionMatrixWrapper<0, ElementT> Base;

    RegionMatrixWrapper0D() : Base((ElementT)0) {
      _sourceDimension = 0;
    }

    RegionMatrixWrapper0D(Base val) : Base(val.readCell(NULL)) {
      _sourceDimension = 0;
    }

    RegionMatrixWrapper0D(const RegionMatrixWrapper0D& that) : Base() {
      Base::_regionHandler = that.getRegionHandler();

      _sourceDimension = that._sourceDimension;
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, that._sourceIndex, sizeof(IndexT) * _sourceDimension);
    }

    ///
    /// Implicit conversion from ElementT/CellProxy
    RegionMatrixWrapper0D(ElementT value) : Base(value) {
      _sourceDimension = 0;
    }
    RegionMatrixWrapper0D(CellProxy& value) : Base() {
      Base::_regionHandler = value._handler;

      _sourceDimension = value._handler->dimensions();
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, value._index, sizeof(IndexT) * _sourceDimension);
    }
    RegionMatrixWrapper0D(const CellProxy& value) : Base() {
      Base::_regionHandler = value._handler;

      _sourceDimension = value._handler->dimensions();
      _sourceIndex = new IndexT[_sourceDimension];
      memcpy(_sourceIndex, value._index, sizeof(IndexT) * _sourceDimension);
    }

    ///
    /// Allow implicit conversion to CellProxy
    operator CellProxy& () const { return this->cell(); }

    RegionMatrixWrapper0D operator=(Base val) {
      this->writeCell(NULL, val.readCell(NULL));
      return *this;
    }

    bool isSize() const{
      // TODO: what's this method suppossed to do??
      return true;
    }

    INLINE CellProxy& cell() const {
      return Base::cell(_sourceIndex);
    }
  };


  typedef RegionMatrixWrapper0D<double> RegionMatrix0D;
  typedef RegionMatrixWrapper<1, double> RegionMatrix1D;
  typedef RegionMatrixWrapper<2, double> RegionMatrix2D;
  typedef RegionMatrixWrapper<3, double> RegionMatrix3D;

  typedef RegionMatrix0D ConstRegionMatrix0D;
  typedef RegionMatrix1D ConstRegionMatrix1D;
  typedef RegionMatrix2D ConstRegionMatrix2D;
  typedef RegionMatrix3D ConstRegionMatrix3D;

}

#endif
