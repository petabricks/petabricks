/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/
#ifndef PETABRICKSMATRIX_H
#define PETABRICKSMATRIX_H

#include "matrixstorage.h"
#include "common/hash.h"

#include <stdarg.h>
#include <stdio.h>

//#define GPU_TRACE 1

//
// Class structure looks like this:
//
//  MatrixRegion  (specialized for MatrixRegion0D and  ConstMatrixRegion0D)
//    extends
//  MatrixRegionVaArgsMethods (specialized a lot, for performance only; GCC optimizes VA_ARGS poorly)
//    extends
//  MatrixRegionBasicMethods
//    extends
//  MatrixRegionMembers (specialized for ConstMatrixRegion0D)
//
// Specializations can be found in matrixspecializations.h
//
// Now the main classes in reverse order of whats listed above:
//

namespace petabricks {


template<typename T>
inline void _regioncopy(MATRIX_ELEMENT_T* out, const T& in) {
    MATRIX_INDEX_T n = 0;
    MATRIX_INDEX_T coord[T::D];
    memset(coord, 0, sizeof coord);
    do {
      out[n++] = in.cell(coord);
    } while(in.incCoord(coord)>=0);
}

template<typename T>
inline void _regioncopy(const T& out, const MATRIX_ELEMENT_T* in) {
    MATRIX_INDEX_T n = 0;
    MATRIX_INDEX_T coord[T::D];
    memset(coord, 0, sizeof coord);
    do {
      out.cell(coord) = in[n++];
    } while(out.incCoord(coord)>=0);
}

template< int D, typename ElementT> class MatrixRegion;

//trick to break the cycle for SliceMatrixRegion going to MatrixRegion<-1>
template<int _D> struct _slicesize    { enum { D = _D-1 }; };
template< >      struct _slicesize<0> { enum { D = 0    }; };

/**
 * We have divided MatrixRegion into many smaller classes (combined through a chain of inheritance)
 * so that small parts may easily be specialized.
 *
 * This part defines some typedefs and simplifies the template args for the rest of the classes.
 */
template< int _D, typename _ElementT >
struct MatrixRegionTypeSpec {
  enum { D = _D };
  typedef _ElementT ElementT;
  typedef MATRIX_INDEX_T IndexT;
  typedef petabricks::MatrixStoragePtr StorageT;
  typedef petabricks::MatrixRegion<_slicesize<D>::D, ElementT> SliceMatrixRegion;
  typedef petabricks::MatrixRegion<D, MATRIX_ELEMENT_T> MutableMatrixRegion;
  typedef petabricks::MatrixRegion<D, ElementT>         MatrixRegion;
};

/**
 * We have divided MatrixRegion into many smaller classes (combined through a chain of inheritance)
 * so that small parts may easily be specialized.
 *
 * This part contains the member variables and storage.
 */
template< typename TypeSpec >
class MatrixRegionMembers {
public:
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::StorageT StorageT;
  typedef typename TypeSpec::IndexT   IndexT;
  typedef typename TypeSpec::ElementT ElementT;

  ///
  /// Constructor
  MatrixRegionMembers(const StorageT& s, ElementT* b, const IndexT RESTRICT* sizes, const IndexT RESTRICT * multipliers)
  {
    if(D>0) {
      const size_t sizeof_sizes = sizeof this->_sizes;
      const size_t sizeof_multipliers = sizeof this->_multipliers;

      if(multipliers != NULL)
        memcpy(this->_multipliers, multipliers, sizeof_multipliers );
      else
        memset(this->_multipliers, 0, sizeof_multipliers);

      if(sizes != NULL)
        memcpy(this->_sizes, sizes, sizeof_sizes);
      else
        memset(this->_sizes, -1, sizeof_sizes);
    }
    _base = b;
    _storage = s;
    if(count()>0){
      _storageInfo = new MatrixStorageInfo();
      exportTo(_storageInfo);
    }
  }

  ElementT* base() const { return _base; }
  const IndexT* sizes() const { return _sizes; }
  const IndexT* multipliers() const { return _multipliers; };
  const StorageT& storage() const { return _storage; }
  const MatrixStorageInfoPtr storageInfo() const {
#ifdef DEBUG
    JASSERT(count()>0)(count());
#endif
    return _storageInfo; 
  }

  void randomize(){
    if(D==0){
      //0D version may not use storage(), so just set the element directly
      JASSERT(base()!=0);
      *const_cast<MATRIX_ELEMENT_T*>(base()) = MatrixStorage::rand();
    }else{
      this->storage()->randomize();
    }
  }

  ///
  /// export to a more generic container (used in memoization)
  void exportTo(MatrixStorageInfo& ms) const {
    //std::cerr << "MATRIXREGION:: dimension = " << D << "count = " << this->count() << std::endl;
    ms.setStorage(_storage, _base);
    ms.setSizeMultipliers(D, _multipliers, _sizes);
    ms.setExtraVal();
  }

  ///
  /// Number of elements in this region
  ssize_t count() const {
    ssize_t s=1;
    for(int i=0; i<D; ++i)
      s*=this->sizes()[i];
    return s;
  }
  
  ///
  /// copy from a more generic container (used in memoization)
  void copyFrom(const MatrixStorageInfo& ms){
    #ifdef DEBUG
    JASSERT(_base!=0);
    #endif
    if(ms.storage()!=storage()){
      #ifdef DEBUG
      JASSERT(ms.storage()->count()==storage()->count());
      #endif
      memcpy(storage()->data(), ms.storage()->data(), storage()->count()*sizeof(ElementT));
    }
  }
  
  IndexT* sizes() { return _sizes; }
  IndexT* multipliers() { return _multipliers; };
private:
  StorageT _storage;
  MatrixStorageInfoPtr _storageInfo;
  ElementT* _base;
  IndexT _multipliers[D];
  IndexT _sizes[D];
};

/**
 * We have divided MatrixRegion into many smaller classes (combined through a chain of inheritance)
 * so that small parts may easily be specialized.
 *
 * This part contains most methods.
 */
template< typename TypeSpec >
class MatrixRegionBasicMethods : public MatrixRegionMembers< TypeSpec > {
public:
  typedef MatrixRegionMembers< TypeSpec > Base;
  MatrixRegionBasicMethods( const typename TypeSpec::StorageT& s
                          , typename TypeSpec::ElementT* b
                          , const typename TypeSpec::IndexT* sizes
                          , const typename TypeSpec::IndexT* multipliers)
    : Base(s,b,sizes,multipliers)
  {
    //_hasStorageInfo = false;
  }

  enum StockLayouts { LAYOUT_ASCENDING, LAYOUT_DECENDING };
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::StorageT            StorageT;
  typedef typename TypeSpec::IndexT              IndexT;
  typedef typename TypeSpec::ElementT            ElementT;
  typedef typename TypeSpec::MatrixRegion        MatrixRegion;
  typedef typename TypeSpec::SliceMatrixRegion   SliceMatrixRegion;
  typedef typename TypeSpec::MutableMatrixRegion MutableMatrixRegion;

  ///
  /// Allocate a storage for a new MatrixRegion
  static MatrixRegion allocate(const IndexT sizes[D]) {
    ssize_t s=1;
    for(int i=0; i<D; ++i)
      s*=sizes[i];
    MatrixStoragePtr tmp = new MatrixStorage(s);
    #ifdef DEBUG
    //in debug mode initialize matrix to garbage
    for(int i=0; i<s; ++i)
      tmp->data()[i] = -666;
    #endif
    return MatrixRegion(tmp, tmp->data(), sizes);
  }

  ///
  ///same as allocate unless this->sizes()==sizes
  bool isSize(const IndexT sizes[D]) const{
    if(this->base()==0) return false;
    for(int i=0; i<D; ++i){
      if(this->sizes()[i]!=sizes[i]){
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
  ///Return an iterator that accesses elements in a transposed fashion
  MatrixRegion transposed() const {
    IndexT sizes[D];
    IndexT multipliers[D];
    for(int i=0; i<D; ++i){
      sizes[i]       = this->sizes()[D-i-1];
      multipliers[i] = this->multipliers()[D-i-1];
    }
    return MatrixRegion(this->storage(), this->base(), sizes, multipliers);
  }

  ///
  /// A region is considered normalized if it occupies the entire buffer and is organized so that
  /// a N-dimensional buffer is laid out as sequential (N-1)-dimensional buffers.
  MatrixRegion asNormalizedRegion() const
  {
    bool copyData = true;
    if(isEntireBuffer()) {
      return MatrixRegion(this->storage(), this->base(), this->sizes(), this->multipliers());
    }

    MutableMatrixRegion t = MutableMatrixRegion::allocate((IndexT*)this->sizes());
    if( copyData ) {
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        t.cell(coord) = this->cell(coord);
      } while(this->incCoord(coord)>=0);
    }
    return t;
  }

  ///
  /// Decide to make a copy or return the orginal for making GPU buffer.s
	ElementT* getGpuInputBufferPtr() {
#ifdef HAVE_OPENCL
    if(isEntireBuffer()) {
      return this->base();
    }

		_gpuInputBuffer = new MatrixStorage(count());
		MutableMatrixRegion t = MutableMatrixRegion(_gpuInputBuffer, _gpuInputBuffer->data(), this->sizes());
    IndexT coord[D];
    memset(coord, 0, sizeof coord);
    do {
      t.cell(coord) = this->cell(coord);
    } while(this->incCoord(coord)>=0);
    //this->storageInfo()->addGpuInputBuffer(_gpuInputBuffer);
    // Store buffer in the global storage so that it won't be derefferenced before enqueueWriteBuffer is done
    CopyPendingMap::_pendingMap.addBuffer(_gpuInputBuffer);
    return _gpuInputBuffer->data();
#else
    UNIMPLEMENTED();
    return 0;
#endif
	}

  ///
  /// Decide to make a copy or return the orginal for making GPU buffer.
  /// Only use this for sequential code.
  MatrixRegion asGpuInputBuffer() const
  {
    if(isEntireBuffer()) {
      return MatrixRegion(this->storage(), this->base(), this->sizes(), this->multipliers());
    }

    MutableMatrixRegion t = MutableMatrixRegion::allocate((IndexT*)this->sizes());
    IndexT coord[D];
    memset(coord, 0, sizeof coord);
    do {
      t.cell(coord) = this->cell(coord);
    } while(this->incCoord(coord)>=0);
    return t;
  }

  ///
  /// Decide to make a copy or return the orginal for making GPU buffer.
  /// Only use this for sequential code.
  MatrixRegion asGpuOutputBuffer(const IndexT c1[D], const IndexT c2[D]) const
  {
    if(isEntireBuffer() &&  intervalSize(c1, c2) == count()) {
      return MatrixRegion(this->storage(), this->base(), this->sizes(), this->multipliers());
    }

    MutableMatrixRegion t = MutableMatrixRegion::allocate((IndexT*)this->sizes());
    return t;
  }


  INLINE ssize_t intervalSize(const IndexT c1[D], const IndexT c2[D]) const
  { ssize_t s=1;
    for(int i=0; i<D; ++i) {
      s*=c2[i] - c1[i];
    }
    return s;
  }

  void print() {
    std::cerr << "dimension = " << D << std::endl;
    std::cerr << "size = ";
    for(int i=0; i<D; ++i) {
      std::cerr << size(i) << " ";
    }    
    std::cerr << std::endl << "mult = ";
    for(int i=0; i<D; ++i) {
      std::cerr << this->multipliers()[i] << " ";
    }
    std::cerr << std::endl;
  }

  ///
  /// Copy data of this to dst
  void copyTo(const MutableMatrixRegion& dst)
  {
    if(this->storage() == dst.storage())
      return;
    IndexT coord[D] = {0};
    do {
      dst.cell(coord) = this->cell(coord);
    } while(this->incCoord(coord)>=0);
  }
  
  ///
  /// Copy data within the boundary c1 and c2 of this to dst
  void copyTo(const MutableMatrixRegion& dst,const IndexT c1[D], const IndexT c2[D])
  {
#ifdef GPU_TRACE
    std::cout << "copyTo boundary" << std::endl;
    for(int i = 0; i < D; i++)
      std::cout << "[" << i << "] : " << c1[i] << "-" << c2[i] << std::endl;
#endif
    if(this->storage() == dst.storage())
      return;
    for(int i = 0; i < D; i++)
      if(c1[i] >= c2[i])
        return;
    IndexT coord[D];
    memcpy(coord, c1, sizeof coord);
    do {
      dst.cell(coord) = this->cell(coord);
    } while(this->incCoordWithBound(coord, c1, c2)>=0);
  }

  ///
  /// Copy data within the given boundaries of this to dst
  void copyTo(const MutableMatrixRegion& dst,std::vector<IndexT*>& begins, std::vector<IndexT*>& ends)
  {
    #ifdef DEBUG
    JASSERT(begins.size() == ends.size())(begins.size())(ends.size());
    #endif
    if(this->storage() == dst.storage())
      return;

    if(begins.size() == 0){
      copyTo(dst);
    }
    else{
      for(size_t i = 0; i < begins.size(); ++i)
        copyTo(dst, begins[i], ends[i]);
    }
  }


  ///
  /// Copy data of src to this
  void copyFrom_unsafe(MatrixRegion& src) const
  {
#ifdef GPU_TRACE
    std::cout << "copyFrom all" << std::endl;
#endif
    if(this->storage() == src.storage())
      return;
    IndexT coord[D];
    memset(coord, 0, sizeof coord);
    do {
      *const_cast<MATRIX_ELEMENT_T*>(this->coordToPtr(coord)) = src.cell(coord);
    } while(this->incCoord(coord)>=0);
  }

  ///
  /// Copy data within the boundary c1 and c2 of src to this
  void copyFrom_unsafe(MatrixRegion& src,const IndexT c1[D], const IndexT c2[D]) const
  {
#ifdef GPU_TRACE
    std::cout << "copyFrom boundary" << std::endl;
    for(int i = 0; i < D; i++)
      std::cout << "[" << i << "] : " << c1[i] << "-" << c2[i] << std::endl;
#endif
    if(this->storage() == src.storage())
      return;
    for(int i = 0; i < D; i++)
      if(c1[i] >= c2[i])
        return;
    IndexT coord[D];
    memcpy(coord, c1, sizeof coord);
    do {
      *const_cast<MATRIX_ELEMENT_T*>(this->coordToPtr(coord)) = src.cell(coord);
    } while(this->incCoordWithBound(coord, c1, c2)>=0);
  }

  ///
  /// Copy data within the given boundaries of src to this
  void copyFrom_unsafe(MatrixRegion& src,std::vector<IndexT*>& begins, std::vector<IndexT*>& ends) const
  {
    #ifdef DEBUG
    JASSERT(begins.size() == ends.size())(begins.size())(ends.size());
    #endif
    if(this->storage() == src.storage())
      return;

    if(begins.size() == 0){
      copyFrom_unsafe(src);
    }
    else{
      for(size_t i = 0; i < begins.size(); ++i)
        copyFrom_unsafe(src, begins[i], ends[i]);
    }
  }

  void useOnCpu() {
#ifdef HAVE_OPENCL
    if(D == 0) return;
    this->storage()->lock();
    std::set<MatrixStorageInfoPtr>& pendings = CopyPendingMap::_pendingMap.allPendings(this->storage());
    for(std::set<MatrixStorageInfoPtr>::iterator it = pendings.begin(); it != pendings.end(); ++it) {
      MatrixStoragePtr storage = (*it)->processPending();
      if(storage) {
        #ifdef GPU_TRACE
        std::cout << "something on gpu..." << std::endl;
        #endif
        //MatrixRegion normalized(storage, storage->data(), this->sizes());
        //copyFrom_unsafe(normalized, (*it)->getBegins(), (*it)->getEnds());
        (*it)->copy((MatrixStoragePtr&) this->storage(), storage, (*it)->getBegins(), (*it)->getEnds());
        (*it)->resetPending();
      }
    }
    CopyPendingMap::_pendingMap.clearPendings(this->storage());
    this->storage()->unlock();
    //CopyPendingMap::_pendingMap.print();
#endif
  }

  ///
  /// Access a single cell of target matrix
  INLINE ElementT& cell(const IndexT c1[D]) const{ return *this->coordToPtr(c1); }

  ///
  /// Create a new iterator for a region of target matrix
  MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{
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
    return MatrixRegion(this->storage(), this->coordToPtr(c1), newSizes, this->multipliers());
  }

  ///
  /// Return a slice through this dimension
  /// The iterator is one dimension smaller and equivilent to always
  /// giving pos for dimension d
  SliceMatrixRegion slice(int d, IndexT pos) const{
    #ifdef DEBUG
    JASSERT(d>=0 && d<D)(d).Text("invalid dimension");
    JASSERT(pos>=0 && pos<size(d))(pos)(size(d)).Text("out of bounds access");
    #endif
    IndexT sizes[D-1];
    IndexT mult[D-1];
    for(int i=0; i<d; ++i){
        sizes[i] = this->sizes()[i];
        mult[i]  = this->multipliers()[i];
    }
    for(int i=d+1; i<D; ++i){
        sizes[i-1] = this->sizes()[i];
        mult[i-1]  = this->multipliers()[i];
    }
    IndexT coord[D];
    memset(coord, 0, sizeof coord);
    coord[d] = pos;
    return SliceMatrixRegion(this->storage(), this->coordToPtr(coord), sizes, mult);
  }


  SliceMatrixRegion col(IndexT x) const{ return slice(0, x); }
  SliceMatrixRegion column(IndexT x) const{ return slice(0, x); }
  SliceMatrixRegion row(IndexT y) const{  return slice(1, y); }

  ///
  /// true if c1 is in bounds
  bool contains(const IndexT coord[D]) const {
    for(int i=0; i<D; ++i)
      if(coord[i]<0 || coord[i]>=size(i))
        return false;
    return true;
  }

  ///
  /// Return the size of a given dimension
  IndexT size(int d) const {
    #ifdef DEBUG
    JASSERT(d>=0 && d<D)(d)((int)D);
    #endif
    return this->sizes()[d];
  }


  IndexT width() const { return size(0); }
  IndexT height() const { return size(1); }
  IndexT depth() const { return size(2); }
  MatrixRegion all() const { return MatrixRegion(this->storage(), this->base(), this->sizes(), this->multipliers()); }

  ///
  /// Number of elements in this region
  ssize_t count() const {
    ssize_t s=1;
    for(int i=0; i<D; ++i)
      s*=this->sizes()[i];
    return s;
  }

  ///
  /// sum of the sizes in each dimension
  IndexT perimeter() const {
    IndexT s=0;
    for(int i=0; i<D; ++i)
      s+=this->sizes()[i];
    return s;
  }

  ///
  /// Number of bytes taken to store the elements in this region
  ssize_t bytes() const {
    return count()*sizeof(ElementT);
  }

  ///
  /// force this region to be a mutable type (removes constness)
  /// this is evil
  MutableMatrixRegion forceMutable() {
    return MutableMatrixRegion(
        this->storage(),
        (MATRIX_ELEMENT_T*) this->base(),
        this->sizes(),
        this->multipliers());
  }

  ///
  /// true if this region occupies the entire buffer _storage
  bool isEntireBuffer() const {
    if(D==0) return true;
    return this->storage() && (ssize_t)this->storage()->count()==count();
  }

  ///
  /// increment a raw coord in ascending order
  /// return largest dimension incremented or -1 for end
  int incCoord(IndexT coord[D]) const{
    if(D==0)
     return -1;
    int i;
    coord[0]++;
    for(i=0; i<D-1; ++i){
      if(coord[i] >= this->size(i)){
        coord[i]=0;
        coord[i+1]++;
      }else{
        return i;
      }
    }
    if(coord[D-1] >= this->size(D-1)){
      return -1;
    }else{
      return D-1;
    }
  }

  bool isLocal() const { return true; }

  ///
  /// increment a raw coord in ascending order with in the given boundary
  /// return largest dimension incremented or -1 for end
  int incCoordWithBound(IndexT coord[D], const IndexT c1[D], const IndexT c2[D]) const{
    if(D==0)
     return -1;
    int i;
    coord[0]++;
    for(i=0; i<D-1; ++i){
      if(coord[i] >= c2[i]){
        coord[i]=c1[i];
        coord[i+1]++;
      }else{
        return i;
      }
    }
    if(coord[D-1] >= c2[D-1]){
      return -1;
    }else{
      return D-1;
    }
  }

  ///
  /// hash the content of this to gen
  void hash(jalib::HashGenerator& gen) const{
    if(this->count()>0){
      IndexT coord[D];
      memset(coord, 0, sizeof coord);
      do {
        float temp = *(this->coordToPtr(coord));
        if (fpclassify(temp) == FP_ZERO) temp = 0;
        if (fpclassify(temp) == FP_NAN) temp = fabs(temp);
        gen.update(&temp, sizeof(temp));
      } while(this->incCoord(coord)>=0);
    }
  }

protected:
  bool _hasStorageInfo;
  ///
  /// Compute the offset in _base for a given coordinate
  ElementT* coordToPtr(const IndexT coord[D]) const{
    IndexT rv = 0;
    for(int i=0; i<D; ++i){
      #ifdef DEBUG
      /*JASSERT(0<=coord[i] && coord[i]<size(i))(coord[i])(size(i))
        .Text("Out of bounds access");*/
      #endif
      rv +=  this->multipliers()[i] * coord[i];
    }
    return this->base()+rv;
  }

  ///
  /// Fill _multipliers with a stock layout
  void setStockLayout(StockLayouts layout){
    IndexT mult = 1;
    if(layout == LAYOUT_ASCENDING){
      for(int i=0; i<D; ++i){
        this->multipliers()[i] = mult;
        mult *= size(i);
      }
    }else{
      for(int i=D-1; i>=0; --i){
        this->multipliers()[i] = mult;
        mult *= size(i);
      }
    }
    if(count()>0)
      this->storageInfo()->setMultipliers(this->multipliers());
  }

#ifdef HAVE_OPENCL
  MatrixStoragePtr _gpuInputBuffer;
#endif
};

/**
 * We have divided MatrixRegion into many smaller classes (combined through a chain of inheritance)
 * so that small parts may easily be specialized.
 *
 * This part contains variable arg count methods.
 * GCC is bad at optimizing these, so we specialized these for commonly used types.
 */
template< typename TypeSpec >
class MatrixRegionVaArgsMethods : public MatrixRegionBasicMethods< TypeSpec > {
public:
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::IndexT       IndexT;
  typedef typename TypeSpec::ElementT     ElementT;
  typedef typename TypeSpec::MatrixRegion MatrixRegion;
  typedef MatrixRegionBasicMethods<TypeSpec> Base;
  MatrixRegionVaArgsMethods( const typename TypeSpec::StorageT& s
                          , typename TypeSpec::ElementT* b
                          , const typename TypeSpec::IndexT* sizes
                          , const typename TypeSpec::IndexT* multipliers)
    : Base(s,b,sizes,multipliers)
  {}

  //these passthroughs must be declared here for overloading to work
  INLINE ElementT& cell(IndexT c[D]) const{ return this->Base::cell(c); }
  INLINE MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{ return this->Base::region(c1,c2); }
  INLINE static MatrixRegion allocate(IndexT s[D]){ return Base::allocate(s); }

  ///
  /// Allocate a storage for a new MatrixRegion (va_args version)
  static MatrixRegion allocate(IndexT x, ...){
    IndexT c1[D];
    va_list ap;
    va_start(ap, x);
    c1[0]=x;
    for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
    va_end(ap);
    return allocate(c1);
  }

  ///
  /// Access a single cell of target matrix
  ElementT& cell(IndexT x, ...) const{
    IndexT c1[D];
    va_list ap;
    va_start(ap, x);
    c1[0]=x;
    for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
    va_end(ap);
    return cell(c1);
  }


  ///
  /// true if coord is in bounds
  bool contains(IndexT x, ...) const {
    IndexT c1[D];
    va_list ap;
    va_start(ap, x);
    c1[0]=x;
    for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
    va_end(ap);
    return contains(c1);
  }

  ///
  /// Create a new iterator for a region of target matrix
  MatrixRegion region(IndexT x, ...) const{
    IndexT c1[D], c2[D];
    va_list ap;
    va_start(ap, x);
    c1[0]=x;
    for(int i=1; i<D; ++i) c1[i]=va_arg(ap, IndexT);
    for(int i=0; i<D; ++i) c2[i]=va_arg(ap, IndexT);
    va_end(ap);
    return region(c1,c2);
  }

};


/**
 * We have divided MatrixRegion into many smaller classes (combined through a chain of inheritance)
 * so that small parts may easily be specialized.
 *
 * This part contains the constructors, and is the final class called by the user.
 */
template< int D, typename ElementT>
class MatrixRegion : public MatrixRegionVaArgsMethods<MatrixRegionTypeSpec<D, ElementT> >{
public:
  typedef petabricks::MatrixRegionTypeSpec<D, ElementT> TypeSpec;
  typedef typename TypeSpec::IndexT IndexT;
  typedef typename TypeSpec::MutableMatrixRegion MutableMatrixRegion;
  typedef MatrixRegionVaArgsMethods<TypeSpec> Base;

  ///
  /// Constructor with a given layout
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , const IndexT multipliers[D])
    : Base(s, b, sizes, multipliers)
  {}

  ///
  /// Constructor with a stock layout
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , typename Base::StockLayouts layout = Base::LAYOUT_ASCENDING)
    : Base(s, b, sizes, NULL)
  {
    this->setStockLayout(layout);
  }

  ///
  /// Copy constructor
  MatrixRegion( const MutableMatrixRegion& that )
    : Base(that.storage(), that.base(), that.sizes(), that.multipliers())
  {}

  ///
  /// Default constructor
  MatrixRegion() : Base(NULL, NULL, NULL, NULL) {}

};

} /* namespace petabricks*/

// specializations are a bit verbose, so we push them to their own file:
#include "matrixspecializations.h"


namespace petabricks {
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;

  //some typedefs:
  namespace sequential {
    typedef MatrixRegion<0,  MATRIX_ELEMENT_T> MatrixRegion0D;
    typedef MatrixRegion<1,  MATRIX_ELEMENT_T> MatrixRegion1D;
    typedef MatrixRegion<2,  MATRIX_ELEMENT_T> MatrixRegion2D;
    typedef MatrixRegion<3,  MATRIX_ELEMENT_T> MatrixRegion3D;
    typedef MatrixRegion<4,  MATRIX_ELEMENT_T> MatrixRegion4D;
    typedef MatrixRegion<5,  MATRIX_ELEMENT_T> MatrixRegion5D;
    typedef MatrixRegion<6,  MATRIX_ELEMENT_T> MatrixRegion6D;
    typedef MatrixRegion<7,  MATRIX_ELEMENT_T> MatrixRegion7D;
    typedef MatrixRegion<8,  MATRIX_ELEMENT_T> MatrixRegion8D;
    typedef MatrixRegion<9,  MATRIX_ELEMENT_T> MatrixRegion9D;

    typedef MatrixRegion<0,  const MATRIX_ELEMENT_T> ConstMatrixRegion0D;
    typedef MatrixRegion<1,  const MATRIX_ELEMENT_T> ConstMatrixRegion1D;
    typedef MatrixRegion<2,  const MATRIX_ELEMENT_T> ConstMatrixRegion2D;
    typedef MatrixRegion<3,  const MATRIX_ELEMENT_T> ConstMatrixRegion3D;
    typedef MatrixRegion<4,  const MATRIX_ELEMENT_T> ConstMatrixRegion4D;
    typedef MatrixRegion<5,  const MATRIX_ELEMENT_T> ConstMatrixRegion5D;
    typedef MatrixRegion<6,  const MATRIX_ELEMENT_T> ConstMatrixRegion6D;
    typedef MatrixRegion<7,  const MATRIX_ELEMENT_T> ConstMatrixRegion7D;
    typedef MatrixRegion<8,  const MATRIX_ELEMENT_T> ConstMatrixRegion8D;
    typedef MatrixRegion<9,  const MATRIX_ELEMENT_T> ConstMatrixRegion9D;
  }

  namespace workstealing {
    typedef sequential::MatrixRegion0D MatrixRegion0D;
    typedef sequential::MatrixRegion1D MatrixRegion1D;
    typedef sequential::MatrixRegion2D MatrixRegion2D;
    typedef sequential::MatrixRegion3D MatrixRegion3D;
    typedef sequential::MatrixRegion4D MatrixRegion4D;
    typedef sequential::MatrixRegion5D MatrixRegion5D;
    typedef sequential::MatrixRegion6D MatrixRegion6D;
    typedef sequential::MatrixRegion7D MatrixRegion7D;
    typedef sequential::MatrixRegion8D MatrixRegion8D;
    typedef sequential::MatrixRegion9D MatrixRegion9D;

    typedef sequential::ConstMatrixRegion0D ConstMatrixRegion0D;
    typedef sequential::ConstMatrixRegion1D ConstMatrixRegion1D;
    typedef sequential::ConstMatrixRegion2D ConstMatrixRegion2D;
    typedef sequential::ConstMatrixRegion3D ConstMatrixRegion3D;
    typedef sequential::ConstMatrixRegion4D ConstMatrixRegion4D;
    typedef sequential::ConstMatrixRegion5D ConstMatrixRegion5D;
    typedef sequential::ConstMatrixRegion6D ConstMatrixRegion6D;
    typedef sequential::ConstMatrixRegion7D ConstMatrixRegion7D;
    typedef sequential::ConstMatrixRegion8D ConstMatrixRegion8D;
    typedef sequential::ConstMatrixRegion9D ConstMatrixRegion9D;
  }

} /* namespace petabricks*/

#endif
