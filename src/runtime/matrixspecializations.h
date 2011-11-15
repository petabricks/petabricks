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
#ifndef PETABRICKSMATRIXSPECIALIZATIONS_H
#define PETABRICKSMATRIXSPECIALIZATIONS_H

#include "matrixregion.h"

namespace petabricks {

/**
 * Specialized MatrixRegion0D and ConstMatrixRegion0D, allow implicit casting to/from ElementT
 */
template<typename ElementT>
class MatrixRegion<0, ElementT >  : public MatrixRegionVaArgsMethods<MatrixRegionTypeSpec<0, ElementT> >{
public:
  enum { D = 0 };
  typedef petabricks::MatrixRegionTypeSpec<D, ElementT> TypeSpec;
  typedef typename TypeSpec::IndexT IndexT;
  typedef typename TypeSpec::MutableMatrixRegion MutableMatrixRegion;
  typedef petabricks::MatrixRegionVaArgsMethods<TypeSpec > Base;

  ///
  /// Constructor with a given layout
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , const IndexT multipliers[D])
    : Base(s, b, NULL, NULL)
  {
    (void)sizes;
    (void)multipliers;
  }

  ///
  /// Constructor with a stock layout
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , typename Base::StockLayouts = Base::LAYOUT_ASCENDING)
    : Base(s, b, sizes, NULL) 
  {}
  
  ///
  /// Copy constructor
  MatrixRegion( const MutableMatrixRegion& that )
    : Base(that.storage(), that.base(), NULL, NULL)
  {}

  ///
  /// Default constructor
  MatrixRegion() : Base(NULL, NULL, NULL, NULL) {}
  
  ///
  /// Implicit conversion from ElementT
  MatrixRegion( ElementT& value ) : Base(NULL, &value, NULL, NULL) {}

  ///
  /// Allow implicit conversion to ElementT
  operator ElementT& () const { return this->cell(); }


  bool isSize() const {
    return this->base()!=0;
  }
  
};

/**
 * Specialized storage for ConstMatrixRegion0D, just store the value directly
 */
template<>
class MatrixRegionMembers < MatrixRegionTypeSpec<0, const MATRIX_ELEMENT_T> > {
public:
  enum { D = 0 };
  typedef MatrixRegionTypeSpec<0, const MATRIX_ELEMENT_T> TypeSpec;
  typedef TypeSpec::ElementT ElementT;
  typedef TypeSpec::IndexT   IndexT;
  typedef TypeSpec::StorageT StorageT;
  
  MatrixRegionMembers(const StorageT&, ElementT* b, const IndexT* , const IndexT*)
    : _val(b!=NULL ? *b : -666)
  {
    _storageInfo = new MatrixStorageInfo();
    exportTo(_storageInfo);
  }
  
  const ElementT* base() const { return &_val; }
  const IndexT* sizes() const { return NULL; }
  const IndexT* multipliers() const { return NULL; };
  const StorageT& storage() const { static StorageT dummy; return dummy; }
  const MatrixStorageInfoPtr storageInfo() const { return _storageInfo; }

  void randomize(){ _val = MatrixStorage::rand(); }
  
  ///
  /// export to a more generic container (used in memoization)
  void exportTo(MatrixStorageInfo& ms) const{
    ms.setStorage(0, 0);
    ms.setSizeMultipliers(0, NULL, NULL);
    ms.setExtraVal(_val);
  }
  
  ///
  /// import from a more generic container (used in memoization)
  void importFrom(MatrixStorageInfo& ms){
    copyFrom(ms);
  }
  
  ///
  /// import from a more generic container (used in memoization)
  void copyFrom(MatrixStorageInfo& ms){
    _val = ms.extraVal();
  }
protected: 
  IndexT* sizes() { return NULL; }
  IndexT* multipliers() { return NULL; };
private:
  MATRIX_ELEMENT_T _val;
  MatrixStorageInfoPtr _storageInfo;
};


/**
 * Specialization to eliminate the use of va_args, not needed for correctness
 */
template< typename ElementT >
class MatrixRegionVaArgsMethods<MatrixRegionTypeSpec<0, ElementT> > : public MatrixRegionBasicMethods< MatrixRegionTypeSpec<0, ElementT> > {
public:
  typedef typename petabricks::MatrixRegionTypeSpec<0, ElementT> TypeSpec;
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::IndexT              IndexT;
  typedef typename TypeSpec::MatrixRegion        MatrixRegion;
  typedef MatrixRegionBasicMethods<TypeSpec> Base;
  MatrixRegionVaArgsMethods( const typename TypeSpec::StorageT& s
                          , typename TypeSpec::ElementT* b
                          , const typename TypeSpec::IndexT* sizes
                          , const typename TypeSpec::IndexT* multipliers)
    : Base(s,b,sizes,multipliers)
  {}
  
  //these passthroughs must be declared here for overloading to work
  INLINE ElementT& cell(IndexT c[D]) const{ return this->Base::cell(c); }
  INLINE bool contains(IndexT c[D]) const{ return this->Base::contains(c); }
  INLINE MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{ return this->Base::region(c1,c2); }
  INLINE static MatrixRegion allocate(IndexT s[D]){ return Base::allocate(s); }
  
  INLINE static MatrixRegion allocate(){
    IndexT c1[0];
    return Base::allocate(c1);
  }
  INLINE ElementT& cell() const{
    IndexT c1[0];
    return Base::cell(c1);
  }
  INLINE bool contains() const{
    IndexT c1[0];
    return Base::contains(c1);
  }
  INLINE MatrixRegion region() const{
    IndexT c1[0];
    IndexT c2[0];
    return Base::region(c1,c2);
  }
};

/**
 * Specialization to eliminate the use of va_args, not needed for correctness
 */
template< typename ElementT >
class MatrixRegionVaArgsMethods<MatrixRegionTypeSpec<1, ElementT> > : public MatrixRegionBasicMethods< MatrixRegionTypeSpec<1, ElementT> > {
public:
  typedef typename petabricks::MatrixRegionTypeSpec<1, ElementT> TypeSpec;
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::IndexT              IndexT;
  typedef typename TypeSpec::MatrixRegion        MatrixRegion;
  typedef MatrixRegionBasicMethods<TypeSpec> Base;
  MatrixRegionVaArgsMethods( const typename TypeSpec::StorageT& s
                          , typename TypeSpec::ElementT* b
                          , const typename TypeSpec::IndexT* sizes
                          , const typename TypeSpec::IndexT* multipliers)
    : Base(s,b,sizes,multipliers)
  {}
  
  //these passthroughs must be declared here for overloading to work
  INLINE ElementT& cell(IndexT c[D]) const{ return this->Base::cell(c); }
  INLINE bool contains(IndexT c[D]) const{ return this->Base::contains(c); }
  INLINE MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{ return this->Base::region(c1,c2); }
  INLINE static MatrixRegion allocate(IndexT s[D]){ return Base::allocate(s); }
  
  INLINE static MatrixRegion allocate(IndexT x){
    IndexT c1[] = {x};
    return Base::allocate(c1);
  }
  INLINE ElementT& cell(IndexT x) const{
    IndexT c1[] = {x};
    return Base::cell(c1);
  }
  INLINE bool contains(IndexT x) const{
    IndexT c1[] = {x};
    return Base::contains(c1);
  }
  INLINE MatrixRegion region(IndexT x1, IndexT x2) const{
    IndexT c1[] = {x1};
    IndexT c2[] = {x2};
    return Base::region(c1,c2);
  }
};

/**
 * Specialization to eliminate the use of va_args, not needed for correctness
 */
template< typename ElementT >
class MatrixRegionVaArgsMethods<MatrixRegionTypeSpec<2, ElementT> > : public MatrixRegionBasicMethods< MatrixRegionTypeSpec<2, ElementT> > {
public:
  typedef typename petabricks::MatrixRegionTypeSpec<2, ElementT> TypeSpec;
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::IndexT              IndexT;
  typedef typename TypeSpec::MatrixRegion        MatrixRegion;
  typedef MatrixRegionBasicMethods<TypeSpec> Base;
  MatrixRegionVaArgsMethods( const typename TypeSpec::StorageT& s
                          , typename TypeSpec::ElementT* b
                          , const typename TypeSpec::IndexT* sizes
                          , const typename TypeSpec::IndexT* multipliers)
    : Base(s,b,sizes,multipliers)
  {}
  
  //these passthroughs must be declared here for overloading to work
  INLINE ElementT& cell(IndexT c[D]) const{ return this->Base::cell(c); }
  INLINE bool contains(IndexT c[D]) const{ return this->Base::contains(c); }
  INLINE MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{ return this->Base::region(c1,c2); }
  INLINE static MatrixRegion allocate(IndexT s[D]){ return Base::allocate(s); }
  
  INLINE static MatrixRegion allocate(IndexT x, IndexT y){
    IndexT c1[] = {x, y};
    return Base::allocate(c1);
  }
  INLINE ElementT& cell(IndexT x, IndexT y) const{
    IndexT c1[] = {x, y};
    return Base::cell(c1);
  }
  INLINE bool contains(IndexT x, IndexT y) const{
    IndexT c1[] = {x, y};
    return Base::contains(c1);
  }
  INLINE MatrixRegion region(IndexT x1, IndexT y1, IndexT x2, IndexT y2) const{
    IndexT c1[] = {x1, y1};
    IndexT c2[] = {x2, y2};
    return Base::region(c1,c2);
  }
};

/**
 * Specialization to eliminate the use of va_args, not needed for correctness
 */
template< typename ElementT >
class MatrixRegionVaArgsMethods<MatrixRegionTypeSpec<3, ElementT> > : public MatrixRegionBasicMethods< MatrixRegionTypeSpec<3, ElementT> > {
public:
  typedef typename petabricks::MatrixRegionTypeSpec<3, ElementT> TypeSpec;
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::IndexT              IndexT;
  typedef typename TypeSpec::MatrixRegion        MatrixRegion;
  typedef MatrixRegionBasicMethods<TypeSpec> Base;
  MatrixRegionVaArgsMethods( const typename TypeSpec::StorageT& s
                          , typename TypeSpec::ElementT* b
                          , const typename TypeSpec::IndexT* sizes
                          , const typename TypeSpec::IndexT* multipliers)
    : Base(s,b,sizes,multipliers)
  {}
  
  //these passthroughs must be declared here for overloading to work
  INLINE ElementT& cell(IndexT c[D]) const{ return this->Base::cell(c); }
  INLINE bool contains(IndexT c[D]) const{ return this->Base::contains(c); }
  INLINE MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{ return this->Base::region(c1,c2); }
  INLINE static MatrixRegion allocate(IndexT s[D]){ return Base::allocate(s); }
  
  INLINE static MatrixRegion allocate(IndexT x, IndexT y, IndexT z){
    IndexT c1[] = {x, y, z};
    return Base::allocate(c1);
  }
  INLINE ElementT& cell(IndexT x, IndexT y, IndexT z) const{
    IndexT c1[] = {x, y, z};
    return Base::cell(c1);
  }
  INLINE bool contains(IndexT x, IndexT y, IndexT z) const{
    IndexT c1[] = {x, y, z};
    return Base::contains(c1);
  }
  INLINE MatrixRegion region(IndexT x1, IndexT y1, IndexT z1, IndexT x2, IndexT y2, IndexT z2) const{
    IndexT c1[] = {x1, y1, z1};
    IndexT c2[] = {x2, y2, z2};
    return Base::region(c1,c2);
  }
};

/**
 * Specialization to eliminate the use of va_args, not needed for correctness
 */
template< typename ElementT >
class MatrixRegionVaArgsMethods<MatrixRegionTypeSpec<4, ElementT> > : public MatrixRegionBasicMethods< MatrixRegionTypeSpec<4, ElementT> > {
public:
  typedef typename petabricks::MatrixRegionTypeSpec<4, ElementT> TypeSpec;
  enum { D = TypeSpec::D };
  typedef typename TypeSpec::IndexT              IndexT;
  typedef typename TypeSpec::MatrixRegion        MatrixRegion;
  typedef MatrixRegionBasicMethods<TypeSpec> Base;
  MatrixRegionVaArgsMethods( const typename TypeSpec::StorageT& s
                          , typename TypeSpec::ElementT* b
                          , const typename TypeSpec::IndexT* sizes
                          , const typename TypeSpec::IndexT* multipliers)
    : Base(s,b,sizes,multipliers)
  {}
  
  //these passthroughs must be declared here for overloading to work
  INLINE ElementT& cell(IndexT c[D]) const{ return this->Base::cell(c); }
  INLINE bool contains(IndexT c[D]) const{ return this->Base::contains(c); }
  INLINE MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{ return this->Base::region(c1,c2); }
  INLINE static MatrixRegion allocate(IndexT s[D]){ return Base::allocate(s); }
  
  INLINE static MatrixRegion allocate(IndexT x, IndexT y, IndexT z, IndexT a){
    IndexT c1[] = {x, y, z, a};
    return Base::allocate(c1);
  }
  INLINE ElementT& cell(IndexT x, IndexT y, IndexT z, IndexT a) const{
    IndexT c1[] = {x, y, z, a};
    return Base::cell(c1);
  }
  INLINE bool contains(IndexT x, IndexT y, IndexT z, IndexT a) const{
    IndexT c1[] = {x, y, z, a};
    return Base::contains(c1);
  }
  INLINE MatrixRegion region(IndexT x1, IndexT y1, IndexT z1, IndexT a1, IndexT x2, IndexT y2, IndexT z2, IndexT a2) const{
    IndexT c1[] = {x1, y1, z1, a1};
    IndexT c2[] = {x2, y2, z2, a2};
    return Base::region(c1,c2);
  }
};

}

#endif

