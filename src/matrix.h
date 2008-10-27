/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef HECURAMATRIX_H
#define HECURAMATRIX_H

#include "jrefcounted.h"
#include "jassert.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#else
# define MATRIX_ELEMENT_T double
#endif

#include <stdio.h>
#include <stdarg.h>

namespace hecura {

class MatrixStorage;
typedef jalib::JRef<MatrixStorage> MatrixStoragePtr;
typedef int IndexT;
typedef MATRIX_ELEMENT_T ElementT;

/**
 * The raw data for a Matrix
 */
class MatrixStorage : public jalib::JRefCounted {
public:
  typedef hecura::IndexT IndexT;
  typedef hecura::ElementT ElementT;
private:
  //no copy constructor
  MatrixStorage(const MatrixStorage&);
public:
  ///
  /// Constructor
  MatrixStorage(IndexT n) {
    _data = new ElementT[n];
  }

  ///
  /// Destructor
  ~MatrixStorage(){
    delete [] _data;
  }

  ElementT* data() { return _data; }
  const ElementT* data() const { return _data; }
private:
  ElementT* _data;
};

/**
 * Access elements of a region of a matrix with a dynamically known size
 * D = Number of dimensions
 * ElementT = double, const double, int, const int, etc
 */
template< int D, typename ElementT = MATRIX_ELEMENT_T >
class MatrixRegion {
public:
  enum StockLayouts { LAYOUT_ASCENDING, LAYOUT_DECENDING };

  ///
  /// Allocate a storage for a new MatrixRegion
  static MatrixRegion allocate(const IndexT sizes[D]) {
    IndexT s=1;
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
  /// Constructor with a given layout
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , const IndexT multipliers[D])
    : _storage(s)
    , _base(b)
  {
    memcpy(_sizes, sizes, sizeof _sizes);
    memcpy(_multipliers, multipliers, sizeof _multipliers );
  }

  ///
  /// Constructor with a stock layout
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , StockLayouts layout = LAYOUT_ASCENDING)
    : _storage(s)
    , _base(b)
  {
    memcpy(_sizes, sizes, sizeof _sizes);
    IndexT mult = 1;
    if(layout == LAYOUT_ASCENDING){
      for(int i=0; i<D; ++i){
        _multipliers[i] = mult;
        mult *= sizes[i];
      }
    }else{
      for(int i=D-1; i>=0; --i){
        _multipliers[i] = mult;
        mult *= sizes[i];
      }
    }
  }

  MatrixRegion() : _base(NULL) {}

  ///
  /// Copy constructor
  MatrixRegion( const MatrixRegion<D, MATRIX_ELEMENT_T>& that )
    : _storage(that.storage())
    , _base(that.base())
  {
    memcpy(_sizes, that.sizes(), sizeof _sizes);
    memcpy(_multipliers, that.multipliers(), sizeof _multipliers );
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
  /// Access a single cell of target matrix
  ElementT& cell(const IndexT c1[D]) const{ return _base[getOffsetFor(c1)]; }

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

  ///
  /// Create a new iterator for a region of target matrix
  MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{
    IndexT newSizes[D];
    for(int i=0; i<D; ++i){
      #ifdef DEBUG
      JASSERT(c1[i]<=c2[i])(c1[i])(c2[i])
        .Text("region has negative size");
      JASSERT(c2[i]<=_sizes[i])(c2[i])(_sizes[i])
        .Text("region goes out of bounds");
      #endif
      newSizes[i]=c2[i]-c1[i];
    }
    return MatrixRegion(_storage, _base+getOffsetFor(c1), newSizes, _multipliers);
  }

  ///
  /// Return a slice through this dimension
  /// The iterator is one dimension smaller and equivilent to always 
  /// giving pos for dimension d
  MatrixRegion<D-1, ElementT> slice(int d, IndexT pos) const{
    #ifdef DEBUG
    JASSERT(d>=0 && d<D)(d)(D).Text("invalid dimension");
    JASSERT(pos>=0 && pos<_sizes[d])(pos)(_sizes[d]).Text("out of bounds access");
    #endif
    IndexT sizes[D-1];
    IndexT mult[D-1];
    for(int i=0; i<d; ++i){
        sizes[i] = _sizes[i];
        mult[i]  = _multipliers[i];
    }
    for(int i=d+1; i<D; ++i){
        sizes[i-1] = _sizes[i];
        mult[i-1]  = _multipliers[i];
    }
    IndexT coord[D];
    memset(coord, 0, sizeof coord);
    coord[d] = pos;
    return MatrixRegion<D-1, ElementT>(_storage, _base+getOffsetFor(coord), sizes, mult);
  }
  
  
  MatrixRegion<D-1, ElementT> col(IndexT x) const{ return slice(0, x); }
  MatrixRegion<D-1, ElementT> row(IndexT y) const{  return slice(1, y); }
  
  ///
  /// Return the size of a given dimension
  IndexT size(int d) const {
    #ifdef DEBUG
    JASSERT(d>=0 && d<D)(d)(D);
    #endif
    return _sizes[d];
  }

  const MatrixStoragePtr& storage() const { return _storage; }
  ElementT* base() const { return _base; }
  const IndexT* sizes() const { return _sizes; }
  const IndexT* multipliers() const { return _multipliers; };

  IndexT width() const { return size(0); }
  IndexT height() const { return size(1); }

  MatrixRegion all() const { return *this; }

  ///
  /// Number of elements in this region
  IndexT count() const {
    IndexT s=1;
    for(int i=0; i<D; ++i)
      s*=_sizes[i];
    return s;
  }

  ///
  /// Number of elements in this region
  IndexT bytes() const {
    return count()*sizeof(ElementT);
  }
protected:
  ///
  /// Compute the offset in _base for a given coordinate
  IndexT getOffsetFor(const IndexT coord[D]) const{
    IndexT rv = 0;
    for(int i=0; i<D; ++i){
      #ifdef DEBUG
      JASSERT(_storage).Text("Tried to read uninitialized matrix");
      JASSERT(0<=coord[i] && coord[i]<_sizes[i])(coord[i])(_sizes[i])
        .Text("Out of bounds access");
      #endif
      rv +=  _multipliers[i] * coord[i];
    }
    return rv;
  }
private:
  MatrixStoragePtr _storage;
  ElementT* _base;
  IndexT _sizes[D];
  IndexT _multipliers[D];
};


/**
 * Specialized 0-Dimensional MatrixRegion
 */
template< typename ElementT >
class MatrixRegion < 0, ElementT> {
public:
  enum { D=0 };
  typedef MatrixStorage::IndexT IndexT;
public:
  enum StockLayouts { LAYOUT_ASCENDING, LAYOUT_DECENDING };

  ///
  /// Copy constructor
  MatrixRegion( const MatrixRegion<D, MATRIX_ELEMENT_T>& that )
    : _storage(that.storage())
    , _base(that.base())
  {}

  ///
  /// Allocate a storage for a new MatrixRegion
  static MatrixRegion allocate(const IndexT sizes[D]) {
    MatrixStoragePtr tmp = new MatrixStorage(1);
    #ifdef DEBUG
    *tmp->data() = 0xBadF00d;
    #endif
    return MatrixRegion(tmp, tmp->data(), sizes);
  }

  ///
  /// Allocate a storage for a new MatrixRegion (va_args version)
  static MatrixRegion allocate(){
    IndexT c1[D];
    return allocate(c1);
  }

  ///
  /// Constructor
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , const IndexT multipliers[D])
    : _storage(s)
    , _base(b)
  {}

  ///
  /// Constructor
  MatrixRegion( const MatrixStoragePtr& s
              , ElementT* b
              , const IndexT sizes[D]
              , StockLayouts layout = LAYOUT_ASCENDING)
    : _storage(s)
    , _base(b)
  {}

  ///
  /// Upcast from a lone value
  MatrixRegion( ElementT value = -666 ){
    MatrixRegion<0, MATRIX_ELEMENT_T> tmp = MatrixRegion<0, MATRIX_ELEMENT_T>::allocate();
    _storage = tmp.storage();
    _base = tmp.base();
    tmp.cell()=value;
  }

  ///
  /// 0-Dimensional region only has 1 cell
  ElementT& cell() const{ return *_base; }
 
  ///
  /// 0-Dimensional region only has 1 cell
  ElementT& cell(const IndexT coord[D]) const{ return *_base; }

  ///
  /// Allow implicit conversion to ElementT
  operator ElementT& () const { return cell(); }

  ///
  /// 0-Dimensional region only has 1 sub-region
  MatrixRegion region() const{ return *this; }
  MatrixRegion region(const IndexT c1[D], const IndexT c2[D]) const{return *this; }

  ///
  /// This should never be called
  /// Included to make generic iterators compile
  IndexT size(int d) const { return 1; };


  ///
  /// Number of elements in this region
  IndexT count() const {
    return 1;
  }

  ///
  /// Number of elements in this region
  IndexT bytes() const {
    return count()*sizeof(ElementT);
  }
  
  const MatrixStoragePtr& storage() const { return _storage; }
  ElementT* base() const { return _base; }
  const IndexT* sizes() const { JWARNING(false); return 0; }
  const IndexT* multipliers() const { JWARNING(false);  return 0; };

  MatrixRegion all() const { return *this; }
private:
  MatrixStoragePtr _storage;
  ElementT* _base;
};


typedef MatrixRegion<0> MatrixRegion0D;
typedef MatrixRegion<1> MatrixRegion1D;
typedef MatrixRegion<2> MatrixRegion2D;
typedef MatrixRegion<3> MatrixRegion3D;
typedef MatrixRegion<4> MatrixRegion4D;
typedef MatrixRegion<5> MatrixRegion5D;
typedef MatrixRegion<6> MatrixRegion6D;
typedef MatrixRegion<7> MatrixRegion7D;
typedef MatrixRegion<8> MatrixRegion8D;
typedef MatrixRegion<9> MatrixRegion9D;

typedef MatrixRegion<0, const MATRIX_ELEMENT_T> ConstMatrixRegion0D;
typedef MatrixRegion<1, const MATRIX_ELEMENT_T> ConstMatrixRegion1D;
typedef MatrixRegion<2, const MATRIX_ELEMENT_T> ConstMatrixRegion2D;
typedef MatrixRegion<3, const MATRIX_ELEMENT_T> ConstMatrixRegion3D;
typedef MatrixRegion<4, const MATRIX_ELEMENT_T> ConstMatrixRegion4D;
typedef MatrixRegion<5, const MATRIX_ELEMENT_T> ConstMatrixRegion5D;
typedef MatrixRegion<6, const MATRIX_ELEMENT_T> ConstMatrixRegion6D;
typedef MatrixRegion<7, const MATRIX_ELEMENT_T> ConstMatrixRegion7D;
typedef MatrixRegion<8, const MATRIX_ELEMENT_T> ConstMatrixRegion8D;
typedef MatrixRegion<9, const MATRIX_ELEMENT_T> ConstMatrixRegion9D;

} /* namespace hecura*/

#endif
