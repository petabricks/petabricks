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
#ifndef PETABRICKSMATRIXDEPENDENCY_H
#define PETABRICKSMATRIXDEPENDENCY_H

#include "matrixdef.h"
#include "region.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"

#include <map>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

namespace petabricks {

class MatrixDependency;
typedef jalib::JRef<MatrixDependency> MatrixDependencyPtr;
class MatrixDependencyMap : public jalib::JRefCounted, public jalib::JPrintable, public std::map<MatrixDefPtr, MatrixDependencyPtr> {
  void print(std::ostream& o) const;
};

class DependencyDirection :  public jalib::JPrintable {
public:
  enum DirectionT {
    D_NONE = 0,
    D_LT=1, D_EQ=2, D_GT=4,
    D_MULTIOUTPUT=8,
    D_LE = D_LT | D_EQ,
    D_GE = D_GT | D_EQ,
    D_NEQ = D_LT | D_GT,
    D_ALL = D_LT | D_EQ | D_GT
  };
  ///
  /// Constructor
  DependencyDirection(size_t dimensions = 0, DirectionT dir=D_NONE);

  ///
  /// Merge two DependencyDirections
  DependencyDirection(const DependencyDirection& left, const DependencyDirection& right);

  ///
  /// Add a dependency in a given direction on a given dimension
  void addDirection(size_t dim, DirectionT dir);

  void addDirection(const DependencyDirection& that){
    *this=DependencyDirection(*this,that);
  }

  ///
  /// Remove a dependency on the given dimension
  void removeDimension(const size_t dimension);
  
  void print(std::ostream& o) const;

  size_t size() const;

  DirectionT operator[](size_t dim) const ;

  
  bool operator!= ( const DependencyDirection& that ) const {
     return _directionMask!=that._directionMask;
  }

  bool isMultioutput() const {
    for(size_t i=0; i<_directionMask.size(); ++i)
      if( (_directionMask[i]&D_MULTIOUTPUT) != 0 )
        return true;
    return false;
  }


  void removeMultioutput() {
    for(size_t i=0; i<_directionMask.size(); ++i) {
      _directionMask[i] = (_directionMask[i] & (~D_MULTIOUTPUT) );
    }
  }


  bool isNone() const {
    for(size_t i=0; i<_directionMask.size(); ++i)
      if( _directionMask[i] != 0 ) {
        return false;
			}
    return true;
  }

  std::string toCodeStr() const;

  bool canIterateForward(int d) const { return (operator[](d) & D_GT)==0; }
  bool canIterateBackward(int d) const { return (operator[](d) & D_LT)==0; }
private:
  std::vector<int> _directionMask;
};

class MatrixDependency : public jalib::JRefCounted, public jalib::JPrintable {
public:
  MatrixDependency( const DependencyDirection& d
                  , const SimpleRegionPtr&     r = NULL);

  void print(std::ostream& o) const;

  void mergeWith( MatrixDependency& that );

  const SimpleRegionPtr& region() const { return _region; }

  const DependencyDirection& direction() const { return _direction; }
  
  void removeDimension(const size_t dimension);
  
private:
  DependencyDirection _direction;
  SimpleRegionPtr     _region;
};

}

#endif
