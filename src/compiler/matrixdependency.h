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
#ifndef PETABRICKSMATRIXDEPENDENCY_H
#define PETABRICKSMATRIXDEPENDENCY_H

#include "matrixdef.h"
#include "region.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"

#include <map>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace petabricks {

class MatrixDependency;
typedef jalib::JRef<MatrixDependency> MatrixDependencyPtr;
class MatrixDependencyMap : public jalib::JRefCounted, public std::map<MatrixDefPtr, MatrixDependencyPtr> {};

class DependencyDirection :  public jalib::JPrintable {
public:
  enum DirectionT {
    D_NONE = 0,
    D_LT=1, D_EQ=2, D_GT=4,
    D_LE = D_LT | D_EQ,
    D_GE = D_GT | D_EQ,
    D_NEQ = D_LT | D_GT,
    D_ALL = D_LT | D_EQ | D_GT,
    DirectionT_count = D_ALL+1
  };
  ///
  /// Constructor
  DependencyDirection(size_t dimensions = 0);

  ///
  /// Merge two DependencyDirections
  DependencyDirection(const DependencyDirection& left, const DependencyDirection& right);

  ///
  /// Add a dependency in a given direction on a given dimension
  void addDirection(size_t dim, DirectionT dir);

  void print(std::ostream& o) const;

  size_t size() const;

  DirectionT operator[](size_t dim) const ;

  void addDirection(const DependencyDirection& that){
    *this=DependencyDirection(*this,that);
  }

  bool operator!= ( const DependencyDirection& that ) const {
     return _directionMask!=that._directionMask;
  }

  bool isNone() const {
    for(size_t i=0; i<_directionMask.size(); ++i)
      if(_directionMask[i]!=D_NONE)
        return false;
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
private:
  DependencyDirection _direction;
  SimpleRegionPtr     _region;
};

}

#endif
