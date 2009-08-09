/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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
#include "matrixdependency.h"


petabricks::DependencyDirection::DependencyDirection(size_t dimensions) : _directionMask(dimensions, D_NONE) {}

///
/// Merge two DependencyDirections
petabricks::DependencyDirection::DependencyDirection(const DependencyDirection& left, const DependencyDirection& right){
//   JASSERT(left.size()==right.size())(left.size())(right.size());
  size_t d = std::max(left.size(), right.size());
  _directionMask.reserve(d);
  for(size_t i=0; i<d; ++i){
    int dir = D_NONE;
    if(left.size()>i)  dir |= left[i];
    if(right.size()>i) dir |= right[i];
    _directionMask.push_back(dir);
  }
}

///
/// Add a dependency in a given direction on a given dimension
void petabricks::DependencyDirection::addDirection(size_t dim, DirectionT dir){
  JASSERT(dim<_directionMask.size())(dim)(_directionMask.size());
  _directionMask[dim] |= dir;
}

void petabricks::DependencyDirection::print(std::ostream& o) const {
  static const char* maskToStr[DirectionT_count] = 
    {"NONE", "<", "=", "<=", ">", "<>", ">=", "*"};
  o << '(';
  for(size_t i=0; i< _directionMask.size(); ++i){
    JASSERT(_directionMask[i] < DirectionT_count);
    if(i!=0) o << ", ";
    o << maskToStr[_directionMask[i]];
  }
  o << ')';
}

std::string petabricks::DependencyDirection::toCodeStr() const{
  std::string str;
  static const char* maskToStr[DirectionT_count] = 
    {"D_NONE", "D_LT", "D_EQ", "D_LE", "D_GT", "D_NEQ", "D_GE", "D_ALL"};
  for(size_t i=0; i< _directionMask.size(); ++i){
    JASSERT(_directionMask[i] < DirectionT_count);
    if(i!=0) str += ", ";
    str += "DependencyDirection::";
    str += maskToStr[_directionMask[i]];
  }
  return str;
}

size_t petabricks::DependencyDirection::size() const { return _directionMask.size(); }

petabricks::DependencyDirection::DirectionT petabricks::DependencyDirection::operator[](size_t dim) const { 
  #ifdef DEBUG
    JASSERT(dim < size());
  #endif
  return (DirectionT)_directionMask[dim];
}


petabricks::MatrixDependency::MatrixDependency( const DependencyDirection& d
                , const SimpleRegionPtr&     r/* = NULL*/)
  : _direction(d), _region(r)
{}

void petabricks::MatrixDependency::print(std::ostream& o) const {
  o << _direction << " @ region(" << _region << ")";
}

void petabricks::MatrixDependency::mergeWith( MatrixDependency& that ){
  _direction = DependencyDirection(_direction, that._direction);
  if(_region && that._region)
    _region = _region->regionUnion(that._region);
  else if(that._region)
    _region = that._region;
}


