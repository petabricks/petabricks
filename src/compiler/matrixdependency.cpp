/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#include "matrixdependency.h"


petabricks::DependencyDirection::DependencyDirection(size_t dimensions, DirectionT dir) : _directionMask(dimensions, dir) {}

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
  static const char* maskToStr[] = 
    {"NONE", "<", "=", "<=", ">", "<>", ">=", "*"};
  o << '(';
  for(size_t i=0; i< _directionMask.size(); ++i){
    if(i!=0) o << ", ";
    int t = _directionMask[i];
    for(;t>=D_MULTIOUTPUT; t-=D_MULTIOUTPUT)
      o << "MULTIOUTPUT";
    o << maskToStr[t];
  }
  o << ')';
}

std::string petabricks::DependencyDirection::toCodeStr() const{
  std::string str;
  static const char* maskToStr[] = 
    {"D_NONE", "D_LT", "D_EQ", "D_LE", "D_GT", "D_NEQ", "D_GE", "D_ALL",
     "D_MULTIOUTPUT"};
  for(size_t i=0; i< _directionMask.size(); ++i){
    JASSERT(_directionMask[i] < (int)(sizeof(maskToStr)/sizeof(char*)));
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


