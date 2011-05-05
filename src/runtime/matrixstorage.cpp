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
#include "matrixstorage.h"

#include "petabricksruntime.h"

MATRIX_ELEMENT_T petabricks::MatrixStorage::rand(){
  return petabricks::PetabricksRuntime::randDouble(-2147483648, 2147483648);
}

void petabricks::MatrixStorage::randomize(){
  for(size_t i=0;i<_count; ++i){
    _data[i] = MatrixStorage::rand();
  }
}

void petabricks::MatrixStorageInfo::setStorage(const MatrixStoragePtr& s, const ElementT* base){
  if(s){
    _storage=s;
    JASSERT(base >= s->data());
    JASSERT(base < s->data()+s->count());
    _baseOffset=base-s->data();
  }else{
    _storage=NULL;
    _baseOffset=0;
  }
  _hash=HashT();
}

void petabricks::MatrixStorageInfo::setSizeMultipliers(int dim, const IndexT* mult, const IndexT* siz){
  _dimensions=dim;
  for(int d=0; d<_dimensions; ++d)
    _multipliers[d]=mult[d];
  for(int d=0; d<_dimensions; ++d)
    _sizes[d]=siz[d];
}

void petabricks::MatrixStorageInfo::setExtraVal(ElementT v)
{
  _extraVal=v;
}

void petabricks::MatrixStorageInfo::computeDataHash() { 
  if(_storage) 
    _hash=_storage->hash();
  else
    _hash=HashT();
}

void petabricks::MatrixStorageInfo::reset(){
  setExtraVal();
  setStorage(0,0);
  _dimensions=-1;
}

void petabricks::MatrixStorageInfo::releaseStorage() { _storage=0; }

petabricks::MatrixStorageInfo::MatrixStorageInfo(){reset();}

bool petabricks::MatrixStorageInfo::isMetadataMatch(const MatrixStorageInfo& that) const{
  if(_dimensions != that._dimensions) return false;
  if(_dimensions<0)                   return false;
  if(_baseOffset != that._baseOffset) return false;
  for(int d=0; d<_dimensions; ++d)
    if(_multipliers[d]!=that._multipliers[d])
      return false;
  for(int d=0; d<_dimensions; ++d)
    if(_sizes[d]!=that._sizes[d])
      return false;
  return true;
}

bool petabricks::MatrixStorageInfo::isDataMatch(const MatrixStorageInfo& that) const{
  if(_extraVal != that._extraVal) return false;
  if(_storage && _hash==HashT()) return false;
  return _hash==that._hash;
}

