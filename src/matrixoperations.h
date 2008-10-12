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

#ifndef HECURAMATRIXOPERATIONS_H
#define HECURAMATRIXOPERATIONS_H

#include "matrix.h"

namespace hecura {
/*
template<typename AMatrix, typename BMatrix>
void matrixCopy(AMatrix& dest,  const BMatrix& src){
  for( typename AMatrix::Iterator i = dest.Begin(); i!=dest.End(); ++i){
    *i = src.get(i.x(), i.y());
  }
}

template<typename AMatrix>
void matrixFill(AMatrix& dest,  Matrix::ElementT value){
  for( typename AMatrix::Iterator i = dest.Begin(); i!=dest.End(); ++i){
    *i = value;
  }
}

template<typename AMatrix>
void matrixFill(AMatrix& dest,  Matrix::ElementT (*generator)(int x, int y)){
  for( typename AMatrix::Iterator i = dest.Begin(); i!=dest.End(); ++i){
    *i = (*generator)(i.x(), i.y());
  }
}

template<typename AMatrix, typename BMatrix>
bool matrixEquals(const AMatrix& left,  const BMatrix& right){
  for( typename AMatrix::ConstIterator i = left.Begin(); i!=left.End(); ++i){
    if(*i != right.get(i.x(), i.y()))
      return false;
  }
  return true;
}
*/

}

#endif
