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
#include "matrix.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

void petabricks::MatrixStorage::randomize(){
#ifdef GOOD_RANDOM
  for(int i=0;i<_count; ++i){
    _data[i] = (2.0*drand48()-1.0)*4294967296.0;
  }
#else
  //this method is bad... only use during compiler development
  int x = mrand48();
  int a = mrand48();
  int b = mrand48();
  for(int i=0;i<_count; ++i){
    _data[i] = (x^=a*x+b);
  }
#endif
}


void petabricks::MatrixRegion<0, const MATRIX_ELEMENT_T>::randomize(){
  _val = drand48();
}

void petabricks::MatrixRegion<0, MATRIX_ELEMENT_T>::randomize(){
  *_val = drand48();
}

