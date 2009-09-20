/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
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
#ifndef PETABRICKSMATRIXSTORAGE_H
#define PETABRICKSMATRIXSTORAGE_H

#include "common/jassert.h"
#include "common/jrefcounted.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#else
# define MATRIX_INDEX_T   int 
# define MATRIX_ELEMENT_T double
#endif

namespace petabricks {

class MatrixStorage;
typedef jalib::JRef<MatrixStorage> MatrixStoragePtr;

/**
 * The raw data for a Matrix
 */
class MatrixStorage : public jalib::JRefCounted {
public:
  typedef MATRIX_INDEX_T IndexT;
  typedef MATRIX_ELEMENT_T ElementT;
private:
  //no copy constructor
  MatrixStorage(const MatrixStorage&);
public:
  ///
  /// Constructor
  MatrixStorage(IndexT n) : _count(n) {
    _data = new ElementT[n];
  }

  ///
  /// Destructor
  ~MatrixStorage(){
    delete [] _data;
  }

  ElementT* data() { return _data; }
  const ElementT* data() const { return _data; }

  IndexT count() const { return _count; }

  ///
  /// Fill the matrix with random data
  void randomize();


  ///
  /// generate a single random number
  static double rand();
private:
  ElementT* _data;
  IndexT _count;
};

}

#endif

