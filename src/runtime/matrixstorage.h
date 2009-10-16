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

