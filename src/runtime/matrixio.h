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
#ifndef PETABRICKSMATRIXIO_H
#define PETABRICKSMATRIXIO_H

#include "matrixregion.h"

#include "common/jassert.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#else
# define MATRIX_ELEMENT_T double
# define MAX_DIMENSIONS   64
#endif

namespace petabricks {

/**
 * Struct for holding outputs of matrixreader.lpp, used for matrixread<->MatrixIO com
 */
struct MatrixReaderScratch {
  ///
  /// Storage allocated to matrix being read
  MatrixStoragePtr storage;
  ///
  /// Pointer to first unread cell of this->storage
  ElementT* buf;
  ///
  /// Remaining space in this->buf, should be zero on completion
  int remaining;
  ///
  /// Number of dimensions
  int dimensions;
  ///
  /// Size of each dimension
  int sizes[MAX_DIMENSIONS];
};


/**
 * A thin wrapper around matrixreader
 */
class MatrixIO{
public:
  ///
  /// Constructor
  MatrixIO(FILE* file = stdin);

  ///
  /// Constructor (opens the given filename)
  MatrixIO(const char* filename, const char* mode);

  ///
  /// Read a D-dimensional matrix from _fd
  template<int D>
  MatrixRegion<D, MATRIX_ELEMENT_T> read(){
    JASSERT(_fd != 0);
    MatrixReaderScratch o;
    _read(o);
    JASSERT(o.dimensions==D)(o.dimensions)(D)
      .Text("Unexpected number of dimensions in input matrix");
    MatrixStorage::IndexT sizes[D];
    for(int i=0; i<D; ++i) sizes[i]=o.sizes[i];
    return MatrixRegion<D, MATRIX_ELEMENT_T>(o.storage, o.storage->data(), sizes);
  }

  ///
  /// Write a given matrix to _fd
  template<int D, typename T>
  void write(MatrixRegion<D,T> m);

  MatrixRegion0D read0D(){ return read<0>(); }
  MatrixRegion1D read1D(){ return read<1>(); }
  MatrixRegion2D read2D(){ return read<2>(); }
  MatrixRegion3D read3D(){ return read<3>(); }
  MatrixRegion4D read4D(){ return read<4>(); }
  MatrixRegion5D read5D(){ return read<5>(); }
  MatrixRegion6D read6D(){ return read<6>(); }
  MatrixRegion7D read7D(){ return read<7>(); }
  MatrixRegion8D read8D(){ return read<8>(); }
  MatrixRegion9D read9D(){ return read<9>(); }
protected:
  void _read(MatrixReaderScratch&);

private:
  FILE* _fd;
};

}

///
/// Write a given matrix to _fd
template<int D, typename T>
inline void petabricks::MatrixIO::write(MatrixRegion<D,T> m){
  if(_fd==0)     return;
  if(_fd==stdin) _fd=stdout;
  fprintf(_fd,"SIZE");
  for(int i=0; i<D; ++i)
    fprintf(_fd," %d",m.size(i));
  fprintf(_fd,"\n");
  MatrixStorage::IndexT coord[D];
  memset(coord, 0, sizeof coord);
  if(D>0){
    for(;;){
      fprintf(_fd,"%4.8g ", (double) m.cell(coord));
      int z=m.incCoord(coord);
      if(z<0) break;
      while(z-->0)
        fprintf(_fd,"\n");
    }
    fprintf(_fd,"\n");
  }else{ //0D case
    fprintf(_fd,"%4.8g", (double) m.cell(coord));
  }
  fprintf(_fd,"\n");
  fflush(_fd);
}

#endif
