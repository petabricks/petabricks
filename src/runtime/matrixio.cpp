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
#include "matrixio.h"

void matrixreaderset_in (FILE *  in_str );
int matrixreaderlex(petabricks::MatrixReaderScratch&);

petabricks::MatrixIO::MatrixIO(FILE* file) : _fd(file) {}
petabricks::MatrixIO::MatrixIO(const char* filename, const char* mode) 
{
  JTRACE("MatrixIO")(filename)(mode);
  if(std::string("-")==filename) 
    _fd = stdin;
  else if(std::string("/dev/null")==filename){
    _fd = 0;
    return;
  }else 
    _fd = fopen(filename,mode);
  JASSERT(_fd!=NULL)(filename)(mode).Text("failed to open file");
}

void petabricks::MatrixIO::_read(MatrixReaderScratch& o){
  if(_fd == stdout) _fd = stdin;
  matrixreaderset_in(_fd);
  matrixreaderlex(o);
  matrixreaderset_in(NULL);
  JASSERT(o.dimensions>=0 && o.dimensions < MAX_DIMENSIONS)
      (o.dimensions).Text("failed to read input matrix, invalid size");
  JASSERT(o.storage).Text("failed to read input matrix");
  JASSERT(o.remaining==0)(o.remaining).Text("failed to read input matrix");
}

// void petabricks::MatrixIO::write(const MATRIX_ELEMENT_T* buf, int h, int w){
//   if(_fd == stdin) _fd = stdout;
//   int i=0;
//   for(int x=0; x<w; ++x){
//     for(int y=0; y<h; ++y){
//       fprintf(_fd, "%4.1f ", buf[i++]);
//     }
//     fprintf(_fd, "\n");
//   }
// }

