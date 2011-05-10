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
#include "matrixio.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

void matrixreaderset_in (FILE *  in_str );
int matrixreaderlex(petabricks::MatrixReaderScratch&);

petabricks::MatrixIO::MatrixIO(FILE* file) : _fd(file) {}
petabricks::MatrixIO::MatrixIO(const char* filename, const char* mode) 
{
  JTRACE("MatrixIO")(filename)(mode);
  if(std::string("-")==filename) 
    _fd = stdin;
  else if(std::string(DEVNULL)==filename){
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

