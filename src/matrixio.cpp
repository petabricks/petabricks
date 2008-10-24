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
#include "matrixio.h"

void matrixreaderset_in (FILE *  in_str );
int matrixreaderlex(hecura::MatrixReaderScratch&);

hecura::MatrixIO::MatrixIO(FILE* file) : _fd(file) {}
hecura::MatrixIO::MatrixIO(const char* filename, const char* mode) 
{
  if(std::string("-")==filename) 
    _fd = stdin;
  else 
    _fd = fopen(filename,mode);
  JASSERT(_fd!=NULL)(filename)(mode).Text("failed to open file");
}

void hecura::MatrixIO::_read(MatrixReaderScratch& o){
  if(_fd == stdout) _fd = stdin;
  matrixreaderset_in(_fd);
  matrixreaderlex(o);
  matrixreaderset_in(NULL);
  JASSERT(o.dimensions>=0 && o.dimensions < MAX_DIMENSIONS)
      (o.dimensions).Text("failed to read input matrix, invalid size");
  JASSERT(o.storage).Text("failed to read input matrix");
  JASSERT(o.remaining==0)(o.remaining).Text("failed to read input matrix");
}

//specialized 0D version
template<>
void hecura::MatrixIO::write<0, MATRIX_ELEMENT_T>(MatrixRegion<0, MATRIX_ELEMENT_T> m){
  if(_fd==stdin) _fd=stdout;
  fprintf(_fd,"SIZE\n%f\n",(float)m.cell());
}

//specialized 0D version
template<>
void hecura::MatrixIO::write<0, const MATRIX_ELEMENT_T>(MatrixRegion<0, const MATRIX_ELEMENT_T> m){
  if(_fd==stdin) _fd=stdout;
  fprintf(_fd,"SIZE\n%f\n",(float)m.cell());
}

// void hecura::MatrixIO::write(const MATRIX_ELEMENT_T* buf, int h, int w){
//   if(_fd == stdin) _fd = stdout;
//   int i=0;
//   for(int x=0; x<w; ++x){
//     for(int y=0; y<h; ++y){
//       fprintf(_fd, "%4.1f ", buf[i++]);
//     }
//     fprintf(_fd, "\n");
//   }
// }

