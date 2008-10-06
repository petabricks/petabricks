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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>

#include "jthread.h"
#include "jrefcounted.h"
#include "jfilesystem.h"
#include "symboliccoordinate.h"
#include "matrix.h"
#include "matrixoperations.h"
#include "maximawrapper.h"
#include "transform.h"
#include "codegenerator.h"

using namespace hecura;

int main( int argc, const char ** argv){
  TransformListPtr t = parsePbFile(argv[1]);
  t->front()->initialize();
  t->front()->print(std::cout);
  CodeGenerator cg(std::cout);
  t->front()->generateCodeSimple(cg);
  return 0; 
}
