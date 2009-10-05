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
#include "matrixstorage.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

MATRIX_ELEMENT_T petabricks::MatrixStorage::rand(){
  return (2.0*drand48()-1.0)*4294967296.0;
}

void petabricks::MatrixStorage::randomize(){
#ifdef GOOD_RANDOM
  for(int i=0;i<_count; ++i){
    _data[i] = rand();
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

