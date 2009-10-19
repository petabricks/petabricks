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
#if !defined(PETABRICKSCODEGENERATOR_H) && defined(HAVE_OPENCL)
#define PETABRICKSCODEGENERATOR_H

#include "formula.h"
#include "trainingdeps.h"

#include "common/jconvert.h"
#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/jtunable.h"

#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <sstream>
#include <string>
#include <vector>

namespace petabricks {

class CodeGenerator;
typedef jalib::JRef<CLCodeGenerator> CLCodeGeneratorPtr;

class CLCodeGenerator : public CodeGenerator
{
  /** Writes a properly-escaped C/C++ string literal to the specified output
   stream. */
  void outputEscapedStringTo( std::ostream& o );

  /** Returns a string holding a properly-escaped C/C++ string literal
   corresponding to the code generator's output. */
  std::string outputEscapedString( );
};

}

#endif
