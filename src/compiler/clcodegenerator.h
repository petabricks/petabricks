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
#if !defined(PETABRICKSCLCODEGENERATOR_H) && defined(HAVE_OPENCL)
#define PETABRICKSCLCODEGENERATOR_H

#include "codegenerator.h"

#include "common/jconvert.h"
#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/jtunable.h"

#include <sstream>
#include <string>

namespace petabricks {

class CLCodeGenerator;
typedef jalib::JRef<CLCodeGenerator> CLCodeGeneratorPtr;

class CLCodeGenerator : public CodeGenerator
{
 public:

  CLCodeGenerator() : CodeGenerator(new StreamTree("OpenCL tmp buf")) {}
  
  /** Writes a properly-escaped C/C++ string literal to the specified output
   stream. */
  void outputEscapedStringTo( std::ostream& o );

  /** Returns a string holding a properly-escaped C/C++ string literal
   corresponding to the code generator's output. */
  std::string outputEscapedString( );

  void localMemoryBarrier( );

  void beginKernel( const std::vector<std::string>& outputs, const std::vector<std::string>& inputs, unsigned int dims );

  void endKernel( );

  void outputStringTo( std::ostream& o );

};

}

#endif
