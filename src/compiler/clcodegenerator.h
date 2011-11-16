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
#ifndef PETABRICKSCLCODEGENERATOR_H
#define PETABRICKSCLCODEGENERATOR_H

#include "codegenerator.h"
#include "transform.h"

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

  CLCodeGenerator(const TrainingDepsPtr& cg) : CodeGenerator(new StreamTree("OpenCL tmp buf"), cg) {}
  
  /** Writes a properly-escaped C/C++ string literal to the specified output
   stream. */
  void outputEscapedStringTo( std::ostream& o );

  /** Returns a string holding a properly-escaped C/C++ string literal
   corresponding to the code generator's output. */
  std::string outputEscapedString( );

  void localMemoryBarrier( );

  void beginKernel(RegionList& _to, RegionList& _from, unsigned int dims, Transform& trans, bool local);

  void endKernel( );

  void outputStringTo( std::ostream& o );

};

}

#endif
