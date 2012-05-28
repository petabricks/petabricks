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
#ifndef PETABRICKSPBC_H
#define PETABRICKSPBC_H

#include "common/jassert.h"
#include "common/openclutil.h"

#include <string>
#include <iostream>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


namespace pbcConfig {
extern std::string thePbPreprocessor;
extern std::string theObjDir;
}

namespace petabricks
{

#define STRINGIFY(x) STRINGIFY_INNER(x)
#define STRINGIFY_INNER(x) #x

enum OpenCLMode
{
	E_OPENCL_DISABLED,
	E_OPENCL_ENABLED,
};

extern OpenCLMode theOpenCLMode;

class RuleFlavor {
public:
  enum RuleFlavorEnum
  {
    SEQUENTIAL,
    WORKSTEALING,
    DISTRIBUTED,
    OPENCL,
    _COUNT,
    SEQUENTIAL_OPENCL,
    WORKSTEALING_OPENCL,
    DISTRIBUTED_OPENCL,
    INVALID,
  };

  typedef unsigned int iterator;
  static iterator begin() { return 0; }
  static iterator end() { return _COUNT; }

  RuleFlavor(RuleFlavorEnum v = INVALID) : _val(v) {}
  RuleFlavor(iterator v) : _val(static_cast<RuleFlavorEnum>(v)) {}
  operator RuleFlavorEnum() const { return _val; }

  const char* str() const {
    switch(*this) {
      case RuleFlavor::SEQUENTIAL:   return "sequential";
      case RuleFlavor::WORKSTEALING: return "workstealing";
      case RuleFlavor::OPENCL:
      case RuleFlavor::SEQUENTIAL_OPENCL:
      case RuleFlavor::WORKSTEALING_OPENCL:
      case RuleFlavor::DISTRIBUTED_OPENCL:       return "opencl";
      case RuleFlavor::DISTRIBUTED:  return "distributed";
      default:
        UNIMPLEMENTED();
        return "";
    }
  }
  
  std::string string() const { return str(); }

  friend std::ostream& operator<<(std::ostream& o, const RuleFlavor& fv) {
    return o<<fv.str();
  }

private:
  RuleFlavorEnum _val;
};

}

#endif

