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
#ifndef PETABRICKSPBC_H
#define PETABRICKSPBC_H


#include "common/jassert.h"

#include <string>
#include <iostream>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_OPENCL
# include "openclutil.h"
#endif

namespace pbcConfig {
extern std::string thePbPreprocessor;
extern std::string theObjDir;
}

namespace petabricks
{

#define STRINGIFY(x) STRINGIFY_INNER(x)
#define STRINGIFY_INNER(x) #x

#if defined(HAVE_OPENCL)

enum OpenCLMode
{
	E_OPENCL_DISABLED,
	E_OPENCL_ENABLED,
};

extern OpenCLMode theOpenCLMode;

#endif

/*
enum CodeGenerationMode
  {
    E_CGM_STATIC,
    E_CGM_DYNAMIC,
  #if defined(HAVE_OPENCL)
    E_CGM_OPENCL,
  #endif
  };

}
*/

enum RuleFlavorEnum
{
  //dynamic first for backward compatibility with isStatic (if we missed any more places)
  E_RF_DYNAMIC,
  E_RF_STATIC,
#if defined(HAVE_OPENCL)
  E_RF_OPENCL,
#endif
};

class RuleFlavor {
public:
  RuleFlavor(RuleFlavorEnum v) : _val(v) {}
  operator RuleFlavorEnum() const { return _val; }


  const char* str() const {
    switch(*this) {
      case E_RF_DYNAMIC: return "workstealing";
      case E_RF_STATIC:  return "sequential";
#if defined(HAVE_OPENCL)
      case E_RF_OPENCL:  return "opencl";
#endif
      default:
        UNIMPLEMENTED();
        return "";
    }
  }
  friend std::ostream& operator<<(std::ostream& o, const RuleFlavor& fv) {
    return o<<fv.str();
  }
private:
  RuleFlavorEnum _val;
};

}

#endif

