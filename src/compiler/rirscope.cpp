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
#include "rirscope.h"

#include "pbc.h"

#include "common/jconvert.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

static const petabricks::RIRScopePtr _makeTypeScope(){
  using namespace petabricks;

  RIRScopePtr t = new RIRScope(NULL);
  t->set("bool", RIRSymbol::SYM_TYPE_BASIC);
  t->set("char", RIRSymbol::SYM_TYPE_BASIC);
  t->set("short", RIRSymbol::SYM_TYPE_BASIC);
  t->set("int", RIRSymbol::SYM_TYPE_BASIC);
  t->set("long", RIRSymbol::SYM_TYPE_BASIC);
  t->set("float", RIRSymbol::SYM_TYPE_BASIC);
  t->set("double", RIRSymbol::SYM_TYPE_BASIC);
  t->set("size_t", RIRSymbol::SYM_TYPE_BASIC);
  t->set("IndexT", RIRSymbol::SYM_TYPE_BASIC);
  t->set("ElementT", RIRSymbol::SYM_TYPE_BASIC);
  for(int i=0; i<=MAX_DIMENSIONS; ++i){
    t->set("MatrixRegion"+jalib::XToString(i)+"D", RIRSymbol::SYM_TYPE_MATRIX_GENERIC);
    t->set("ConstMatrixRegion"+jalib::XToString(i)+"D", RIRSymbol::SYM_TYPE_MATRIX_GENERIC);
    for(RuleFlavor::iterator rf=RuleFlavor::begin(); rf!=RuleFlavor::end(); ++rf){
      t->set(RuleFlavor(rf).string()+"::MatrixRegion"+jalib::XToString(i)+"D", RIRSymbol::SYM_TYPE_MATRIX_SPECIFIC);
      t->set(RuleFlavor(rf).string()+"::ConstMatrixRegion"+jalib::XToString(i)+"D", RIRSymbol::SYM_TYPE_MATRIX_SPECIFIC);
    }
  }
  return t;
}


const petabricks::RIRScopePtr& petabricks::RIRScope::global(){
  static RIRScopePtr inst = new RIRScope(_makeTypeScope());
  return inst;
}
  
petabricks::RIRSymbolPtr petabricks::RIRScope::localLookup(const std::string& name) const{
  RIRSymbolMap::const_iterator i = _symbols.find(name);
  if(i!=_symbols.end())
    return i->second;
  return NULL;
}

petabricks::RIRSymbolPtr petabricks::RIRScope::lookup(const std::string& name) const{
  RIRSymbolPtr t = localLookup(name);
  if(_parent && !t) return _parent->lookup(name);
  return t;
}

