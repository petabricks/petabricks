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
#include "rirscope.h"

#include "common/jconvert.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

static const petabricks::RIRScopePtr _makeTypeScope(){
  using namespace petabricks;

  RIRScopePtr t = new RIRScope(NULL);
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
    t->set("MatrixRegion"+jalib::XToString(i)+"D", RIRSymbol::SYM_TYPE_MATRIX);
    t->set("ConstMatrixRegion"+jalib::XToString(i)+"D", RIRSymbol::SYM_TYPE_MATRIX);
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

