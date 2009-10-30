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
#ifndef PETABRICKSRIRSCOPE_H
#define PETABRICKSRIRSCOPE_H

#include "common/jprintable.h"
#include "common/jrefcounted.h"

#include <map>

namespace petabricks {

class RIRSymbol;
typedef jalib::JRef<RIRSymbol> RIRSymbolPtr;
typedef std::map<std::string, RIRSymbolPtr> RIRSymbolMap;
class RIRScope;
typedef jalib::JRef<RIRScope> RIRScopePtr;

class RIRSymbol: public jalib::JRefCounted, public jalib::JPrintable {
public:
  enum SymbolType {
    INVALID,
    SYM_TYPE               = 0x1000,
    SYM_TYPE_BASIC,
    SYM_TYPE_MATRIX,
    SYM_TRANSFORM          = 0x2000,
    SYM_TRANSFORM_TEMPLATE,
    SYM_CONFIG             = 0x4000,
    SYM_CONFIG_TRANSFORM_LOCAL, 
    SYM_CONFIG_PASSED, 
    SYM_LOCAL_VAR          = 0x8000
  };
  RIRSymbol(SymbolType t, const std::string& rp = "") 
    : _type(t), _replacement(rp)
  {}
  SymbolType type() const { return _type; }

  void print(std::ostream& o) const {
    o<<"RIRSymbol";
  }

  bool isType() const { return (_type & SYM_TYPE) != 0; }
  bool isConfig() const { return (_type & SYM_CONFIG) != 0; }
  bool isTransform() const { return (_type & SYM_TRANSFORM) != 0; }

  bool hasReplacement() const { return _replacement.length()>0; } 
  const std::string& replacement() const { return _replacement; }
private:
  SymbolType  _type;
  std::string  _replacement;
};

class RIRScope: public jalib::JRefCounted {
public:
  static const RIRScopePtr& global();

  RIRScope(const RIRScopePtr& parent) 
    : _parent(parent) 
  {}
  
  void set(const std::string& name, RIRSymbol::SymbolType val){
    set(name, new RIRSymbol(val));
  }

  void set(const std::string& name, const RIRSymbolPtr& val){
    _symbols[name] = val;
  }
  
  RIRSymbolPtr localLookup(const std::string& name) const;

  RIRSymbolPtr lookup(const std::string& name) const;

  RIRScopePtr createChildLayer() { return new RIRScope(this); }
  const RIRScopePtr& parentLayer() const { return _parent; }
private:
  RIRScopePtr _parent;
  RIRSymbolMap _symbols;
};

}

#endif
