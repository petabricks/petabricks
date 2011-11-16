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
    SYM_TYPE               = 0x0100,
    SYM_TYPE_BASIC,             
    SYM_TYPE_MATRIX        = 0x0110,
    SYM_TYPE_MATRIX_GENERIC,            
    SYM_TYPE_MATRIX_SPECIFIC,            
    SYM_TRANSFORM          = 0x0200,
    SYM_TRANSFORM_TEMPLATE,     
    SYM_TRANSFORM_VARACCURACY,  
    SYM_CONFIG             = 0x0400,
    SYM_CONFIG_TRANSFORM_LOCAL,  
    SYM_CONFIG_PASSED,          
    SYM_LOCAL_VAR          = 0x0800,
    SYM_ARG                = 0x1000,
    SYM_ARG_ELEMENT,
    SYM_ARG_REGION,
    _LAST
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
  bool isTemplateTransform() const { return _type == SYM_TRANSFORM_TEMPLATE || _type==SYM_TRANSFORM_VARACCURACY; }

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
