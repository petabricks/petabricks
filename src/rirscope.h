/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef PETABRICKSRIRSCOPE_H
#define PETABRICKSRIRSCOPE_H

#include "jrefcounted.h"
#include "jprintable.h"
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
    SYM_CONFIG_TRANSFORM_LOCAL
  };
  RIRSymbol(SymbolType t) 
    : _type(t)
  {}
  SymbolType type() const { return _type; }

  void print(std::ostream& o) const {
    o<<"RIRSymbol";
  }

  bool isConfig() const { return (_type & SYM_CONFIG) != 0; }
  bool isTransform() const { return (_type & SYM_TRANSFORM) != 0; }
private:
  SymbolType  _type;
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
