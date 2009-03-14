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
#include <map>

namespace petabricks {

class RIRSymbol;
typedef jalib::JRef<RIRSymbol> RIRSymbolPtr;
typedef std::map<std::string, RIRSymbolPtr> RIRSymbolMap;
class RIRScope;
typedef jalib::JRef<RIRScope> RIRScopePtr;

class RIRSymbol: public jalib::JRefCounted {
public:
  enum SymbolType {
    INVALID,
    SYM_TRANSFORM,
    SYM_TRANSFORM_TEMPLATE,
    SYM_TUNABLE
  };
  RIRSymbol(SymbolType t) 
    : _type(t)
  {}
private:
  SymbolType  _type;
};


class RIRScope: public jalib::JRefCounted {
public:
  RIRScope(const RIRScopePtr& parent) 
    : _parent(parent) 
  {}

  void set(const std::string& name, const RIRSymbolPtr& val){
    _symbols[name] = val;
  }
  
  RIRSymbolPtr localLookup(const std::string& name) const{
    RIRSymbolMap::const_iterator i = _symbols.find(name);
    if(i!=_symbols.end())
      return i->second;
    return NULL;
  }

  RIRSymbolPtr lookup(const std::string& name) const{
    RIRSymbolPtr t = localLookup(name);
    if(_parent && !t) return _parent->lookup(name);
    return t;
  }
private:
  RIRScopePtr _parent;
  RIRSymbolMap _symbols;
};

}

#endif
