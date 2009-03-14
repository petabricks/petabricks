/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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
#include "rirscope.h"

const petabricks::RIRScopePtr& petabricks::RIRScope::global(){
  static RIRScopePtr inst = new RIRScope(NULL);
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

