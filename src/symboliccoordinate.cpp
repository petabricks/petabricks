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
#include "symboliccoordinate.h"

#include "jasm.h"
#include "jconvert.h"

hecura::SymCoord::SymCoord(){
}

hecura::SymCoord::~SymCoord(){
}

std::string hecura::SymbolCoordPart::makeDisplayName(){
  static volatile long cntr = 0;
  return "i" + jalib::XToString( jalib::atomicAdd<1>(&cntr) );
}

hecura::SymbolCoordPart::SymbolCoordPart( const char* n /* = NULL*/ ) 
  : _displayName(n!=NULL ? n : makeDisplayName()) 
{}

void hecura::SymbolCoordPart::print(std::ostream& os)    const { 
  os << _displayName; 
}
void hecura::LiteralCoordPart::print(std::ostream& os)   const { 
  os << _value;
}
void hecura::SummationCoordPart::print(std::ostream& os) const {
  //os << "(size=" << _parts.size() << " ";
  bool normal = isNormalize();
  if(!normal) os << "(";
  const char* pfx = "";
  for( TParts::const_iterator i = _parts.begin()
     ; i!=_parts.end()
     ; ++i )
  {
    os << pfx;
    pfx = " + ";

    if(i->second != 1){
      if(i->second == -1)
        os << '-';
      else 
        os << i->second;
    }

    os << *i->first;
  }
  if(!normal) os << ")";
}

bool hecura::SummationCoordPart::isNormalize() const {
  for( TParts::const_iterator i = _parts.begin()
     ; i!=_parts.end()
     ; ++i )
  {
    if(! i->first->isSimple())
      return false;
  }
  return true;
}

hecura::CoordPartPtr hecura::CoordPart::normalize() const {
  SummationCoordPart * sym;
  CoordPartPtr ret;
  ret = sym = new SummationCoordPart();
  TValue lit = 0;
  
  normalize(1, *sym, lit);
  if(lit!=0) sym->add(new LiteralCoordPart( lit ));
  return ret;
}

void hecura::LiteralCoordPart::normalize(TMultiple mult, SummationCoordPart& /*symbols*/, TValue& literals) const {
  literals += mult*_value;
}

void hecura::SymbolCoordPart::normalize(TMultiple mult, SummationCoordPart& symbols, TValue& /*literals*/) const {
  symbols.add(this, mult); 
}

void hecura::SummationCoordPart::normalize(TMultiple mult, SummationCoordPart& symbols, TValue& literals) const {
  for( TParts::const_iterator i = _parts.begin()
     ; i!=_parts.end()
     ; ++i )
  {
    i->first->normalize(mult*i->second, symbols, literals);
  }
}

bool hecura::CoordPart::isSimple() const { return true; }
bool hecura::SummationCoordPart::isSimple() const { return false; } 

std::ostream& operator<< ( std::ostream& os, const hecura::CoordPartPtr& part ){
  part->print(os);
  return os;
}

std::ostream& operator<< ( std::ostream& os, const hecura::CoordPart& part ){
  part.print(os);
  return os;
}
