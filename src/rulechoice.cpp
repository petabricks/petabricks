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
#include "rulechoice.h"

#include "codegenerator.h"

hecura::RuleChoice::RuleChoice( const RulePtr& rule
                              , const FormulaPtr& c/*=FormulaPtr()*/
                              , const RuleChoicePtr& n/*=RuleChoicePtr()*/)
  : _rule(rule), _condition(c), _next(n)
{}

void hecura::RuleChoice::print(std::ostream& o) const {
  o << "RuleChoice"; //TODO
}

void hecura::RuleChoice::generateCodeSimple(const SimpleRegionPtr& region,CodeGenerator& o){
  if(_condition){
    o.beginIf(_condition->toString());
  }
  _rule->generateCallCodeSimple(o, region);

  if(_condition){
    if(_next){
      o.elseIf();
      _next->generateCodeSimple(region, o);
    }
    o.endIf();
  }
}
