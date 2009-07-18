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
#include "rule.h"
#include "jasm.h"
#include "maximawrapper.h"

jalib::AtomicT theNextRuleId = 0;

petabricks::RuleInterface::RuleInterface()
  : _id(jalib::atomicIncrementReturn(&theNextRuleId))
{}

bool petabricks::RulePriCmp::operator()(const RulePtr& r1, const RulePtr& r2) const 
{
  int p1 = r1->priority()*2 - r1->hasWhereClause();
  int p2 = r2->priority()*2 - r2->hasWhereClause();
  if(p1==p2) return r1<r2;
  return p1<p2;
}


bool petabricks::RuleDescriptor::operator< (const RuleDescriptor& that) const{
  std::string strA = this->_formula->toString();
  std::string strB = that._formula->toString(); 
  const char* op = "<";
  if(_type==RULE_END && that._type==RULE_BEGIN) op = "<=";
  
  //optimization and zero handling
  if(strA==strB) return strcmp(op,"<=")==0;
  if(strA=="0")  return true;
  if(strB=="0")  return false;

  return MaximaWrapper::instance().compare(this->_formula, op , that._formula);
}

bool petabricks::RuleDescriptor::isSamePosition(const FormulaPtr& that) const{
   return MAXIMA.tryCompare(this->_formula, "=" , that) == MaximaWrapper::YES;
}

petabricks::FormulaPtr petabricks::RuleInterface::trimImpossible(const FormulaList& l){
  JASSERT(l.size()==1)(l).Text("trimming formulas not yet implemented");
  return l.front();
}


