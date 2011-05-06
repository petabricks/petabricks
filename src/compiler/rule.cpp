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
#include "rule.h"

#include "maximawrapper.h"

#include "common/jasm.h"


void petabricks::RuleFlags::print(std::ostream& os) const {
  if(priority != PRIORITY_DEFAULT){
    os << "priority(" << priority << ") ";
  }
  if(rotations != NOROTATE){
    os << "rotations(";
    if((ROTATE_90  & rotations)!=0) os << " 90";
    if((ROTATE_180 & rotations)!=0) os << " 180";
    if((ROTATE_270 & rotations)!=0) os << " 270";
    if((MIRROR_X   & rotations)!=0) os << " mirrorx";
    if((MIRROR_Y   & rotations)!=0) os << " mirrory";
    os << " ) ";
  }
}

static jalib::AtomicT theNextRuleId = 0;
petabricks::RuleInterface::RuleInterface()
  : _id(jalib::atomicIncrementReturn(&theNextRuleId))
  , _isDisabled(false)
{}

bool petabricks::RulePriCmp::operator()(const RulePtr& r1, const RulePtr& r2) const 
{
  int p1 = r1->priority()*2 - r1->hasWhereClause();
  int p2 = r2->priority()*2 - r2->hasWhereClause();
  if(p1==p2) return r1->id()<r2->id();
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


