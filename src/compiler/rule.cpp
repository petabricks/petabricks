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
#include "rule.h"

#include "maximawrapper.h"

#include "common/jasm.h"

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


