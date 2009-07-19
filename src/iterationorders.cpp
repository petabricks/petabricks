/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
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
#include "iterationorders.h"

#include "codegenerator.h"
#include "maximawrapper.h"
#include "rule.h"

petabricks::IterationDefinition::IterationDefinition(RuleInterface& rule, bool isSingleCall)
  : _order(rule.dimensions())
{
  rule.removeInvalidOrders(_order);
  for(int i=0; i<_order.size(); ++i){
    _var.push_back(rule.getOffsetVar(i));
    _begin.push_back(rule.getOffsetVar(i,"begin"));
    _end.push_back(rule.getOffsetVar(i,  "end"));
    _step.push_back(rule.getSizeOfRuleIn(i));
  }
  if(isSingleCall){
    _step.clear();
  }
}

void petabricks::IterationDefinition::genLoopBegin(CodeGenerator& o){
  if(isSingleCall()){
    o.write("{");
    for(size_t i=0; i<_var.size(); ++i){
      o.varDecl("const IndexT "+_var[i]->toString()+" = "+_begin[i]->toString());
    }
  }else{
    for(size_t i=0; i<_var.size(); ++i){
      FormulaPtr b=_begin[i];
      FormulaPtr e=_end[i];
      FormulaPtr s=_step[i];
      FormulaPtr v=_var[i];
      if(_order[i]==IterationOrder::BACKWARD)
        o.beginReverseFor(v->toString(), b, e, s);
      else
        o.beginFor(v->toString(), b, e, s);
    }
  }
}

void petabricks::IterationDefinition::genLoopEnd(CodeGenerator& o){
  if(isSingleCall()){
    o.write("}");
  }else{
    for(size_t i=0; i<_var.size(); ++i){
      o.endFor();
    }
  }
}

std::vector<std::string> petabricks::IterationDefinition::args() const {
  std::vector<std::string> rv;
  CoordinateFormula::const_iterator i;
  for(i=_begin.begin(); i!=_begin.end(); ++i) 
    rv.push_back("const IndexT "+(*i)->toString());
  for(i=_end.begin(); i!=_end.end(); ++i) 
    rv.push_back("const IndexT "+(*i)->toString());
  return rv;
}

std::vector<std::string> petabricks::IterationDefinition::argnames() const {
  std::vector<std::string> rv;
  CoordinateFormula::const_iterator i;
  for(i=_begin.begin(); i!=_begin.end(); ++i) 
    rv.push_back((*i)->toString());
  for(i=_end.begin(); i!=_end.end(); ++i) 
    rv.push_back((*i)->toString());
  return rv;
}

