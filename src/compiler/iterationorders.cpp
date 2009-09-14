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

#include "common/jconvert.h"

petabricks::IterationDefinition::IterationDefinition(RuleInterface& rule, const DependencyDirection& order, bool isSingleCall)
  : _order(order)
{
  for(size_t i=0; i<_order.size(); ++i){
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
      //TODO: expand to reorder dimensions
      if(_order.canIterateForward(i) || !_order.canIterateBackward(i)){
        JWARNING(_order.canIterateForward(i))(_order).Text("couldn't find valid iteration order, assuming forward");
        o.beginFor(v->toString(), b, e, s);
      } else {
        o.beginReverseFor(v->toString(), b, e, s);
      }
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

std::vector<std::string> petabricks::IterationDefinition::packedargs() const {
  std::string t[] = {
    "IndexT _iter_begin["+jalib::XToString(dimensions())+"]",
    "IndexT _iter_end["+jalib::XToString(dimensions())+"]"
  };
  return std::vector<std::string>(t, t+2);
}
std::vector<std::string> petabricks::IterationDefinition::packedargnames() const {
  std::string t[] = {
    "_iter_begin",
    "_iter_end"
  };
  return std::vector<std::string>(t, t+2);
}
void petabricks::IterationDefinition::unpackargs(CodeGenerator& o) const {
  for(size_t i=0; i<_var.size(); ++i){
    o.write("const IndexT "+_begin[i]->toString()+" = _iter_begin["+jalib::XToString(i)+"];");
    o.write("const IndexT "+_end[i]->toString()+" = _iter_end["+jalib::XToString(i)+"];");
  }
}

