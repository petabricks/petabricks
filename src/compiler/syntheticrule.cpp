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
#include "syntheticrule.h"

#include "codegenerator.h"
#include "iterationorders.h"
#include "maximawrapper.h"
#include "transform.h"

void petabricks::SyntheticRule::compileRuleBody(Transform& tx, RIRScope& s){}
void petabricks::SyntheticRule::initialize(Transform&){}

petabricks::RuleFlags::PriorityT petabricks::SyntheticRule::priority() const { 
  return RuleFlags::PRIORITY_DEFAULT; 
}
bool petabricks::SyntheticRule::isRecursive() const { 
  return true;
}
bool petabricks::SyntheticRule::hasWhereClause() const { 
  return false; 
}
petabricks::FormulaPtr petabricks::SyntheticRule::getWhereClause() const { 
  return NULL; 
}

bool petabricks::SyntheticRule::canProvide(const MatrixDefPtr& m) const { 
  UNIMPLEMENTED(); 
  return false;
}

void petabricks::SyntheticRule::getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension) { 
  UNIMPLEMENTED(); 
}

void petabricks::SyntheticRule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
}
void petabricks::SyntheticRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region) {
}
void petabricks::SyntheticRule::generateDeclCodeSimple(Transform& trans, CodeGenerator& o) {
}
void petabricks::SyntheticRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o) {
}

void petabricks::SyntheticRule::markRecursive() { 
  UNIMPLEMENTED();
}
const petabricks::FormulaPtr& petabricks::SyntheticRule::recursiveHint() const { 
  static FormulaPtr t = new FormulaVariable(TRANSFORM_N_STR);  
  return t;
}
void petabricks::SyntheticRule::print(std::ostream& os) const {
  os << "SyntheticRule " << _id << std::endl;;
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

void petabricks::WhereExpansionRule::generateCallCodeSimple( Transform& trans
                                                           , CodeGenerator& o
                                                           , const SimpleRegionPtr& region){
  o.callSpatial(codename()+TX_STATIC_POSTFIX, region);
}

void petabricks::WhereExpansionRule::generateCallTaskCode( const std::string& name
                                                         , Transform& trans
                                                         , CodeGenerator& o
                                                         , const SimpleRegionPtr& region){
  o.mkSpatialTask(name, trans.instClassName(), codename()+TX_STATIC_POSTFIX, region);
}
  

void petabricks::WhereExpansionRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o){
  //for now static only:
  IterationDefinition iterdef(*this, getSelfDependency() , false);
  std::vector<std::string> packedargs = iterdef.packedargs();
  o.beginFunc("petabricks::DynamicTaskPtr", codename()+TX_STATIC_POSTFIX, packedargs);
  iterdef.unpackargs(o);
  iterdef.genLoopBegin(o);
  genWhereSwitch(trans,o);
  iterdef.genLoopEnd(o);
  o.write("return NULL;");
  o.endFunc();
}


void petabricks::WhereExpansionRule::genWhereSwitch(Transform& trans, CodeGenerator& o){
  RuleSet::iterator i;
  for(i=_rules.begin(); i!=_rules.end(); ++i){
    for(int d=0; d<(*i)->dimensions(); ++d){
      o._define((*i)->getOffsetVar(d)->toString(), getOffsetVar(d)->toString());
    }

    FormulaPtr wc = (*i)->getWhereClause();
    if(!wc)
      o.elseIf();
    else if(i==_rules.begin())
      o.beginIf(wc->toCppString());
    else
      o.elseIf(wc->toCppString());

    (*i)->generateTrampCellCodeSimple(trans, o, true);
    
    for(int d=0; d<(*i)->dimensions(); ++d){
      o._undefine((*i)->getOffsetVar(d)->toString());
    }

    if(!wc){
      o.endIf();
      return; //we reached an unconditioned rule
    }
  }
  o.elseIf();
  o.write("JASSERT(false).Text(\"All where clauses failed, no rule to compute region\");");
  o.endIf();
}


bool petabricks::WhereExpansionRule::isSingleElement() const { 
  return false;
}

int petabricks::WhereExpansionRule::dimensions() const {
  RuleSet::const_iterator i=_rules.begin();
  int rv = (*i)->dimensions();
  for(++i ;i!=_rules.end(); ++i)
    JASSERT(rv==(*i)->dimensions())(rv)((*i)->dimensions())
      .Text("where clauses only work with common number of dimensions");;
  return rv;
}
petabricks::DependencyDirection petabricks::WhereExpansionRule::getSelfDependency() const {
  DependencyDirection rv;
  RuleSet::const_iterator i;
  for(i=_rules.begin(); i!=_rules.end(); ++i)
    rv.addDirection((*i)->getSelfDependency());
  return rv;
}
petabricks::FormulaPtr petabricks::WhereExpansionRule::getSizeOfRuleIn(int d) {
  RuleSet::const_iterator i=_rules.begin();
  FormulaPtr rv = (*i)->getSizeOfRuleIn(d);
  for(++i ;i!=_rules.end(); ++i)
    JASSERT(MAXIMA.compare(rv,"=", (*i)->getSizeOfRuleIn(d)))
      .Text("where clauses only work with common sizes in each choice");;
  return rv;
}

std::string petabricks::WhereExpansionRule::codename() const {
  return "whereExpansion"+jalib::XToString(_id); 
}

void petabricks::WhereExpansionRule::collectDependencies(StaticScheduler& scheduler) { 
  RuleSet::const_iterator i;
  for(i=_rules.begin(); i!=_rules.end(); ++i)
    (*i)->collectDependencies(scheduler);
}


