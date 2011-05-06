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
#include "syntheticrule.h"

#include "codegenerator.h"
#include "iterationorders.h"
#include "maximawrapper.h"
#include "transform.h"

void petabricks::SyntheticRule::compileRuleBody(Transform&, RIRScope&){}
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

std::string petabricks::SyntheticRule::getLabel() const {
  return "synthetic";
}

bool petabricks::SyntheticRule::canProvide(const MatrixDefPtr&) const { 
  UNIMPLEMENTED(); 
  return false;
}

void petabricks::SyntheticRule::getApplicableRegionDescriptors(RuleDescriptorList&, const MatrixDefPtr&, int, const RulePtr&) { 
  UNIMPLEMENTED(); 
}

void petabricks::SyntheticRule::generateCallCode(const std::string&,
                                                 Transform&,
                                                 CodeGenerator&,
                                                 const SimpleRegionPtr&,
                                                 RuleFlavor){
}

void petabricks::SyntheticRule::generateDeclCodeSimple(Transform&, CodeGenerator&) {}
void petabricks::SyntheticRule::generateTrampCodeSimple(Transform&, CodeGenerator&) {}

void petabricks::SyntheticRule::markRecursive() { 
  UNIMPLEMENTED();
}
const petabricks::FormulaPtr& petabricks::SyntheticRule::recursiveHint() const { 
  return FormulaPtr::null();
}
void petabricks::SyntheticRule::print(std::ostream& os) const {
  os << "SyntheticRule " << _id << std::endl;;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//WrapperSyntheticRule:

void petabricks::WrapperSyntheticRule::generateCallCode(const std::string& name,
                                            Transform& trans,
                                            CodeGenerator& o,
                                            const SimpleRegionPtr& region,
                                            RuleFlavor flavor){
  _rule->generateCallCode(name, trans, o, region, flavor);
}

void petabricks::WrapperSyntheticRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o){
  _rule->generateTrampCodeSimple(trans, o);
}


bool petabricks::WrapperSyntheticRule::isSingleElement() const { 
  return _rule->isSingleElement();
}

int petabricks::WrapperSyntheticRule::dimensions() const {
  return _rule->dimensions();
}
petabricks::DependencyDirection petabricks::WrapperSyntheticRule::getSelfDependency() const {
  return _rule->getSelfDependency();
}
petabricks::FormulaPtr petabricks::WrapperSyntheticRule::getSizeOfRuleIn(int d) {
  return _rule->getSizeOfRuleIn(d);
}

void petabricks::WrapperSyntheticRule::collectDependencies(StaticScheduler& scheduler) { 
  _rule->collectDependencies(scheduler);
}

petabricks::RuleFlags::PriorityT petabricks::WrapperSyntheticRule::priority() const { 
  return _rule->priority();
}
bool petabricks::WrapperSyntheticRule::isRecursive() const { 
  return _rule->isRecursive();
}
bool petabricks::WrapperSyntheticRule::hasWhereClause() const { 
  return _rule->hasWhereClause();
}
petabricks::FormulaPtr petabricks::WrapperSyntheticRule::getWhereClause() const { 
  return _rule->getWhereClause();
}
bool petabricks::WrapperSyntheticRule::canProvide(const MatrixDefPtr& md) const { 
  return _rule->canProvide(md);
}
void petabricks::WrapperSyntheticRule::getApplicableRegionDescriptors(RuleDescriptorList& rdl, const MatrixDefPtr& md, int i, const RulePtr& rule) { 
  _rule->getApplicableRegionDescriptors(rdl, md, i, rule);
}
const petabricks::FormulaPtr& petabricks::WrapperSyntheticRule::recursiveHint() const { 
  return _rule->recursiveHint();
}



////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//TODO: consider new file for WhereExpansionRule

void petabricks::WhereExpansionRule::generateCallCode(const std::string& name,
                                            Transform& trans,
                                            CodeGenerator& o,
                                            const SimpleRegionPtr& region,
                                            RuleFlavor flavor){
  SRCPOSSCOPE();
  switch(flavor) {
  case E_RF_STATIC:
    o.callSpatial(codename()+TX_STATIC_POSTFIX, region);
    break;
  case E_RF_DYNAMIC:
    o.mkSpatialTask(name, trans.instClassName(), codename()+TX_STATIC_POSTFIX, region);
    break;
  default:
    UNIMPLEMENTED();
  }
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

    (*i)->generateTrampCellCodeSimple(trans, o, E_RF_STATIC);
    
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


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//TODO: consider new file for DuplicateExpansionRule

void petabricks::DuplicateExpansionRule::generateCallCode(const std::string& name,
                                            Transform& trans,
                                            CodeGenerator& o,
                                            const SimpleRegionPtr& region,
                                            RuleFlavor flavor){
  SRCPOSSCOPE();
  size_t old = _rule->setDuplicateNumber(_dup);
  WrapperSyntheticRule::generateCallCode(name, trans, o, region, flavor);
  _rule->setDuplicateNumber(old);
}

void petabricks::DuplicateExpansionRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o){
  size_t old = _rule->setDuplicateNumber(_dup);
  WrapperSyntheticRule::generateTrampCodeSimple(trans, o);
  _rule->setDuplicateNumber(old);
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//TODO: consider new file for CallInSequenceRule


void petabricks::CallInSequenceRule::generateCallCode(const std::string& name,
                                            Transform& trans,
                                            CodeGenerator& o,
                                            const SimpleRegionPtr& region,
                                            RuleFlavor flavor){
  SRCPOSSCOPE();
  if(flavor != E_RF_STATIC)
    o.write("{ DynamicTaskPtr __last;");
  RuleList::iterator i;
  for(i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->generateCallCode(name, trans, o, region, flavor);
    if(flavor != E_RF_STATIC) {
      if(i!=_rules.begin()) {
        o.write(name+"->dependsOn(__last);");
        o.write("__last->enqueue();");
      }
      o.write("__last = "+name+";");
    }
  }
  if(flavor != E_RF_STATIC)
    o.write("}");
}

void petabricks::CallInSequenceRule::generateTrampCodeSimple(Transform& /*trans*/, CodeGenerator& /*o*/) { UNIMPLEMENTED(); }
  
bool petabricks::CallInSequenceRule::isSingleElement() const { UNIMPLEMENTED(); return false; }
  
int petabricks::CallInSequenceRule::dimensions() const { UNIMPLEMENTED(); return -1; }
petabricks::FormulaPtr petabricks::CallInSequenceRule::getSizeOfRuleIn(int /*d*/) { UNIMPLEMENTED(); return 0; }

std::string petabricks::CallInSequenceRule::codename() const { UNIMPLEMENTED(); return ""; }

void petabricks::CallInSequenceRule::collectDependencies(StaticScheduler& /*scheduler*/) { UNIMPLEMENTED(); }

void petabricks::CallInSequenceRule::genWhereSwitch(Transform& /*trans*/, CodeGenerator& /*o*/) { UNIMPLEMENTED(); }

petabricks::DependencyDirection petabricks::CallInSequenceRule::getSelfDependency() const { UNIMPLEMENTED(); return 0; }


