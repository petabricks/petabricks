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
#include "rulechoice.h"

#include "codegenerator.h"
#include "rule.h"
#include "staticscheduler.h"
#include "transform.h"

#include <algorithm>

namespace {
  struct RuleIdComparer {
    bool operator()(const petabricks::RulePtr& x, const petabricks::RulePtr& y){
      return x->id() < y->id();
    }
  };

}

const petabricks::FormulaPtr& petabricks::RuleChoice::autotuned() {
  static FormulaPtr t = new FormulaVariable("AUTOTUNED");
  return t;
}

petabricks::RuleChoice::RuleChoice( const RuleSet& rule
                              , const FormulaPtr& c/*=FormulaPtr()*/
                              , const RuleChoicePtr& n/*=RuleChoicePtr()*/)
  : _rules(rule), _condition(c), _next(n)
{}

void petabricks::RuleChoice::print(std::ostream& o) const {
  o << "RuleChoice"; //TODO
}

void petabricks::RuleChoice::generateCodeSimple( bool isStatic
                                            , const std::string& taskname
                                            , Transform& trans
                                            , ScheduleNode& node
                                            , const SimpleRegionPtr& region
                                            , CodeGenerator& o
                                            , const std::string& tpfx)
{
  // o.cg().addAlgchoice(tpfx.substr(0,tpfx.length()-1), isStatic, level());

  std::string choicename = tpfx + "lvl" + jalib::XToString(level()) + "_rule";

  if(_condition){
    o.beginIf(processCondition(tpfx + "lvl" + jalib::XToString(level()) + "_cutoff",_condition, choicename, o));
  }

  std::vector<RulePtr> sortedRules(_rules.begin(), _rules.end());
  std::sort(sortedRules.begin(), sortedRules.end(), RuleIdComparer());

  int n=0;
  if(sortedRules.size()>1){
//     for(std::vector<RulePtr>::const_iterator i=sortedRules.begin(); i!=sortedRules.end(); ++i){
//       choicename += "_"+jalib::XToString((*i)->id() - trans.ruleIdOffset());
//     }
    o.createTunable(true ,"algchoice.alg", choicename, 0, 0, sortedRules.size()-1);
    o.beginSwitch(choicename);
  }
  for(std::vector<RulePtr>::const_iterator i=sortedRules.begin(); i!=sortedRules.end(); ++i){
    if(sortedRules.size()>1) o.beginCase(n++);
    if(isStatic || taskname.empty()){
      (*i)->generateCallCodeSimple(trans, o, region);
    }else{
      (*i)->generateCallTaskCode(taskname, trans, o, region);
      node.printDepsAndEnqueue(o, trans,  sortedRules[0]);
    }
    if(sortedRules.size()>1) o.endCase();
  }
  if(sortedRules.size()>1){
    o.write("default: JASSERT(false)("+choicename+".value());");
    o.endSwitch();
  }

  if(_condition){
    if(_next){
      o.elseIf();
      _next->generateCodeSimple(isStatic, taskname, trans, node, region, o, tpfx);
    }
    o.endIf();
  }
}

std::string petabricks::RuleChoice::processCondition(const std::string& name, const FormulaPtr& f, const std::string& algchoicename, CodeGenerator& o)
{
//   if(f->getFreeVariables()->contains(autotuned()->toString()))
//     return f->replace(autotuned(), new FormulaVariable(name));

  if(f==autotuned()){
    std::string s;
    FormulaPtr hint;
    bool needComplex=false;
    o.createTunable(true, "algchoice.cutoff", name, std::numeric_limits<int>::max(), 1);
    std::vector<RulePtr> sortedRules(_rules.begin(), _rules.end());
    std::sort(sortedRules.begin(), sortedRules.end(), RuleIdComparer());
    for(size_t i=0; i<sortedRules.size(); ++i){
      if(!s.empty()) s += " || ";
      FormulaPtr curhint = sortedRules[i]->recursiveHint();
      if(!curhint){
        static FormulaPtr t = new FormulaVariable(TRANSFORM_N_STR);
        curhint = t;
      }
      if(!hint) {
        hint = curhint;
      } else if(hint->toString() != curhint->toString()) {
        needComplex=true;
      }
      FormulaPtr f=new FormulaGT(hint, new FormulaVariable(name));
      s += "(" + algchoicename + "==" + jalib::XToString(i) + " && " + f->toString() + ")";
    }
    if(needComplex)
      return s;
    FormulaPtr f=new FormulaGT(hint, new FormulaVariable(name));
    return f->toCppString();
  }else{
    return f->toCppString();
  }
}
