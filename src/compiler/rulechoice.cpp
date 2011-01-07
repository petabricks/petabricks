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
#include "syntheticrule.h"
#include "transform.h"

#include <algorithm>

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
                                            , const std::string& tpfx
                                            , int levelOffset)
{
  if(levelOffset==0){
    levelOffset=level()+1;
    std::string t = tpfx;
    t.resize(t.length()-1);
    if(_rules.size()>1){
      o.cg().addAlgchoice(t, _rules.size());
    }
  }


  std::string choicename = tpfx + "lvl" + jalib::XToString(levelOffset-level()) + "_rule";


  if(_condition){
    o.beginIf(processCondition(tpfx + "lvl" + jalib::XToString(levelOffset+1-level()) + "_cutoff",_condition, choicename, o));
  }

  std::vector<RulePtr> sortedRules(_rules.begin(), _rules.end());
  std::sort(sortedRules.begin(), sortedRules.end(), RuleIdComparer());

  int n=0;
  if(sortedRules.size()>1){
//     for(std::vector<RulePtr>::const_iterator i=sortedRules.begin(); i!=sortedRules.end(); ++i){
//       choicename += "_"+jalib::XToString((*i)->id() - trans.ruleIdOffset());
//     }
    // Create a tunables configuration for the rule choice.  Initialize it to
    // the first rule.
    o.createTunable(true, "algchoice.alg", choicename, 0, 0,
                    (int)sortedRules.size()-1);
    o.cg().emitRules(choicename, sortedRules);
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
      _next->generateCodeSimple(isStatic, taskname, trans, node, region, o, tpfx, levelOffset);
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
    o.createTunable(true, "algchoice.cutoff", name, jalib::maxval<int>(), 1);
    std::vector<RulePtr> sortedRules(_rules.begin(), _rules.end());
    std::sort(sortedRules.begin(), sortedRules.end(), RuleIdComparer());
    for(size_t i=0; i<sortedRules.size(); ++i){
      if(!s.empty()) s += " || ";
      FormulaPtr curhint = sortedRules[i]->recursiveHint();
      if(!curhint){
        static FormulaPtr t = new FormulaVariable(TRANSFORM_N_STR"()");
        curhint = t;
      }
      if(!hint) {
        hint = curhint;
      } else if(hint->toString() != curhint->toString()) {
        needComplex=true;
      }
      FormulaPtr f=new FormulaLE(hint, new FormulaVariable(name));
      s += "(" + algchoicename + "==" + jalib::XToString(i) + " && " + f->toString() + ")";
    }
    if(needComplex)
      return s;
    FormulaPtr f=new FormulaLE(hint, new FormulaVariable(name));
    return f->toCppString();
  }else{
    return f->toCppString();
  }
}

petabricks::RuleChoicePtr petabricks::RuleChoice::makeRuleChoice( const RuleSet& choices
                                                             , const MatrixDefPtr& m
                                                             , const SimpleRegionPtr& r)
{
  /*
   * This function must build a stack of RuleChoicePtr's from choices.
   * The field RuleChoice::_next forms a linked list of choices.
   * At runtime the first choice for which RuleChoice::_condition holds is used.
   */


  //split choices into recursive and base
  RuleSet recursive,base;
  for(RuleSet::const_iterator i=choices.begin(); i!=choices.end(); ++i){
    //assume all recursive fornow:
   // if((*i)->isRecursive())
      recursive.insert(*i);
   // else
   //   base.insert(*i);
  }
  JASSERT(base.size()+recursive.size()>0)(m)(r).Text("No choics exist for region");

  //default to base case
  RuleChoicePtr rv;

  if(!base.empty()){
    rv=new RuleChoice(base); //the first rule
  }

  int levels = MAX_REC_LEVELS;
  if(recursive.size()==0) levels = 0;
  if(recursive.size()==1) levels = 1;
  while(levels-->0){
    FormulaPtr condition;
    if(rv) condition = RuleChoice::autotuned();
    //add recursive case
    rv=new RuleChoice(recursive, condition, rv);
  }
  return rv;
}


petabricks::RuleChoicePtr petabricks::RuleChoice::makeCoscheduledRuleChoice( const RuleSet& choices
                                                             , const MatrixDefList& matrices
                                                             , const SimpleRegionPtr& r)
{
  RuleSet::const_iterator i;
  MatrixDefList::const_iterator m;
  RuleSet complete;
  RuleSet partial;

  for(i=choices.begin(); i!=choices.end(); ++i) {
    bool hasall = true;
    for(m=matrices.begin(); m!=matrices.end(); ++m)
      hasall &= (*i)->canProvide(*m);
    if(hasall)
      complete.insert(*i);
    else
      partial.insert(*i);
  }

  while(! partial.empty() ) {
    RuleList partiala;
    RuleSet partialb;
    std::vector<bool> satisfied(false, matrices.size());
    for(i=partial.begin(); i!=partial.end(); ++i) {
      bool hasall = true;
      for(size_t n=0; n<matrices.size(); ++n) {
        if(!satisfied[n]) {
          satisfied[n] = (*i)->canProvide(matrices[n]);
          hasall &= satisfied[n];
        }
      }
      if(hasall){
        ++i;
        partiala.insert(partiala.end(), partial.begin(), i);
        partialb.insert(i, partial.end());
        break;
      }
    }
    partial.swap(partialb);
    JASSERT(partiala.size()>0).Text("coscheduling failed");
    complete.insert(new CallInSequenceRule(partiala));
  }

  return makeRuleChoice(complete, matrices[0], r);
}

