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
#include "transform.h"
#include "rule.h"
#include "staticscheduler.h"

const hecura::FormulaPtr& hecura::RuleChoice::autotuned() {
  static FormulaPtr t = new FormulaVariable("AUTOTUNED");
  return t;
}

hecura::RuleChoice::RuleChoice( const RuleSet& rule
                              , const FormulaPtr& c/*=FormulaPtr()*/
                              , const RuleChoicePtr& n/*=RuleChoicePtr()*/)
  : _rules(rule), _condition(c), _next(n)
{}

void hecura::RuleChoice::print(std::ostream& o) const {
  o << "RuleChoice"; //TODO
}

void hecura::RuleChoice::generateCodeSimple(  const std::string& taskname
                                            , Transform& trans
                                            , ScheduleNode& node
                                            , const SimpleRegionPtr& region
                                            , CodeGenerator& o){
  if(_condition){
    o.beginIf(processCondition(_condition, trans, o)->toString());
  }

  std::string choicename = trans.name() + "_alg_lvl" + jalib::XToString(level())
                        + "_rules";
  int n=0;
  if(_rules.size()>1){
    for(RuleSet::const_iterator i=_rules.begin(); i!=_rules.end(); ++i){
      choicename += "_"+jalib::XToString((*i)->id());
    }
    o.createTunable(trans.name(), choicename, 0, 0, _rules.size()-1);
    o.beginSwitch(choicename);
  }
  for(RuleSet::const_iterator i=_rules.begin(); i!=_rules.end(); ++i){
    if(_rules.size()>1) o.beginCase(n++);
    if(taskname.empty())
      (*i)->generateCallCodeSimple(trans, o, region);
    else{
      (*i)->generateCallTaskCode(taskname, trans, o, region);
      node.printDepsAndEnqueue(o, *_rules.begin());
    }
    if(_rules.size()>1) o.endCase();
  }
  if(_rules.size()>1){
    o.write("default: JASSERT(false);");
    o.endSwitch();
  }

  if(_condition){
    if(_next){
      o.elseIf();
      _next->generateCodeSimple(taskname, trans, node, region, o);
    }
    o.endIf();
  }
}

hecura::FormulaPtr hecura::RuleChoice::processCondition(const FormulaPtr& f, Transform& trans, CodeGenerator& o)
{
  if(f->getFreeVariables()->contains(autotuned()->toString())){
    std::string name=trans.getTunerName("recursive_cutoff_");
    o.createTunable(trans.name(), name, 1000);
    return f->replace(autotuned(), new FormulaVariable(name));
  }else{
    return f;
  }
}
