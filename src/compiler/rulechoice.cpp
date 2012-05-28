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
#include "rulechoice.h"

#include "codegenerator.h"
#include "maximawrapper.h"
#include "rule.h"
#include "scheduler.h"
#include "syntheticrule.h"
#include "transform.h"

#include <algorithm>

size_t petabricks::RuleChoiceCollection::size() const {
  std::vector<const RuleChoiceConsumer*>::const_iterator i;
  size_t c = 1;
  for(i=_ordering.begin(); i!=_ordering.end(); ++i) {
    if(_combinedChoices.find(*i) == _combinedChoices.end()) {
      c *= std::max<size_t>(1, (*i)->choices().size());
    }
  }
  return c;
}

petabricks::RuleChoiceAssignment petabricks::RuleChoiceCollection::getAssignment(size_t choice) const {
  std::vector<const RuleChoiceConsumer*>::const_iterator i;
  RuleChoiceAssignment a;
  for(i=_ordering.begin(); i!=_ordering.end(); ++i) {
    const RuleSet& rs = (*i)->choices();
    a[*i] = RulePtr::null();
    std::map<const RuleChoiceConsumer*, const RuleChoiceConsumer*>::const_iterator comb = _combinedChoices.find(*i);
    if(comb == _combinedChoices.end()) {
      ssize_t c = choice % std::max<size_t>(1, rs.size());
      choice /= std::max<size_t>(1, rs.size());;
      for(RuleSet::const_iterator r=rs.begin(); r!=rs.end(); ++r){
        if(c--==0) {
          a[*i] = *r;
          break;
        }
      }
    } else {
      const RuleSet& rsb = comb->second->choices();
      JASSERT(rs.size()==rsb.size());

      //if they were the same RuleSet we could do:
      //a[*i] = a[comb->second];

      RuleSet::const_iterator r;
      RuleSet::const_iterator rb;
      for(r=rs.begin(), rb=rsb.begin(); r!=rs.end() && rb!=rsb.end(); ++r, ++rb){
        if(*rb == a[comb->second]){
          //found the corresponding rule in this choice site
          a[*i] = *r;
          break;
        }
      }
    }
    JASSERT(rs.empty() || a[*i] != RulePtr::null());
  }
  return a;
}

void petabricks::RuleChoiceCollection::pruneChoiceSpace() {
  std::vector<const RuleChoiceConsumer*>::const_iterator i;

  //combine choice sites with the same RuleSet
  std::map<RuleSet, const RuleChoiceConsumer*> rulesets;
  for(i=_ordering.begin(); i!=_ordering.end(); ++i) {
    const RuleSet& rs = (*i)->choices();
    if(rs.size()<=1) continue;

    if(rulesets.find(rs) == rulesets.end()) {
      rulesets[rs] = *i;
    }else{
      _combinedChoices[*i] = rulesets[rs];
    }
  }
  rulesets.clear();

  /*
  std::map<size_t, const RuleChoiceConsumer*> corners;
  for(i=_ordering.begin(); i!=_ordering.end(); ++i) {
    const RuleSet& rs = (*i)->choices();
    if(rs.size()<=1) continue;
    if(_combinedChoices.find(*i) != _combinedChoices.end()) continue;
    if( MAXIMA.comparePessimistically((*i)->region()->symbolicSize(), "<=", FormulaInteger::one()) ) {
      if(corners.find(rs.size()) == corners.end()) {
        corners[rs.size()] = *i;
      }else{
        _combinedChoices[*i] = corners[rs.size()];
       // JTRACE("corner block combines")(rs.size());
      }
    }
    }*/

  JTRACE("combined")(size());
}

void petabricks::RuleChoiceCollection::generateDecisionTree(std::string& pfx, size_t choiceCount, CodeGenerator& o) {
  o.cg().addAlgchoice(pfx.substr(0, pfx.length()-1), (int)choiceCount);
  for(int lvl = 1; lvl<=MAX_REC_LEVELS; ++lvl) {
    std::string rule   = pfx + "lvl" + jalib::XToString(lvl) + "_rule";
    o.createTunable(true, "algchoice.alg", rule, 0, 0, (int)choiceCount);

    if(lvl<MAX_REC_LEVELS) {
      std::string cutoff = pfx + "lvl" + jalib::XToString(lvl+1) + "_cutoff";
      o.createTunable(true, "algchoice.cutoff", cutoff, jalib::maxval<int>(), 1);
      if(lvl==1)
        o.beginIf("LIKELY(_transform_n < "+cutoff+")");
      else
        o.beginIf("_transform_n < "+cutoff);
    }

    o.write("return "+rule+";");

    if(lvl<MAX_REC_LEVELS) {
      o.endIf();
    }
  }
}


