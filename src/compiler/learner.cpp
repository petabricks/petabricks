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
#include "learner.h"

#include "syntheticrule.h"

#include <algorithm>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

petabricks::RuleChoicePtr petabricks::Learner::makeRuleChoice( const RuleSet& choices
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


petabricks::RuleChoicePtr petabricks::Learner::makeCoscheduledRuleChoice( const RuleSet& choices
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

