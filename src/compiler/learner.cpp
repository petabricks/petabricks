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
#include "learner.h"

#include "performancetester.h"

#include <algorithm>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define MULTIGRID_FLAG


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
    if((*i)->isRecursive())
      recursive.insert(*i);
    else
      base.insert(*i);
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

