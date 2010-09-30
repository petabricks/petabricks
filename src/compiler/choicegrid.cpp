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

#include "choicegrid.h"
#include "codegenerator.h"
#include "maximawrapper.h"
#include "rule.h"

#include "common/jconvert.h"

petabricks::ChoiceGridPtr petabricks::ChoiceGrid::constructFrom( 
  const RuleSet&                allowedRules,
  const RuleDescriptorListList& dimensions,
  size_t                        dimension /*= 0*/)
{
  JTRACE("Constructing choice grid")(allowedRules.size())(dimension);
  FormulaPtr    currentPos = FormulaInteger::zero();
  ChoiceGridPtr rootNode = new ChoiceGrid(dimension, currentPos);
  ChoiceGridPtr currentNode = rootNode;
  RuleSet activeRules;
  if(dimensions.empty()){
    JTRACE("Output of 0D matrix");
    currentNode->finalizeConstruction(FormulaInteger::one(), allowedRules);
    currentNode->applyRulePriorities();
    return rootNode;
  }
  const RuleDescriptorList& boundaries = dimensions[dimension];
  ChoiceGridPtr lastNode;

  for(RuleDescriptorList::const_iterator i=boundaries.begin(); i!=boundaries.end(); ++i){
    const RuleDescriptor& rd = *i;
    //whenever current pos changes:
    if(!rd.isSamePosition(currentPos)){
//       JTRACE("new pos")(dimension)(rd.getPosition());
      if(lastNode)
        lastNode->_nextElement = currentNode;
      currentPos = rd.getPosition();
      currentNode->finalizeConstruction(currentPos, activeRules);
      if(dimension+1 < dimensions.size())
        currentNode->_nextDimension = constructFrom(activeRules, dimensions, dimension+1);
      else
        currentNode->applyRulePriorities();
      //make next element
      lastNode = currentNode;
      currentNode = new ChoiceGrid(dimension, currentPos);
    }
    if(allowedRules.find(rd.rule()) != allowedRules.end()){
      if(rd.isBegin()){ //add new rule 
        activeRules.insert(rd.rule());
      }else{ //remove expired rule
        activeRules.erase(rd.rule());
      }
    }
  }
  JWARNING(activeRules.empty());
  return rootNode;
}


void petabricks::ChoiceGrid::print(std::ostream& os) const {
  jalib::ConstMap(&RuleInterface::printIdentifier, os, _applicableRules);
}

// void petabricks::ChoiceGrid::generateCodeSimple(CodeGenerator& o, const SimpleRegionPtr& prefix){
//   SimpleRegionPtr tmp = new SimpleRegion(prefix);
//   tmp->addDimension(_begin, _end);
// 
//   if(_nextDimension){
//     _nextDimension->generateCodeSimple(o, tmp);
//   }else{
//     (*_applicableRules.begin())->generateCallCodeSimple(o, tmp);
//   }
// 
//   if(_nextElement) _nextElement->generateCodeSimple(o,prefix);
// }

void petabricks::ChoiceGrid::buildIndex(ChoiceGridIndex& idx, const SimpleRegionPtr& prefix /*= 0*/){
  SimpleRegionPtr tmp = prefix ? new SimpleRegion(prefix) : new SimpleRegion();
  tmp->addDimension(_begin, _end);

  if(_nextDimension)
    _nextDimension->buildIndex(idx, tmp);
  else
    idx[tmp] = this;

  if(_nextElement) _nextElement->buildIndex(idx, prefix);
}

void petabricks::ChoiceGrid::finalizeConstruction(const FormulaPtr& end, const RuleSet& applicable){
  _end=end;
  _applicableRules = applicable;
}

void petabricks::ChoiceGrid::applyRulePriorities(){
  RuleSet rs;
  RulePtr r,l;
  for(RuleSet::const_iterator i=_applicableRules.begin(); i!=_applicableRules.end(); ++i){
    r = *i;
    if(l && r->priority()>l->priority() && !l->hasWhereClause()){
      JTRACE("applying rule priorities")(rs.size())(_applicableRules.size());
      _applicableRules.swap(rs);
      return;
    }
    rs.insert(r);
    l = r;
  }
}
  
void petabricks::ChoiceGridIndex::removeDisabledRules(){
    for(iterator i=begin(); i!=end(); ++i)
      i->second->removeDisabledRules();
  }

void petabricks::ChoiceGrid::removeDisabledRules(){
  RuleSet rs;
  for(RuleSet::const_iterator i=_applicableRules.begin(); i!=_applicableRules.end(); ++i){
    if( ! (*i)->isDisabled() )
      rs.insert(*i);
    else
      JTRACE("REMOVED DISABLED RULE")(*i);
  }
  rs.swap(_applicableRules);

  if(_nextDimension)
    _nextDimension->removeDisabledRules();
  if(_nextElement) 
    _nextElement->removeDisabledRules();
}


