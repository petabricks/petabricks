/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#ifndef PETABRICKSUSERRULE_H
#define PETABRICKSUSERRULE_H

#include "codegenerator.h"
#include "matrixdependency.h"
#include "rule.h"
#include "ruleir.h"

#include "common/jconvert.h"

#include <vector>

namespace petabricks {

/**
 * Represent a transform rule defined by the user
 */
class UserRule : public RuleInterface{
public:
  ///
  /// Constructor -- return style rule
  UserRule(const RegionPtr& to, const RegionList& from, const FormulaList& where);

  ///
  /// Constructor -- to style rule
  UserRule(const RegionList& to, const RegionList& from, const FormulaList& where);
  
  ///
  /// Initialize this rule after parsing
  void initialize(Transform&);
  
  ///
  /// Set this->_body
  void setBody(const char*);

  ///
  /// Set priority flag
  void setPriority(RuleFlags::PriorityT v)  { _flags.priority = v; }
  
  ///
  /// Set rotation flag
  void addRotations(RuleFlags::RotationT v) { _flags.rotations |= v; }

  ///
  /// Print this rule to a given stl stream
  /// implements JPrintable::print
  void print(std::ostream& o) const;
  
  ///
  /// Add RuleDescriptors to output corresponding to the extrema of the applicable region in dimension
  void getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension);

  ///
  /// Generate seqential code to declare this rule
  void generateDeclCodeSimple(Transform& trans, CodeGenerator& o);


  ///
  /// Generate seqential code to declare this rule
  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic);
  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o){
    generateTrampCodeSimple(trans, o, true);
    generateTrampCodeSimple(trans, o, false);
  }
  void generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic);


  ///
  /// Generate seqential code to invoke this rule
  void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region); 
  void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);

  ///
  /// Return function the name of this rule in the code
  std::string implcodename(Transform& trans) const;
  std::string trampcodename(Transform& trans) const;

  bool isReturnStyle() const { return _flags.isReturnStyle; }

  int dimensions() const;

  void addAssumptions() const;

  void collectDependencies(StaticScheduler& scheduler);

  void markRecursive() { 
    markRecursive(new FormulaVariable(TRANSFORM_N_STR));
  }
  void markRecursive(const FormulaPtr& rh) { 
    if(!_flags.isRecursive){
      _flags.isRecursive = true; 
      _recursiveHint = rh;
    }
  }

  bool isRecursive() const { return _flags.isRecursive; }

  RuleFlags::PriorityT priority() const { return _flags.priority; }
  const FormulaList& conditions() const { return _conditions; }

  void removeInvalidOrders(IterationOrderList& o);

  bool canProvide(const MatrixDefPtr& m) const {
    return _provides.find(m) != _provides.end();
  }

  std::vector<std::string> getCallArgs(Transform& trans, const SimpleRegionPtr& region);

  const FormulaPtr& recursiveHint() const { return _recursiveHint; }


  FormulaPtr getSizeOfRuleIn(int d){
    for(size_t i=0; i<_to.size(); ++i){
      if(d < (int)_to[i]->dimensions()){
        return _to[i]->getSizeOfRuleIn(d);
      }
    }
    JASSERT(false)(d)(_id);
    return 0;
  }

  bool isSingleElement() const {
    if(_to.size()!=1) return false;
    return _to[0]->isSingleElement();
  }

  void compileRuleBody(Transform& tx, RIRScope& s);

  bool isSingleCall() const {
    for(size_t i=0; i<_to.size(); ++i)
      if(!_to[i]->isAll())
        return false;
    return true;
  }

  bool hasWhereClause() const { return _conditions.size()>0; }

  FormulaPtr getWhereClause() const { return getWhereClause(0); }
  FormulaPtr getWhereClause(int start) const {
    if(_conditions.size()==0) return NULL;
    if((int)_conditions.size()-1==start) return _conditions[start];
    return new FormulaAnd(_conditions[start], getWhereClause(start+1));
  }
  
private:
  RuleFlags   _flags;
  RegionList  _from;
  RegionList  _to;
  FormulaList _conditions;
  FormulaList _definitions;
  std::string     _bodysrc;
  RIRBlockCopyRef _bodyirStatic;
  RIRBlockCopyRef _bodyirDynamic;
  MatrixDependencyMap _depends;
  MatrixDependencyMap _provides;
  FormulaPtr          _recursiveHint;
};

}

#endif
