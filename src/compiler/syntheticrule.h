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
#ifndef PETABRICKSSYNTHETICRULE_H
#define PETABRICKSSYNTHETICRULE_H

#include "rule.h"

namespace petabricks {

class SyntheticRule : public RuleInterface {
public:
  void initialize(Transform&);
  void compileRuleBody(Transform& tx, RIRScope& s);

  RuleFlags::PriorityT priority() const;
  bool isRecursive() const;
  bool hasWhereClause() const;
  FormulaPtr getWhereClause() const;

  bool canProvide(const MatrixDefPtr& m) const;

  void getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension);
  
  void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  void generateDeclCodeSimple(Transform& trans, CodeGenerator& o);
  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o);
  
  void markRecursive();
  const FormulaPtr& recursiveHint() const;


  void print(std::ostream& os) const;


  int dimensions() const { UNIMPLEMENTED(); return 0; }
  FormulaPtr getSizeOfRuleIn(int d) { UNIMPLEMENTED(); return 0; }
  void generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic) { UNIMPLEMENTED(); }

};



class WhereExpansionRule : public SyntheticRule {
public:

  WhereExpansionRule(const RuleSet& rules) 
    : _rules(rules) 
  {}

  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o);

  void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  
  bool isSingleElement() const;
  
  int dimensions() const;
  FormulaPtr getSizeOfRuleIn(int d);

  std::string codename() const;

  void collectDependencies(StaticScheduler& scheduler);

  void genWhereSwitch(Transform& trans, CodeGenerator& o);

  DependencyDirection getSelfDependency() const;
private:
  RuleSet _rules;
};


}

#endif

