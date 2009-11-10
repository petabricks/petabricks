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
#ifndef PETABRICKSSYNTHETICRULE_H
#define PETABRICKSSYNTHETICRULE_H

#include "rule.h"

namespace petabricks {

/**
 * Provides reasonable no-op defaults for RuleInterface (base class for synthetic rules)
 */
class SyntheticRule : public RuleInterface {
public:
  void initialize(Transform&);
  void compileRuleBody(Transform& tx, RIRScope& s);
  void performExpansion(Transform&){}

  RuleFlags::PriorityT priority() const;
  bool isRecursive() const;
  bool hasWhereClause() const;
  FormulaPtr getWhereClause() const;

  std::string getLabel() const;

  bool canProvide(const MatrixDefPtr& m) const;

  void getApplicableRegionDescriptors(RuleDescriptorList& output,
                                      const MatrixDefPtr& matrix, int dimension, const RulePtr&);

  void generateCallCodeSimple(Transform& trans, CodeGenerator& o,
                              const SimpleRegionPtr& region);
  void generateCallTaskCode(const std::string& name, Transform& trans,
                            CodeGenerator& o, const SimpleRegionPtr& region);
  void generateDeclCodeSimple(Transform& trans, CodeGenerator& o);
  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o);

  void markRecursive();
  const FormulaPtr& recursiveHint() const;

  void print(std::ostream& os) const;

  int dimensions() const { UNIMPLEMENTED(); return 0; }
  FormulaPtr getSizeOfRuleIn(int) { UNIMPLEMENTED(); return 0; }
  void generateTrampCellCodeSimple(Transform&, CodeGenerator&, RuleFlavor) {
    UNIMPLEMENTED();
  }

};


/**
 * Base class for synthetic rules that wrap other rules
 * provides default implementations that forward to _rule
 */
class WrapperSyntheticRule : public SyntheticRule {
public:
  WrapperSyntheticRule(const RulePtr& rule) 
    : _rule(rule)
  {
    _applicableRegion = _rule->applicableRegion();
  }
  
  //these just forward to _rule
  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o);
  void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  bool isSingleElement() const;
  int dimensions() const;
  FormulaPtr getSizeOfRuleIn(int d);
  void collectDependencies(StaticScheduler& scheduler);
  DependencyDirection getSelfDependency() const;
  petabricks::RuleFlags::PriorityT priority() const;
  bool isRecursive() const;
  bool hasWhereClause() const;
  petabricks::FormulaPtr getWhereClause() const;
  bool canProvide(const MatrixDefPtr& md) const;
  void getApplicableRegionDescriptors(RuleDescriptorList& rdl, const MatrixDefPtr& md, int i, const RulePtr&);
  const petabricks::FormulaPtr& recursiveHint() const;
protected:
  RulePtr _rule;
};


///
///combines multiple rules with where clauses
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


///
/// duplicate a rule that has a duplicate keyword
class DuplicateExpansionRule : public WrapperSyntheticRule {
public:
  DuplicateExpansionRule(const RulePtr& rule, size_t dup) 
    : WrapperSyntheticRule(rule), _dup(dup)
  {
    JASSERT(_rule && dup<_rule->duplicateCount());
  }

  ///
  /// calls setDuplicateNumber() then forwards the call to _rule
  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o);

  ///
  /// calls setDuplicateNumber() then forwards the call to _rule
  void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);

  ///
  /// calls setDuplicateNumber() then forwards the call to _rule
  void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
private:
  size_t _dup;
};


}

#endif
