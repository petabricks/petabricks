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



  virtual void trimDependency(DependencyDirection& ,
                              const ChoiceDepGraphNode& ,
                              const ChoiceDepGraphNode& ){}


  bool canProvide(const MatrixDefPtr& m) const;

  void getApplicableRegionDescriptors(RuleDescriptorList& output,
                                      const MatrixDefPtr& matrix, int dimension, const RulePtr&);

//void generateCallCode(const std::string& nodename,
//                      Transform& trans,
//                      CodeGenerator& o,
//                      const SimpleRegionPtr& region,
//                      RuleFlavor flavor,
//                      std::vector<RegionNodeGroup>& regionNodesGroups,
//                      int nodeID,
//                      int gpuCopyOut); 
//
  void virtual generateDeclCode(Transform& trans, CodeGenerator& o, RuleFlavor);
  void virtual generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor) = 0;

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
  void generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor);

  void generateCallCode(const std::string& nodename,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor,
                        std::vector<RegionNodeGroup>& regionNodesGroups,
                        int nodeID,
                        int gpuCopyOut);
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

  void generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor rf);

  void generateCallCode(const std::string& nodename,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor,
                        std::vector<RegionNodeGroup>& regionNodesGroups,
                        int nodeID,
                        int gpuCopyOut);

  bool isSingleElement() const;

  int dimensions() const;
  FormulaPtr getSizeOfRuleIn(int d);

  std::string codename() const;

  void collectDependencies(StaticScheduler& scheduler);

  void genWhereSwitch(Transform& trans, CodeGenerator& o, RuleFlavor rf);

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
  void generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor);

  void generateCallCode(const std::string& nodename,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor,
                        std::vector<RegionNodeGroup>& regionNodesGroups,
                        int nodeID,
                        int gpuCopyOut);
private:
  size_t _dup;
};


///
///combines multiple rules that must be called together
class CallInSequenceRule : public SyntheticRule {
public:
  CallInSequenceRule(const RuleList& rules)
    : _rules(rules)
  {}

  void generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor rf);

  void generateCallCode(const std::string& nodename,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor,
                        std::vector<RegionNodeGroup>& regionNodesGroups,
                        int nodeID,
                        int gpuCopyOut);

  bool isSingleElement() const;

  int dimensions() const;
  FormulaPtr getSizeOfRuleIn(int d);

  std::string codename() const;

  void collectDependencies(StaticScheduler& scheduler);

  void genWhereSwitch(Transform& trans, CodeGenerator& o);

  DependencyDirection getSelfDependency() const;
private:
  RuleList _rules;
};


}

#endif
