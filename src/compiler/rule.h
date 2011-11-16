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
#ifndef PETABRICKSRULE_H
#define PETABRICKSRULE_H

#include "pbc.h"
#include "formula.h"
#include "matrixdef.h"
#include "region.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/srcpos.h"

#include <string>
#include <vector>

namespace petabricks {

class ChoiceDepGraphNode;
class CodeGenerator;
class DependencyDirection;
class FormulaList;
class IterationOrderList;
class MatrixDependencyMap;
class RIRScope;
class RuleDescriptor;
class RuleInterface;
class StaticScheduler;
class Transform;
class UserRule;
typedef jalib::JRef<RuleInterface> RulePtr;
class RuleList : public std::vector<RulePtr>, public jalib::JRefCounted, public jalib::SrcPosTaggable {};
typedef std::vector<RuleDescriptor>     RuleDescriptorList;
typedef std::vector<RuleDescriptorList> RuleDescriptorListList;
typedef jalib::JRef<MatrixDependencyMap> MatrixDependencyMapPtr;

struct RulePriCmp
{
  bool operator()(const RulePtr& r1, const RulePtr& r2) const;
};

class RuleSet : public std::set<RulePtr, RulePriCmp> {
  void removeDimensionFromRegions(MatrixDefPtr matrix, size_t dimension);
};

/**
 * Priority/rotation flags for a Rule
 */
class RuleFlags {
public:
  RuleFlags() : priority(PRIORITY_DEFAULT), rotations(NOROTATE), isRecursive(false), isReturnStyle(true) {}

  typedef int PriorityT;
  enum { PRIORITY_PRIMARY   = 0
       , PRIORITY_DEFAULT   = 1
       , PRIORITY_SECONDARY = 2
       , PRIORITY_MAX = 1024};
  
  typedef int RotationT;
  enum {
    NOROTATE     = 0,
    ROTATE_90    = 1,
    ROTATE_180   = 2,
    ROTATE_270   = 4,
    MIRROR_X     = 8,
    MIRROR_Y     = 16,
    ROTATE = ROTATE_90 | ROTATE_180 | ROTATE_270,
    MIRROR = MIRROR_X | MIRROR_Y
  };

  void print(std::ostream& o) const;
  
  PriorityT  priority;
  RotationT  rotations;
  bool       isRecursive;
  bool       isReturnStyle;
};

/**
 * Class for describing the data dependencies of various instances of the same
 * matrix inside a rule
 */
class DataDependencyVectorMap : public jalib::JPrintable, 
                                public std::multimap<MatrixDefPtr, 
                                                CoordinateFormula> {
  void print(std::ostream& o) const;
};

/**
 * Base class for rules, both UserRule and SyntheticRule
 */
class RuleInterface : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
public:
  RuleInterface();

  ///
  /// Initialize this rule after parsing
  virtual void initialize(Transform&) = 0;
  virtual void performExpansion(Transform&) = 0;
  virtual void compileRuleBody(Transform& tx, RIRScope& s) = 0;

  virtual RuleFlags::PriorityT priority() const = 0;
  virtual bool isRecursive() const = 0;
  virtual bool canProvide(const MatrixDefPtr& m) const = 0;
  virtual bool isSingleElement() const = 0;

  virtual void trimDependency(DependencyDirection& dep,
                              const ChoiceDepGraphNode& from,
                              const ChoiceDepGraphNode& to) = 0;

  virtual void collectDependencies(StaticScheduler& scheduler) = 0;
  virtual void getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension, const RulePtr&) = 0;
  
  virtual void generateCallCode(const std::string& nodename,
                                Transform& trans,
                                CodeGenerator& o,
                                const SimpleRegionPtr& region,
                                RuleFlavor flavor,
                                std::vector<RegionNodeGroup>& regionNodesGroups,
                                int nodeID,
                                int gpuCopyOut) = 0;
  void generateCallCode(const std::string& nodename,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor) {
    std::vector<RegionNodeGroup> empty;
    generateCallCode(nodename, trans, o, region, flavor, empty, 0, 0);
  }
  virtual void generateDeclCode(Transform& trans, CodeGenerator& o, RuleFlavor rf) = 0;
  virtual void generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor rf) = 0;
  
  virtual void markRecursive() = 0;
  virtual const FormulaPtr& recursiveHint() const = 0;
  
  virtual bool hasWhereClause() const = 0;
  virtual FormulaPtr getWhereClause() const = 0;

  virtual std::string getLabel() const = 0;

  ///
  /// Remove out-of-bounds solutions from the given formula list 
  FormulaPtr trimImpossible(const FormulaList& l);

  ///
  /// Get the offset variable for the "center" of this rule.
  /// The offset variable is used to represent the location where the rule 
  /// is applied.  All coordinates are rewritten to be relative to the
  /// offset vars.
  FormulaPtr getOffsetVar(int dimension, const char* extra=NULL) const;
  
  ///
  /// invert the above function
  int offsetVarToDimension(const std::string& dimension, const char* extra=NULL) const;
  
  void printIdentifier(std::ostream& o) const { o <<_id << " "; }
  int id() const { return _id; }
  virtual int getAssociatedId() { return _id; }
  virtual bool isEnabledGpuRule() { return false; }
  
  const SimpleRegionPtr& applicableRegion() const { return _applicableRegion; }
  
  
  virtual int dimensions() const = 0;
  virtual FormulaPtr getSizeOfRuleIn(int d) = 0;
  virtual void generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, RuleFlavor flavor) = 0;

  virtual DependencyDirection getSelfDependency() const = 0;
  
  bool isDuplicated() const { return duplicateCount()>1; }
  virtual size_t duplicateCount() const { return 1; }
  /// returns old duplicate number
  virtual size_t setDuplicateNumber(size_t c){ JASSERT(c==0); return 0; }
  virtual size_t getDuplicateNumber() { return 0; }


  bool isDisabled() const { return _isDisabled; }
  void disableRule() { _isDisabled = true; }
  
  DataDependencyVectorMap& getDataDependencyVectorMap() {return _dataDependencyVectorMap; }
  
  ///Remove the specified dimension from every reference to the given matrix 
  ///that appears inside this rule
  virtual void removeDimensionFromMatrix(const MatrixDefPtr, const size_t) {}
  
  ///Fix the type of all the versioned regions associated with this rule
  virtual void fixVersionedRegionsType() {}
  
  ///Get the list of regions that the rule reads and modifies
  virtual RegionList getSelfDependentRegions() { return RegionList(); }
  
  ///Get the list of regions that the rule only reads or writes
  virtual RegionList getNonSelfDependentRegions() { return RegionList(); }

  virtual RegionList getFromRegions( ) const { return RegionList(); }
  
protected:
  int _id;
  SimpleRegionPtr _applicableRegion;
  bool _isDisabled;
  DataDependencyVectorMap _dataDependencyVectorMap; /**< Data dependency vector.
                                                     * It contains only the deps
                                                     * for regions that come 
                                                     * from "through" matrixes*/
};


/**
 * A pointer to the begin/end of of a Rule in a given dimension, used to sort rules
 */
class RuleDescriptor /*: public jalib::JPrintable*/ {
public:
  enum Type { RULE_BEGIN, RULE_END };

  ///
  /// Constructor
  RuleDescriptor(Type t, const RulePtr& r, const MatrixDefPtr& m, const FormulaPtr& f)
    : _type(t), _rule(r), _matrix(m), _formula(f)
  {}

  ///
  /// Used by quicksort
  bool operator< (const RuleDescriptor& that) const;

  ///
  /// Access member
  const RulePtr& rule() const { return _rule; }

  ///
  /// Test _type
  bool isBegin() const { return _type == RULE_BEGIN; };

  ///
  /// Test _type
  bool isEnd() const { return _type == RULE_END; };

  FormulaPtr getPosition() const { return _formula; }

  ///
  /// Make a maxima call to test equality with a given formula
  bool isSamePosition(const FormulaPtr& that) const;

  void print(std::ostream& o) const {
    o << _formula << "{";
    if(isBegin())
      o << "begin";
    else
      o << "end";
    o << "_" << _rule->id() << "} ";
  }
  
private:
  Type          _type;
  RulePtr       _rule;
  MatrixDefPtr  _matrix;
  FormulaPtr    _formula;
};

}

#endif
