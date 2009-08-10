/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
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
#ifndef PETABRICKSRULE_H
#define PETABRICKSRULE_H

#include "formula.h"
#include "matrixdef.h"
#include "region.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"

#include <string>
#include <vector>

namespace petabricks {

class IterationOrderList;
class RIRScope;
class CodeGenerator;
class RuleDescriptor;
class MatrixDependencyMap;
class Transform;
class StaticScheduler;
class FormulaList;
class UserRule;
class RuleInterface;
typedef jalib::JRef<RuleInterface> RulePtr;
class RuleList : public std::vector<RulePtr> , public jalib::JRefCounted {};
typedef std::vector<RuleDescriptor>     RuleDescriptorList;
typedef std::vector<RuleDescriptorList> RuleDescriptorListList;
typedef jalib::JRef<MatrixDependencyMap> MatrixDependencyMapPtr;

struct RulePriCmp
{
  bool operator()(const RulePtr& r1, const RulePtr& r2) const;
};
typedef std::set<RulePtr, RulePriCmp> RuleSet;

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
 * Base class for rules, both UserRule and SyntheticRule
 */
class RuleInterface : public jalib::JRefCounted, public jalib::JPrintable {
public:
  RuleInterface();

  ///
  /// Initialize this rule after parsing
  virtual void initialize(Transform&) = 0;
  virtual void compileRuleBody(Transform& tx, RIRScope& s) = 0;

  virtual RuleFlags::PriorityT priority() const = 0;
  virtual bool isRecursive() const = 0;
  virtual bool canProvide(const MatrixDefPtr& m) const = 0;
  virtual bool isSingleElement() const = 0;

  virtual void collectDependencies(StaticScheduler& scheduler) = 0;
  virtual void getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension) = 0;
  
  virtual void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region) = 0; 
  virtual void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region) = 0;
  virtual void generateDeclCodeSimple(Transform& trans, CodeGenerator& o) = 0;
  virtual void generateTrampCodeSimple(Transform& trans, CodeGenerator& o) = 0;
  
  virtual void markRecursive() = 0;
  virtual const FormulaPtr& recursiveHint() const = 0;
  
  virtual bool hasWhereClause() const = 0;
  virtual FormulaPtr getWhereClause() const = 0;

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
  
  
  const SimpleRegionPtr& applicableRegion() const { return _applicableRegion; }
  
  
  virtual int dimensions() const = 0;
  virtual void removeInvalidOrders(IterationOrderList& o) = 0;
  virtual FormulaPtr getSizeOfRuleIn(int d) = 0;
  virtual void generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic) = 0;
  
protected:
  int _id;
  SimpleRegionPtr _applicableRegion;
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
