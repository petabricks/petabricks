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
#ifndef HECURARULE_H
#define HECURARULE_H

#include "matrixdependency.h"
#include "jconvert.h"
#include "jrefcounted.h"
#include "jprintable.h"
#include "codegenerator.h"
#include "region.h"
#include "matrixdef.h"
#include "formula.h"


#include <vector>

namespace hecura {
class CodeGenerator;
class RuleDescriptor;
class MatrixDependencyMap;
class Transform;
class StaticScheduler;
class FormulaList;
class Rule;
class FreeVarList;
typedef jalib::JRef<Rule> RulePtr;
class RuleList : public std::vector<RulePtr> , public jalib::JRefCounted {};
typedef std::set<RulePtr> RuleSet;
typedef std::vector<RuleDescriptor>     RuleDescriptorList;
typedef std::vector<RuleDescriptorList> RuleDescriptorListList;
typedef jalib::JRef<MatrixDependencyMap> MatrixDependencyMapPtr;


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
 * Represent a transform rule
 */
class Rule : public jalib::JRefCounted, public jalib::JPrintable {
public:
  ///
  /// Constructor -- return style rule
  Rule(const RegionPtr& to, const RegionList& from, const FormulaList& where);

  ///
  /// Constructor -- to style rule
  Rule(const RegionList& to, const RegionList& from, const FormulaList& where);
  
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
  /// ...
  void printIdentifier(std::ostream& o) const { o <<_id << " "; }

  ///
  /// Get the offset variable for the "center" of this rule.
  /// The offset variable is used to represent the location where the rule 
  /// is applied.  All coordinates are rewritten to be relative to the
  /// offset vars.
  FormulaPtr getOffsetVar(int dimension, const char* extra=NULL) const;
  
  ///
  /// Remove out-of-bounds solutions from the given formula list 
  FormulaPtr trimImpossible(const FormulaList& l);
  
  ///
  /// Add RuleDescriptors to output corresponding to the extrema of the applicable region in dimension
  void getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension);

  ///
  /// Access member
  int id() const { return _id; }

  ///
  /// Generate seqential code to declare this rule
  void generateDeclCodeSimple(Transform& trans, CodeGenerator& o);


  ///
  /// Generate seqential code to declare this rule
  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o);
  void generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o);


  ///
  /// Generate seqential code to invoke this rule
  void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region); 

  ///
  /// Return function the name of this rule in the code
  std::string implcodename(Transform& trans) const;
  std::string trampcodename(Transform& trans) const;

  ///
  /// 
  bool isReturnStyle() const { return _flags.isReturnStyle; }

  int dimensions() const;

  void addAssumptions() const;

  const SimpleRegionPtr& applicanbleRegion() const { return _applicanbleRegion; }

  void collectDependencies(StaticScheduler& scheduler);

  void markRecursive(const FormulaPtr& rh = new FormulaVariable(INPUT_SIZE_STR)) { 
    _flags.isRecursive = true; 
    _recursiveHint = rh;
  }

  bool isRecursive() const { return _flags.isRecursive; }

  RuleFlags::PriorityT priority() const { return _flags.priority; }
  const FormulaList& conditions() const { return _conditions; }

  void removeInvalidOrders(IterationOrderList& o);

  bool canProvide(const MatrixDefPtr& m) const {
    return _provides.find(m) != _provides.end();
  }

  void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  std::vector<std::string> getCallArgs(Transform& trans, const SimpleRegionPtr& region);

  const FormulaPtr& recursiveHint() const { return _recursiveHint; }


  FormulaPtr getSizeOfRuleIn(int d){
    for(size_t i=0; i<_to.size(); ++i){
      if(d<_to[i]->dimensions()){
        return _to[i]->getSizeOfRuleIn(d);
      }
    }
    JASSERT(false)(d)(_id);
  }

  bool isSingleElement() const {
    if(_to.size()!=1) return false;
    return _to[0]->isSingleElement();
  }
private:
  int _id;
  RuleFlags   _flags;
  RegionList  _from;
  RegionList  _to;
  FormulaList _conditions;
  FormulaList _definitions;
  SimpleRegionPtr _applicanbleRegion;
  std::string     _body;
  MatrixDependencyMap _depends;
  MatrixDependencyMap _provides;
  FormulaPtr          _recursiveHint;
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
