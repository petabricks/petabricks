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
#ifndef PETABRICKSRULECHOICE_H
#define PETABRICKSRULECHOICE_H

#include "formula.h"
#include "rule.h"

namespace petabricks {

struct RuleIdComparer {
  bool operator()(const petabricks::RulePtr& x, const petabricks::RulePtr& y){
    return x->id() < y->id();
  }
};

class CodeGenerator;
class RuleChoice;
class ScheduleNode;
typedef jalib::JRef<RuleChoice> RuleChoicePtr;

/**
 * Stores the choice made by the Learner for a given region
 */
class RuleChoice : public jalib::JRefCounted, public jalib::JPrintable {
public:
  static RuleChoicePtr makeRuleChoice(const RuleSet& choices, const MatrixDefPtr&, const SimpleRegionPtr&);
  static RuleChoicePtr makeCoscheduledRuleChoice(const RuleSet& choices, const MatrixDefList&, const SimpleRegionPtr&);

  ///
  /// Constructor
  RuleChoice(const RuleSet& rule, const FormulaPtr& cond=FormulaPtr(), const RuleChoicePtr& next=RuleChoicePtr());

  ///
  /// Needed for JPrintable
  void print(std::ostream& o) const;

  ///
  /// Output c++ code
  void generateCodeSimple ( bool isStatic
                          , const std::string& taskname
                          , Transform& trans
                          , ScheduleNode& node
                          , const SimpleRegionPtr& region
                          , CodeGenerator& o
                          , const std::string& tpfx
                          , int levelOffset = 0);
  
  const RuleSet& rules() const { return _rules; }

  static const FormulaPtr& autotuned();

  std::string processCondition(const std::string& name, const FormulaPtr& f, const std::string& choicename, CodeGenerator& o);

  int level() const { return 1+(_next?_next->level():0); }

  bool hasCondition() const { return _condition; }
private: 
  ///
  /// Rule to invoke
  RuleSet _rules;
  ///
  /// This choice may only be applied if this evaluates to true (may be null)
  FormulaPtr _condition;
  ///
  /// If _condition evaluates to false, use this choice instead (may be null)
  RuleChoicePtr _next;

  std::string _tunablePrefix;
};

}

#endif
