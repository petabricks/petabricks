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
#ifndef PETABRICKSCHOICEGRID_H
#define PETABRICKSCHOICEGRID_H

#include "formula.h"
#include "matrixdef.h"
#include "rule.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"

#include <map>
#include <vector>

namespace petabricks {
class CodeGenerator;
class ChoiceGrid;
typedef jalib::JRef<ChoiceGrid>                    ChoiceGridPtr;
typedef std::vector<ChoiceGridPtr>                 ChoiceGridList;
class ChoiceGridIndex : public std::map<SimpleRegionPtr, ChoiceGridPtr>, public jalib::JPrintable {
public:
  void print(std::ostream& os) const {
    for(const_iterator i=begin(); i!=end(); ++i)
      os << "    [" << i->first << "] : " << i->second << "\n";
  }
};
class ChoiceGridMap : public std::map<MatrixDefPtr, ChoiceGridIndex>, public jalib::JPrintable {
public:
  void print(std::ostream& os) const {
    for(const_iterator i=begin(); i!=end(); ++i)
      os << "  " << i->first << ":\n" << i->second;
  }
};

/**
 * Divides a target matrix into regions with uniform sets of applicable rules
 */
class ChoiceGrid : public jalib::JRefCounted, public jalib::JPrintable {
protected:
  ChoiceGrid(int d, const FormulaPtr& begin) : _dimension(d), _begin(begin) {}

  void finalizeConstruction(const FormulaPtr& end, const RuleSet& applicable);
public:
  static ChoiceGridPtr constructFrom( const RuleSet&                allowedRules
                                    , const RuleDescriptorListList& dimensions
                                    , size_t                        dimension = 0);

  void print(std::ostream& os) const;

  void generateCodeSimple(CodeGenerator& o, const SimpleRegionPtr& prefix);

  void buildIndex(ChoiceGridIndex& idx, const SimpleRegionPtr& prefix = 0);

  const RuleSet& rules() const { return _applicableRules; }
  RuleSet& rules() { return _applicableRules; }

  bool hasWhereClauses() const {
    if(_applicableRules.empty()) return false;
    return (*_applicableRules.begin())->hasWhereClause();
  }

  void applyRulePriorities();
private:
  int           _dimension;
  FormulaPtr    _begin;
  FormulaPtr    _end;
  RuleSet       _applicableRules;
  ChoiceGridPtr _nextElement;
  ChoiceGridPtr _nextDimension;

};

// class ChoiceGridElement : public jalib::JRefCounted, public jalib::JPrintable {
// public:
//   void print(std::ostream& os) const;
// };

}

#endif
