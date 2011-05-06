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
#ifndef PETABRICKSCHOICEGRID_H
#define PETABRICKSCHOICEGRID_H

#include "formula.h"
#include "matrixdef.h"
#include "rule.h"

#include "common/jprintable.h"
#include "common/srcpos.h"
#include "common/jrefcounted.h"

#include <map>
#include <vector>

namespace petabricks {
class CodeGenerator;
class ChoiceGrid;
typedef jalib::JRef<ChoiceGrid>                    ChoiceGridPtr;
typedef std::vector<ChoiceGridPtr>                 ChoiceGridList;
class ChoiceGridIndex : public std::map<SimpleRegionPtr, ChoiceGridPtr>,
                        public jalib::JPrintable,
                        public jalib::SrcPosTaggable {
public:
  void print(std::ostream& os) const {
    for(const_iterator i=begin(); i!=end(); ++i)
      os << "    [" << i->first << "] : " << i->second << "\n";
  }
  void removeDisabledRules();
};
class ChoiceGridMap : public std::map<MatrixDefPtr, ChoiceGridIndex>, public jalib::JPrintable, public jalib::SrcPosTaggable {
public:
  void print(std::ostream& os) const {
    for(const_iterator i=begin(); i!=end(); ++i)
      os << "  " << i->first << ":\n" << i->second;
  }
  void removeDisabledRules(){
    for(iterator i=begin(); i!=end(); ++i)
      i->second.removeDisabledRules();
  }
};

/**
 * Divides a target matrix into regions with uniform sets of applicable rules
 */
class ChoiceGrid : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
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

  void removeDisabledRules();
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
