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
#ifndef HECURACHOICEGRID_H
#define HECURACHOICEGRID_H

#include "jrefcounted.h"
#include "jprintable.h"
#include "rule.h"
#include "formula.h"
#include "matrixdef.h"

#include <vector>
#include <map>

namespace hecura {
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

  void finalizeConstruction(const FormulaPtr& end, const RuleSet& applicable){
    _end=end;
    _applicableRules = applicable;
  }
public:
  static ChoiceGridPtr constructFrom( const RuleSet&                allowedRules
                                    , const RuleDescriptorListList& dimensions
                                    , size_t                        dimension = 0);

  void print(std::ostream& os) const;

  void generateCodeSimple(CodeGenerator& o, const SimpleRegionPtr& prefix);

  void buildIndex(ChoiceGridIndex& idx, const SimpleRegionPtr& prefix = 0);

  const RuleSet& rules() const { return _applicableRules; }
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
