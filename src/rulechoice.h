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
#ifndef HECURARULECHOICE_H
#define HECURARULECHOICE_H

#include "rule.h"
#include "formula.h"
#include "jrefcounted.h"

namespace hecura {

class CodeGenerator;
class RuleChoice;
typedef jalib::JRef<RuleChoice> RuleChoicePtr;

/**
 * Stores the choice made by the Learner for a given region
 */
class RuleChoice : public jalib::JRefCounted, public jalib::JPrintable {
public:
  ///
  /// Constructor
  RuleChoice(const RulePtr& rule, const FormulaPtr& cond=FormulaPtr(), const RuleChoicePtr& next=RuleChoicePtr());

  ///
  /// Needed for JPrintable
  void print(std::ostream& o) const;

  ///
  /// Output c++ code
  void generateCodeSimple(const SimpleRegionPtr& region, CodeGenerator& o);
private: 
  ///
  /// Rule to invoke
  RulePtr _rule;
  ///
  /// This choice may only be applied if this evalates to true (may be null)
  FormulaPtr _condition;
  ///
  /// If _condition evaluates to false, use this choice instead (may be null)
  RuleChoicePtr _next;
};

}

#endif
