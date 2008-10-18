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
#ifndef HECURALEARNER_H
#define HECURALEARNER_H

#include "rulechoice.h"
#include "region.h"

namespace hecura {

class PerformanceTester;

/**
 * Makes choices during code generation using accumulated information.
 * There is one Learner instance per Transform.
 */
class Learner{
public:
  ///
  /// Constructor
  Learner();

  ///
  /// Called before a code generation cycle
  void onIterationBegin();

  ///
  /// Pick which rules to use for a given region
  RuleChoicePtr makeRuleChoice(const RuleSet& choices, const MatrixDefPtr&, const SimpleRegionPtr&);

  ///
  /// Pick which order to iterate in, initial value of choices is allows directions
  void makeIterationChoice(IterationOrderList& choices, const MatrixDefPtr&, const SimpleRegionPtr&){}

  ///
  /// Called after a code generation cycle
  void onIterationEnd();

  ///
  /// Called after onIterationEnd().
  /// True causes the compiler to call runTests then start over
  bool shouldIterateAgain();

  ///
  /// Run performance tests on the output of the last iteration
  void runTests(PerformanceTester& tester);

private:
  int _numIterations;
};

}

#endif
