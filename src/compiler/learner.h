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
#ifndef PETABRICKSLEARNER_H
#define PETABRICKSLEARNER_H

#include "region.h"
#include "rulechoice.h"

namespace petabricks {

class PerformanceTester;

/**
 * Makes choices during code generation using accumulated information.
 * There is one Learner instance per Transform.
 */
class Learner{
public:
  ///
  /// Pick which rules to use for a given region
  RuleChoicePtr makeRuleChoice(const RuleSet& choices, const MatrixDefPtr&, const SimpleRegionPtr&);
  
  RuleChoicePtr makeCoscheduledRuleChoice(const RuleSet& choices, const MatrixDefList&, const SimpleRegionPtr&);
};

}

#endif
