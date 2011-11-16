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
#ifndef HEURISTIC_H
#define HEURISTIC_H

#include <limits>
#include <map>

#include "common/jrefcounted.h"
//#include "common/jassert.h"

#include "maximawrapper.h"

namespace petabricks {
typedef std::map<std::string, double> ValueMap;

class Heuristic : public jalib::JRefCounted {
public:
  Heuristic(const std::string formula) :
        _formula(MaximaWrapper::instance().runCommandSingleOutput(formula)),
        _min( -std::numeric_limits<double>::infinity()),
        _max( std::numeric_limits<double>::infinity()) {}
    
  FormulaPtr usedFormula() const;
  
  double eval (const ValueMap featureValues=ValueMap()) const;
  
  void setMin(const double min) { _min = min; }
  void setMax(const double max) { _max = max; }
  
private:
  double evalWithoutLimits(const ValueMap featureValues) const;
  
private:
  FormulaPtr _formula;
  double _min;
  double _max;
};


typedef jalib::JRef<Heuristic> HeuristicPtr;
typedef std::map<std::string, HeuristicPtr> HeuristicMap;


}

#endif