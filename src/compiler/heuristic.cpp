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
#include "heuristic.h"

double petabricks::Heuristic::eval (const ValueMap featureValues) const {
  FormulaPtr evaluated = _formula->clone();
  
  for(ValueMap::const_iterator i=featureValues.begin(), e=featureValues.end();
      i!=e;
      ++i) {
    const std::string& featureName=i->first;
    const std::string featureValueStr = jalib::XToString(i->second);
    
    evaluated = MaximaWrapper::instance().subst(featureValueStr, featureName, evaluated);
  }
  
  evaluated = MaximaWrapper::instance().toFloat(evaluated);
  
  double value = evaluated->value();
  
  //Keep the value within  the limits
  if (value < _min) {
    return _min;
  }
  else if (value > _max) {
    return _max;
  }
  else {
    return value;
  }
}

petabricks::FormulaPtr petabricks::Heuristic::usedFormula() const { 
  if (! _formula->isConstant()) {
    return _formula;
  }
  
  //The formula is a constant value
  
  double value = eval(ValueMap());
  if(value < _min) {
    //Return min
    return MaximaWrapper::instance().runCommandSingleOutput(jalib::XToString(_min));
  }
  if(value > _max) {
    //Return max
    return MaximaWrapper::instance().runCommandSingleOutput(jalib::XToString(_max));
  }
  
  return _formula;
}