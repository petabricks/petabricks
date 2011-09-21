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
#ifndef HEURISTICMANAGER_H
#define HEURISTICMANAGER_H

#include <map>

#include "common/jrefcounted.h"
#include "common/jassert.h"

#include "maximawrapper.h"

namespace petabricks {
typedef std::map<std::string, double> ValueMap;

class Heuristic : public jalib::JRefCounted {
public:
  Heuristic(const std::string name, const std::string formula) :
        _name(name),
        _formula(MaximaWrapper::instance().runCommandSingleOutput(formula)) {}
    
  std::string name() const {return _name;}
  FormulaPtr formula() const { return _formula; }
  double eval (const ValueMap featureValues);
  
private:
  std::string _name;
  FormulaPtr _formula;
};




typedef jalib::JRef<Heuristic> HeuristicPtr;
typedef std::map<std::string, HeuristicPtr> HeuristicMap;



class HeuristicManager {
public:
  ///Singleton instance
  static HeuristicManager& instance() { static HeuristicManager inst;
                                        return inst;
                                      }
                                              
                                              
  void registerDefault(const std::string name, const std::string formula) {
          _defaultHeuristics[name] = HeuristicPtr(new Heuristic(name, formula));
        }
  
  HeuristicPtr& getHeuristic(const std::string name);
  
  const HeuristicMap& usedHeuristics() const { return _heuristicCache; }
  
private:
  HeuristicMap _heuristicCache;
  HeuristicMap _defaultHeuristics;
};

}

#endif