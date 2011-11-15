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
#ifndef PETABRICKSTRAININGDEPS_H
#define PETABRICKSTRAININGDEPS_H

#include "common/jassert.h"
#include "common/jrefcounted.h"
#include "common/jtunable.h"

#include "rule.h"
#include "heuristicmanager.h"

#include <map>
#include <sstream>
#include <vector>

namespace petabricks {

class TrainingDeps;
typedef jalib::JRef<TrainingDeps> TrainingDepsPtr;

class TrainingDeps : public jalib::JRefCounted {
public:

  void addAlgchoice(const std::string& name, int rules){
    _os << "    <algchoice "
        << " name=\""    << name << "\""
        << " rules=\""    << rules << "\""
        << " />\n";
  }

  void addTunable( bool isTunable
                 , const std::string& category
                 , const std::string& name
                 , jalib::TunableValue initial
                 , jalib::TunableValue min
                 , jalib::TunableValue max)
  {
    if(isTunable)
      _os<< "    <tunable";
    else
      _os<< "    <config";

    _os << " name=\""    << name << "\""
        << " type=\""    << category << "\""
        << " initial=\"" << initial << "\""
        << " min=\""     << min << "\""
        << " max=\""     << max << "\""
        << " />\n";
  }

  void beginTransform(const std::string& name,
                      const std::string& instanceName,
                      int templateChoice,
                      bool isVariableAccuracy,
                      double accuracyTarget
                      ){
    _os << "  <transform ";
    _os << " name=\""         << instanceName << "\"";
    _os << " templateName=\"" << name << "\"";
    _os << " isTemplateInstance=\"" << (name==instanceName ? "no" : "yes") << "\"";
    _os << " templateChoice=\"" << jalib::XToString(templateChoice) << "\"";
    _os << " isVariableAccuracy=\"" << isVariableAccuracy << "\"";
    _os << " accuracyTarget=\"" << accuracyTarget << "\"";
    _os << ">\n";
  }

  void beginGlobal(){
    _os << "  <global>\n";
  }
  void endGlobal(){
    _os << "  </global>\n";
  }

  void emitRules(std::string& choicename,
                 const std::vector<RulePtr>& sortedRules);

  void endTransform(const std::string& name, const std::string& /*instanceName*/){
    const std::vector<std::string>& calls = _callgraph[name];
    for(size_t i=0; i<calls.size(); ++i) {
      _os << "    <calls caller=\"" << name << "\" callee=\"" << calls[i]
          << "\" />\n";
    }
    _os << "  </transform>\n";
  }

  static void addCallgraphEdge(const std::string& caller, const std::string& callee){
    _callgraph[caller].push_back(callee);
  }

  void addHeuristics(const HeuristicMap& heuristics) {
    for(HeuristicMap::const_iterator i=heuristics.begin(), e=heuristics.end();
        i != e;
        ++i) {
      const std::string name = i->first;
      const HeuristicPtr& heuristic = i->second;
      addHeuristic(name, heuristic->usedFormula()->toCppString());
    }
  }
  
  void addHeuristic(const std::string name, const std::string formula) {
    _os << "  <heuristic";
    _os << " name=\"" << name << "\"";
    _os << " formula=\"" << jalib::escapeXML(formula) << "\"";
    _os << " />\n";
  }
  
  void dumpTo(std::ostream& o){
    o << "<traininginfo>\n";
    o << _os.str();
    o << "</traininginfo>\n";
  }

private:
  std::stringstream _os;
  static std::map<std::string, std::vector<std::string> > _callgraph;
};

}

#endif
