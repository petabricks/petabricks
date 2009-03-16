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
#ifndef PETABRICKSTRANSFORM_H
#define PETABRICKSTRANSFORM_H

#include "jrefcounted.h"
#include "jprintable.h"
#include "matrixdef.h"
#include "choicegrid.h"
#include "rule.h"
#include "learner.h"
#include "performancetester.h"
#include "staticscheduler.h"

#include <vector>
#include <set>

namespace petabricks {

class Transform;
class TemplateArg;
typedef jalib::JRef<Transform> TransformPtr;
typedef jalib::JRef<TemplateArg> TemplateArgPtr;
class TransformList: public std::vector<TransformPtr>, public jalib::JRefCounted {};
class TemplateArgList: public std::vector<TemplateArgPtr>, public jalib::JRefCounted {};
typedef jalib::JRef<TransformList> TransformListPtr;
typedef jalib::JRef<TemplateArgList> TemplateArgListPtr;
typedef std::set<std::string> ConstantSet;

class TemplateArg : public jalib::JRefCounted, public jalib::JPrintable {
public:
  TemplateArg(std::string name, int min, int max)
    :_name(name), _min(min), _max(max) {
    JASSERT(max>=min)(min)(max);
  }
  void print(std::ostream& o) const { o << _name << "(" << _min << ", " << _max << ")"; }
  
  std::string name() const { return _name; }
  int min() const { return _min; }
  int max() const { return _max; }
  int range() const { return _max-_min+1; }
private:
  std::string _name;
  int _min;
  int _max;
};

class ConfigItem {
public:
  ConfigItem(bool isTunable, std::string name, int initial, int min, int max)
      :_isTunable(isTunable),
       _name(name),
       _initial(initial),
       _min(min),
       _max(max)
  {}
private:
  bool        _isTunable;
  std::string _name;
  int         _initial;
  int         _min;
  int         _max;
};

typedef std::vector<ConfigItem> ConfigItems;


/**
 * a transformation algorithm
 */
class Transform : public jalib::JRefCounted, public jalib::JPrintable {
public:
  ///
  /// Constructor
  Transform() :_isMain(false),_tuneId(0) {}
  
  //called durring parsing:
  void setName(const std::string& str) { _name=str; }
  void addFrom(const MatrixDefList&);
  void addThrough(const MatrixDefList&);
  void addTo(const MatrixDefList&);
  void setRules(const RuleList&);
  
  ///
  /// Initialize after parsing
  void initialize();

  void compile();

  void print(std::ostream& o) const;

  const std::string& name() const { return _name; }
  
  MatrixDefPtr lookupMatrix(const std::string& name) const{
    MatrixDefMap::const_iterator i = _matrices.find(name);
    JASSERT(i != _matrices.end())(name).Text("Unknown input/output matrix");
    return i->second;
  }

  void generateCode(CodeGenerator& o);

  void generateCodeSimple(CodeGenerator& o);

  void generateMainCode(CodeGenerator& o);

  void fillBaseCases(const MatrixDefPtr& matrix);
  
  const FreeVars& constants() const { return _constants; }
  FreeVars& constants() { return _constants; }

  void extractSizeDefines(CodeGenerator& o);

  void markMain() { _isMain=true; }

  Learner& learner() { return _learner; }
  PerformanceTester& tester() { return _tester; }

  void addTestCase(const TestCasePtr& p) {tester().addTestCase(p);}

  std::vector<std::string> maximalArgList() const;

  std::string createTunerPrefix(){
    return _name + "_" + jalib::XToString(_tuneId++) + "_";
  }

  int ruleIdOffset() const { return _rules.front()->id()-1; }

  std::string taskname() const { return _name+"_fin"; }

  void addTemplateArg(const TemplateArgList& args){
    _templateargs.insert(_templateargs.end(), args.begin(), args.end());
  }

  std::vector<std::string> spawnArgs() const;
  std::vector<std::string> spawnArgNames() const;
  std::vector<std::string> normalArgs() const;
  std::vector<std::string> normalArgNames() const;

  void genTmplJumpTable(CodeGenerator& o,
                        const std::string& rt,
                        const std::string& name,
                        const std::vector<std::string>& args,
                        const std::vector<std::string>& argNames);

  int tmplChoiceCount() const;

  bool isTemplate() const { return !_templateargs.empty(); }

  std::string tmplName(int n, CodeGenerator* o=NULL) const;

  void addConfig(const std::string& n, int initial, int min=0, int max=std::numeric_limits<int>::max()){
    _config.push_back(ConfigItem(false,n,initial, min,max));
  }
  
  void addTunable(const std::string& n, int initial, int min=0, int max=std::numeric_limits<int>::max()){
    _config.push_back(ConfigItem(true,n,initial, min,max));
  }

private:
  std::string   _name;
  MatrixDefList _from;
  MatrixDefList _through;
  MatrixDefList _to;
  MatrixDefMap  _matrices;
  RuleList      _rules;
  ChoiceGridMap _baseCases;
  FreeVars      _constants;
  bool          _isMain;
  Learner       _learner;
  StaticSchedulerPtr _scheduler;
  PerformanceTester _tester;
  TemplateArgList _templateargs;
  int _tuneId;
  ConfigItems _config;
};

}

#endif
