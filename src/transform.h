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
#include "rirscope.h"

#include <vector>
#include <set>
#include <limits>

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

class DoubleList: public std::vector<double>, public jalib::JRefCounted {};

class TemplateArg : public jalib::JRefCounted, public jalib::JPrintable {
public:
  TemplateArg(std::string name, int min, int max)
    :_name(name), _min(min), _max(max) {
    JASSERT(max>=min)(min)(max);
  }
  void print(std::ostream& o) const { o << _name << "(" << _min << ", " << _max << ")"; }
  
  const std::string& name() const { return _name; }
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
  ConfigItem(int flags, std::string name, int initial, int min, int max)
      :_flags(flags),
       _name(name),
       _initial(initial),
       _min(min),
       _max(max)
  {}
  
  std::string name     () const { return _name;     }
  int         initial  () const { return _initial;  }
  int         min      () const { return _min;      }
  int         max      () const { return _max;      }

  std::string category() const {
    std::string cat;

    if(hasFlag(ConfigItem::FLAG_USER))
      cat+="user.";
    else
      cat+="system.";

    if(hasFlag(ConfigItem::FLAG_TUNABLE))
      cat+="tunable";
    else
      cat+="config";

    return cat;
  }

  enum FlagT {
    FLAG_TUNABLE       = 1<<0, 
    FLAG_USER          = 1<<1,
    FLAG_SIZESPECIFIC = 1<<2,
    FLAG_ACCURACY      = 1<<3
  };
  bool hasFlag(FlagT f) const {
    return (_flags & f) != 0;
  }
private:
  int         _flags;
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
  Transform();
  
  //called durring parsing:
  void setName(const std::string& str) { _originalName=_name=str; }
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

  void generateCodeSimple(CodeGenerator& o, const std::string& nextMain = "NULL");
  
  void registerMainInterface(CodeGenerator& o);

  void generateMainInterface(CodeGenerator& o, const std::string& nextMain);

  void fillBaseCases(const MatrixDefPtr& matrix);
  
  const FreeVars& constants() const { return _constants; }
  FreeVars& constants() { return _constants; }

  void extractSizeDefines(CodeGenerator& o, FreeVars fv);

  void markMain() { _isMain=true; }

  Learner& learner() { return _learner; }
  //PerformanceTester& tester() { return _tester; }

  //void addTestCase(const TestCasePtr& p) {tester().addTestCase(p);}

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
                        bool isStatic,
                        const std::vector<std::string>& args,
                        const std::vector<std::string>& argNames);
  
  void extractConstants(CodeGenerator& o);

  int tmplChoiceCount() const;

  bool isTemplate() const { return !_templateargs.empty(); }

  std::string tmplName(int n, CodeGenerator* o=NULL) const;
  
  void addConfigItem(int flags, const std::string& n, int initial=0, int min=0, int max=std::numeric_limits<int>::max()){
    _config.push_back(ConfigItem(flags,n,initial, min,max));
    if(_config.back().hasFlag(ConfigItem::FLAG_SIZESPECIFIC))
      addConstant(n, FreeVar::FLAG_SIZESPECIFICCFG);
    else
      _scope->set(n, RIRSymbol::SYM_CONFIG_TRANSFORM_LOCAL);
  }

  std::string instClassName() const { return _name+"_instance"; }

  void markSplitSizeUse(CodeGenerator& o);

  void expandWhereClauses(RuleSet&, const MatrixDefPtr&, const SimpleRegionPtr&);

  void addParams(const OrderedFreeVars& p) { _parameters.insert(_parameters.end(), p.begin(), p.end()); }

  MatrixDefList defaultVisibleInputs() const {
    MatrixDefList tmp;
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      if( (*i)->numDimensions() == 0 )
        tmp.push_back(*i);
    }
    return tmp;
  }

  bool isMain() const { return _isMain; }

  void setAccuracyMetric(const std::string& str){
    JASSERT(_accuracyMetric=="")(_name).Text("accuracy_metric declared twice");
    _accuracyMetric=str;
  }
  void setAccuracyBins(const std::vector<double>& v){
    JASSERT(_accuracyBins.empty())(_name).Text("accuracy_bins declared twice");
    _accuracyBins = v;
  }
  void setGenerator(const std::string& str){
    JASSERT(_generator=="")(_name).Text("generator declared twice");
    _generator=str;
  }
    
  std::vector<std::string> argnames() const {
    std::vector<std::string> args;
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      args.push_back((*i)->name());
    }
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      args.push_back((*i)->name());
    }
    return args;
  }

  void addConstant(const std::string& c, int flags=0) { 
    _constants.insert(FreeVar(c,flags)); 
  }


  bool isAccuracyInverted() const {
    int forward = 0;
    int backward = 0;
    for(size_t i=1; i<_accuracyBins.size(); ++i){
      double a=_accuracyBins[i-1];
      double b=_accuracyBins[i];
      JASSERT(a!=b)(_name)(a)(b).Text("invalid accuracy_bins");
      if(a<b) ++forward;
      else ++backward;
    }
    JASSERT(forward==0 || backward==0)(forward)(backward)(_name).Text("invalid accuracy_bins");
    return backward>0;
  }

private:
  std::string     _originalName;
  std::string     _name;
  MatrixDefList   _from;
  MatrixDefList   _through;
  MatrixDefList   _to;
  MatrixDefMap    _matrices;
  RuleList        _rules;
  ChoiceGridMap   _choiceGrid;
  FreeVars        _constants;
  OrderedFreeVars _parameters;
  bool            _isMain;
  Learner         _learner;
  StaticSchedulerPtr _scheduler;
  //PerformanceTester  _tester;
  TemplateArgList     _templateargs;
  int                 _tuneId;
  ConfigItems         _config;
  RIRScopePtr         _scope;
  bool                _usesSplitSize;
  std::string         _accuracyMetric;
  std::vector<double> _accuracyBins;
  std::string         _generator;
};

}

#endif
