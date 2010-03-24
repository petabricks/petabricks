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
#ifndef PETABRICKSTRANSFORM_H
#define PETABRICKSTRANSFORM_H

#include "choicegrid.h"
#include "configitem.h"
#include "learner.h"
#include "matrixdef.h"
#include "rirscope.h"
#include "rule.h"
#include "staticscheduler.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/srcpos.h"

#include <limits>
#include <set>
#include <vector>

namespace petabricks {

class Transform;
typedef jalib::JRef<Transform> TransformPtr;
class TransformList: public std::vector<TransformPtr>, public jalib::JRefCounted, public jalib::SrcPosTaggable {};
typedef jalib::JRef<TransformList> TransformListPtr;
typedef std::set<std::string> ConstantSet;

class DoubleList: public std::vector<double>, public jalib::JRefCounted, public jalib::SrcPosTaggable {};

/**
 * a transformation algorithm
 */
class Transform : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
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
  
  FreeVars constants() const {
    FreeVars fv;
    for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i)
      fv.insert(i->name());
    return fv;
  }

  void extractSizeDefines(CodeGenerator& o, FreeVars fv, const char* inputsizestr);
  
  void declTransformNFunc(CodeGenerator& o);
  void declTryMemoizeFunc(CodeGenerator& o);

  void markMain() { _isMain=true; }
  void markMemoized() { _memoized=true; }

  Learner& learner() { return _learner; }

  //void addTestCase(const TestCasePtr& p) {tester().addTestCase(p);}

  std::vector<std::string> maximalArgList() const;

  int nextTunerId() {
    return _tuneId++;
  }

  int ruleIdOffset() const { return _rules.front()->id()-1; }

  std::string taskname() const { return _name+"_fin"; }

  void addTemplateArg(const TemplateArgList& args){
    for(size_t i=0; i<args.size(); ++i)
      args[i]->addFlag(ConfigItem::FLAG_TEMPLATEVAR);
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
  bool isVariableAccuracy() const { return !_accuracyBins.empty(); }

  std::string tmplName(int n, CodeGenerator* o=NULL);
  
  void addConfigItem(int flags, const std::string& n, int initial=0, int min=0, int max=std::numeric_limits<int>::max()){
    ConfigItems::iterator i;
    //check if its already there?
    for(i=_config.begin(); i!=_config.end(); ++i)
      if(i->name()==n)
        break;
    if(i==_config.end()){
      _config.push_back(ConfigItem(flags,n,initial,min,max));
      i=_config.end()-1;
    }else{
      i->merge(flags,n,initial,min,max);
    }
  }

  void addSizeVar(const std::string& name){
    addConfigItem(ConfigItem::FLAG_SIZEVAR, name);
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


  const ConfigItems& config() const { return _config; }

  void addRule(const RulePtr& rp) { _rules.push_back(rp); }
  
  
  const std::string& accuracyMetric() const { return _accuracyMetric; }

protected:
  static std::map<std::string, TransformPtr> theTransformMap();

private:
  std::string     _originalName;
  std::string     _name;
  MatrixDefList   _from;
  MatrixDefList   _through;
  MatrixDefList   _to;
  MatrixDefMap    _matrices;
  RuleList        _rules;
  ChoiceGridMap   _choiceGrid;
  OrderedFreeVars _parameters;
  bool            _isMain;
  bool            _memoized;
  Learner         _learner;
  StaticSchedulerPtr _scheduler;
  TemplateArgList     _templateargs;
  int                 _tuneId;
  ConfigItems         _config;
  RIRScopePtr         _scope;
  bool                _usesSplitSize;
  std::string         _accuracyMetric;
  std::vector<double> _accuracyBins;
  std::string         _generator;
  int                 _templateChoice;
  double              _curAccTarget;
};

}

#endif
