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
#include "transform.h"

#include "codegenerator.h"
#include "clcodegenerator.h"
#include "maximawrapper.h"
#include "pbc.h"
#include "scheduler.h"
#include "syntheticrule.h"

#include "common/jconvert.h"

#include <algorithm>

namespace{
  //helper func
  template<typename T> void appendAll(T& dst, const T& src){
    dst.insert(dst.end(), src.begin(), src.end());
  }
}
  
petabricks::Transform::Transform() 
  : _isMain(false)
  , _memoized(false)
  , _tuneId(0)
  , _scope(RIRScope::global()->createChildLayer())
  , _usesSplitSize(false)
  , _templateChoice(-1)
  , _curAccTarget(DEFAULT_ACCURACY)
{}

void petabricks::Transform::addFrom(const MatrixDefList& l){
  appendAll(_from, l);
  for(MatrixDefList::const_iterator i=l.begin(); i!=l.end(); ++i){
    MatrixDefPtr& elmt = _matrices[(*i)->name()];
    if(elmt){
      JASSERT(false)(elmt)(*i).Text("Overlapping input/outputs not yet supported");
    }
    elmt = *i;
    elmt->addType(MatrixDef::T_FROM);
  }
}
void petabricks::Transform::addThrough(const MatrixDefList& l){
  appendAll(_through, l);
  for(MatrixDefList::const_iterator i=l.begin(); i!=l.end(); ++i){
    MatrixDefPtr& elmt = _matrices[(*i)->name()];
    if(elmt){
      JASSERT(false)(elmt)(*i).Text("Overlapping input/outputs not yet supported");
    }
    elmt = *i;
    elmt->addType(MatrixDef::T_THROUGH);
  }
}
void petabricks::Transform::addTo(const MatrixDefList& l){
  appendAll(_to, l);
  for(MatrixDefList::const_iterator i=l.begin(); i!=l.end(); ++i){
    MatrixDefPtr& elmt = _matrices[(*i)->name()];
    if(elmt){
      JASSERT(false)(elmt)(*i).Text("Overlapping input/outputs not yet supported");
    }
    elmt = *i;
    elmt->addType(MatrixDef::T_TO);
  }
}
void petabricks::Transform::setRules(const RuleList& l){
  JWARNING(_rules.size()==0)(_rules.size());
  appendAll(_rules, l);
}

void petabricks::Transform::print(std::ostream& o) const {
  SRCPOSSCOPE();
  o << "lineno " << srcPos() << std::endl;
  if(!_templateargs.empty()){ 
    o << "template < ";   printStlList(o, _templateargs.begin(), _templateargs.end(), ", "); 
    o << " > \n";
  }
  o << "transform " << _name;
  if(!_parameters.empty()){ 
    o << "\nparam ";   printStlList(o, _parameters.begin(), _parameters.end(), ", ");
  }
  if(!_from.empty()){ 
    o << "\nfrom ";   printStlList(o, _from.begin(), _from.end(), ", ");
  }
  if(!_through.empty()){ 
    o << "\nthrough "; printStlList(o, _through.begin(), _through.end(), ", ");
  }
  if(!_to.empty()){ 
    o << "\nto ";     printStlList(o, _to.begin(), _to.end(), ", ");
  }
  if(!_config.empty()){ 
    o << "\nconfigitems ";   printStlList(o, _config.begin(), _config.end(), ", ");
  }
  if(!_accuracyMetric.empty()) o << "\naccuracy_metric " << _accuracyMetric;
  if(!_accuracyBins.empty()){ 
    o << "\naccuracy_bins";   printStlList(o, _accuracyBins.begin(), _accuracyBins.end(), ", ");
  }
  if(!_generator.empty()) o << "\ngenerator" << _generator;

  o << "\n{\n";
  printStlList(o, _rules);
  o << "}\n";
  o << "ChoiceGrid:\n" << _choiceGrid;
  o << "\n";
}

void petabricks::Transform::initialize() {
  SRCPOSSCOPE();
  MaximaWrapper::instance().pushContext();

  if(_accuracyBins.size()>1){
    JWARNING(_templateargs.empty())(_name).Text("variable accuracy templates not yet supported");
    _templateargs.push_back(new TemplateArg(TEMPLATE_BIN_STR, 0, (int)_accuracyBins.size()-1));
  }

  jalib::Map(&MatrixDef::initialize, *this, _from);
  jalib::Map(&MatrixDef::initialize, *this, _through);
  jalib::Map(&MatrixDef::initialize, *this, _to);

  jalib::Map(&MatrixDef::exportConstants, *this, _from);
  jalib::Map(&MatrixDef::exportConstants, *this, _through);
  jalib::Map(&MatrixDef::exportConstants, *this, _to);

  jalib::Map(&MatrixDef::exportAssumptions, _from);
  jalib::Map(&MatrixDef::exportAssumptions, _through);
  jalib::Map(&MatrixDef::exportAssumptions, _to);

  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i)
    MaximaWrapper::instance().declareInteger(i->name());
  
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->shouldPass())
      _scope->set(i->name(), RIRSymbol::SYM_CONFIG_PASSED);
    else
      _scope->set(i->name(), RIRSymbol::SYM_CONFIG_TRANSFORM_LOCAL);
  }

  jalib::Map(&RuleInterface::initialize, *this, _rules);

  for(size_t i=0; i<_rules.size(); ++i){
    _rules[i]->performExpansion(*this);
  }

  for(MatrixDefList::iterator m=_to.begin(); m!=_to.end(); ++m)
    fillBaseCases(*m);
  for(MatrixDefList::iterator m=_through.begin(); m!=_through.end(); ++m)
    fillBaseCases(*m);

  //tester().setIOSizes(_from.size(), _to.size());

  if(isVariableAccuracy())
    RIRScope::global()->set(_name, RIRSymbol::SYM_TRANSFORM_VARACCURACY);
  else if(isTemplate())
    RIRScope::global()->set(_name, RIRSymbol::SYM_TRANSFORM_TEMPLATE);
  else
    RIRScope::global()->set(_name, RIRSymbol::SYM_TRANSFORM);

  theTransformMap()[_name] = this;

  MaximaWrapper::instance().popContext();
}

void petabricks::Transform::fillBaseCases(const MatrixDefPtr& matrix) {
  SRCPOSSCOPE();
  RuleDescriptorListList boundaries;
  boundaries.resize( matrix->numDimensions() );
  RuleSet allowed;
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    if((*i)->canProvide(matrix)){
      allowed.insert(*i);
      //JTRACE("adding allowed rule")((*i)->id());
    }
  }

  for(size_t d=0; d<boundaries.size(); ++d){
    for(RuleSet::iterator i=allowed.begin(); i!=allowed.end(); ++i){
      (*i)->getApplicableRegionDescriptors(boundaries[d], matrix, d, *i);
    }
    std::sort(boundaries[d].begin(), boundaries[d].end());
  }
  ChoiceGridPtr tmp = ChoiceGrid::constructFrom(allowed, boundaries);
  if(matrix->numDimensions()>0){
    tmp->buildIndex(_choiceGrid[matrix]);
  }else{
    _choiceGrid[matrix][new SimpleRegion()] = tmp;
  }

  //convert any where clauses
  ChoiceGridIndex& regions=_choiceGrid[matrix];
  for(ChoiceGridIndex::iterator i=regions.begin(); i!=regions.end(); ++i){
    ChoiceGrid& cg = *i->second;
    if(cg.hasWhereClauses()){
      JTRACE("converting where clauses to WhereExpansionRule")(matrix)(i->first);
      expandWhereClauses(cg.rules(), matrix, i->first);
    }
  }
}

void petabricks::Transform::expandWhereClauses( RuleSet& rules
                                              , const MatrixDefPtr&
                                              , const SimpleRegionPtr&){
  SRCPOSSCOPE();
  //TODO: it is possible that some subset of the rules could make a complete choice
  //      at some point we may want to detect such subsets and create multiple
  //      WhereExpansionRules for each of those subsets
  // for now, just find first allowed rule dynamically
  
  RuleSet whereRules;
  
  //Separate rules with where clauses
  for(RuleSet::iterator i=rules.begin(), e=rules.end(); i != e; ++i) {
    const RulePtr thisRule= *i;
    if(thisRule->hasWhereClause()) {
      whereRules.insert(thisRule);
      rules.erase(i);
    }
  }
  
  //Add fallback rule inside whereExpansion
  for(RuleSet::iterator i=rules.begin(), e=rules.end(); i != e; ++i) {
    const RulePtr thisRule= *i;
    if( ! thisRule->hasWhereClause()) {
      whereRules.insert(thisRule);
      break;
    }
  }
  
  //Add the where expansion rule to the set of rules
  RulePtr t = new WhereExpansionRule(whereRules);
  rules.insert(t);
  _rules.push_back(t);
}

void petabricks::Transform::compile(){ 
  SRCPOSSCOPE();
  MaximaWrapper::instance().pushContext();
  jalib::Map(&MatrixDef::exportAssumptions, _from);
  jalib::Map(&MatrixDef::exportAssumptions, _through);
  jalib::Map(&MatrixDef::exportAssumptions, _to);


  jalib::Map(&RuleInterface::compileRuleBody, *this, *_scope, _rules);

  JASSERT(!_scheduler);

  _choiceGrid.removeDisabledRules();

  _scheduler=new StaticScheduler(_choiceGrid, *this);
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    _scheduler->markInputMatrix(*i);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    _scheduler->markOutputMatrix(*i);
  }
  for(RuleList::const_iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->collectDependencies(_scheduler);
  }
  _scheduler->generateSchedule();
  
  MaximaWrapper::instance().popContext();
}
  
int petabricks::Transform::tmplChoiceCount() const {
  int choiceCnt = 1;
  for(size_t i=0; i<_templateargs.size(); ++i){
    choiceCnt*=_templateargs[i]->range();
  }
  return choiceCnt;
}
  
std::string petabricks::Transform::tmplName(int n, CodeGenerator* o) {
  std::string name = _name+TMPL_IMPL_PFX;
  _curAccTarget = DEFAULT_ACCURACY;
  int choice=n;
  //add #defines
  for(size_t i=0; i<_templateargs.size(); ++i){
    int val=(choice%_templateargs[i]->range()) + _templateargs[i]->min().i();
    choice/=_templateargs[i]->range();
    if(o!=NULL){
      o->write("#define " + _templateargs[i]->name() + " " + jalib::XToString(val));
      if(_templateargs[i]->name() == TEMPLATE_BIN_STR){
        if(isAccuracyInverted()){
          _curAccTarget = -(*(_accuracyBins.rbegin()+val));
        }else{
          _curAccTarget = (*(_accuracyBins.begin()+val));
        }
      }
    }
    name += "_" + jalib::XToString(val);
  }
  JASSERT(choice==0)(choice);
  return name;
}

void petabricks::Transform::generateCode(CodeGenerator& o){ 
  SRCPOSSCOPE();
  if(_templateargs.empty())
    generateCodeSimple(o); //normal case
  else {
    std::string origName = _name;
    //count number of times we need to explode it
    size_t choiceCnt = tmplChoiceCount();
    JWARNING(choiceCnt<25)(choiceCnt)(_name).Text("Explosion of choices for template... are you sure???");
    //for each possible way
    for(size_t c=0; c<choiceCnt; ++c){
      std::string nextName = "NULL";
      if(c+1 < choiceCnt) nextName = tmplName(c+1)+"_main::instance()";
      _name = tmplName(c, &o);
      _templateChoice = c;
    

      JTRACE("generating template version")(c);
      generateCodeSimple(o, nextName);

      //remove defines
      for(size_t i=0; i<_templateargs.size(); ++i){
        o.write("#undef " + _templateargs[i]->name());
      }

      _name = origName;
    }
    _templateChoice = -1;
    genTmplJumpTable(o, true, normalArgs(), normalArgNames());
    genTmplJumpTable(o, false, normalArgs(), normalArgNames());
    o.hos() << "typedef "+tmplName(0)+"_main "+_name+"_main;\n";
  }
}
  
void petabricks::Transform::genTmplJumpTable(CodeGenerator& o,
                    bool isStatic,
                    const std::vector<std::string>& args,
                    const std::vector<std::string>& argNames)
{
  SRCPOSSCOPE();
  std::ostringstream formula;
  std::stringstream ss;
  std::vector<std::string> targs;
  for(size_t i=0, mult=1; i<_templateargs.size(); ++i){
    targs.push_back("int "+_templateargs[i]->name());
    if(mult>1) formula << " + " << mult << "*";
    formula << '(' << _templateargs[i]->name() << '-' << _templateargs[i]->min() << ')';
    mult *= _templateargs[i]->range();
  }
  targs.insert(targs.end(), args.begin(), args.end());
  if(isStatic)
    o.beginFunc( "void" , _name+TX_STATIC_POSTFIX, targs);
  else
    o.beginFunc( "petabricks::DynamicTaskPtr" , _name+TX_DYNAMIC_POSTFIX, targs);
  
  for(size_t i=0; i<_templateargs.size(); ++i){
    ss << "JASSERT(" << _templateargs[i]->name() << ">=" << _templateargs[i]->min() << " && "
                     << _templateargs[i]->name() << "<=" << _templateargs[i]->max() << ")"
                     << "(" << _templateargs[i]->name() << ");";
    o.write(ss.str());
    ss.str("");
  }

  //count number of times we need to explode it
  size_t choiceCnt = tmplChoiceCount();
  //for each possible way
  o.beginSwitch(formula.str());
  for(size_t c=0; c<choiceCnt; ++c){
    std::string fn = tmplName(c);
    o.beginCase(c);
    if(isStatic){
      o.call(fn+TX_STATIC_POSTFIX, argNames);
    }else{
      o.write("return ");
      o.call(fn+TX_DYNAMIC_POSTFIX, argNames);
    }
    o.endCase();
  }
  if(isStatic)
    o.write("default: JASSERT(false); return;");
  else
    o.write("default: JASSERT(false); return 0;");
  o.endSwitch();
  o.endFunc();
}


std::vector<std::string> petabricks::Transform::normalArgs() const{
  std::vector<std::string> args;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->argDeclRW(args);
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->argDeclRO(args);
  }
  return args;
}

std::vector<std::string> petabricks::Transform::normalArgNames() const{
  std::vector<std::string> argNames;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    argNames.push_back((*i)->name());
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    argNames.push_back((*i)->name());
  }
  return argNames;
}

std::vector<std::string> petabricks::Transform::spawnArgs() const{
  std::vector<std::string> args = normalArgs();
  args.push_back("const DynamicTaskPtr& _before");
  return args;
}
std::vector<std::string> petabricks::Transform::spawnArgNames() const{
  std::vector<std::string> args = normalArgNames();
  args.push_back("_before");
  return args;
}


void petabricks::Transform::generateCodeSimple(CodeGenerator& o, const std::string& nextMain){ 
  SRCPOSSCOPE();
  _usesSplitSize=false;
  _tuneId = 0;
  std::vector<std::string> args = normalArgs();
  std::vector<std::string> argNames = normalArgNames();
  std::vector<std::string> returnStyleArgs = args;
  if(_to.size()==1) returnStyleArgs.erase(returnStyleArgs.begin());

  o.cg().beginTransform(_originalName, _name, _templateChoice, !_accuracyBins.empty(), _curAccTarget);
  o.comment("Begin output for transform " + _name);
  o.newline();
  
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_FROMCFG)){
      i->createTunableDecls(_name+"_", o);
    }
  }

  o.write("#define TRANSFORM_LOCAL(x) PB_CAT("+_name+"_, x)");
  o.newline();

  o.comment("User rules");
  Map(&RuleInterface::generateDeclCodeSimple, *this, o, _rules);
  o.newline();

  o.beginClass(instClassName(), "petabricks::TransformInstance");

  o.globalDefine(_name+TX_DYNAMIC_POSTFIX+"(args...)",
      "petabricks::tx_call_dynamic(new "+instClassName()+"(args))");
  o.globalDefine(_name+TX_STATIC_POSTFIX+"(args...)",
      instClassName()+"(args).runStatic()");

  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    o.addMember((*i)->matrixTypeName(), (*i)->name());
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.addMember((*i)->constMatrixTypeName(), (*i)->name());
  }
  
  if(_scheduler->size()>1){
    o.beginFunc("bool", "useContinuation");
    o.createTunable(true, "system.flag.unrollschedule", _name + "_unrollschedule", 1, 0, 1);
    o.write("return "+_name + "_unrollschedule == 0;");
    o.endFunc();
  }

  o.constructorBody("init();");
  o.beginFunc("void", "init");
  extractConstants(o);
  o.endFunc();
  
  o.beginFunc("DynamicTaskPtr", "runDynamic");
  if(_memoized){
    o.beginIf("tryMemoize()");
    o.write("return NULL;");
    o.endIf();
  }
#ifndef SINGLE_SEQ_CUTOFF
  o.createTunable(true, "system.cutoff.sequential", _name + "_sequentialcutoff", 0);
  o.beginIf(TRANSFORM_N_STR "() < TRANSFORM_LOCAL(sequentialcutoff)");
#else
  o.beginIf(TRANSFORM_N_STR "() < sequentialcutoff");
#endif
  o.write("runStatic();");
  o.write("return NULL;");
  o.endIf();
  _scheduler->generateCode(*this, o, E_RF_DYNAMIC);
  o.endFunc();

  o.beginFunc("void", "runStatic");
  if(_memoized){
    o.beginIf("tryMemoize()");
    o.write("return;");
    o.endIf();
  }
  _scheduler->generateCode(*this, o, E_RF_STATIC);
  o.endFunc();
  
  o.comment("Rule trampolines");
  Map(&RuleInterface::generateTrampCodeSimple, *this, o, _rules);
  o.newline();

  declTransformNFunc(o);
  
  if(_memoized){
    declTryMemoizeFunc(o);
  }

  o.mergehelpers();

  o.endClass();
  
  generateMainInterface(o, nextMain);
  o.write("#undef TRANSFORM_LOCAL");
  o.comment("End of output for "+_name);
  o.cg().endTransform(_originalName, _name);
  o.newline();
  o.newline();
}

void petabricks::Transform::declTransformNFunc(CodeGenerator& o){
  SRCPOSSCOPE();
  o.beginFunc("IndexT", TRANSFORM_N_STR);
  o.write("IndexT _rv_n=1;");
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_SIZEVAR))
      o.write("_rv_n = std::max<IndexT>(_rv_n, "+i->name()+");");
  }
  o.write("return _rv_n;");
  o.endFunc();
}

void petabricks::Transform::declTryMemoizeFunc(CodeGenerator& o){
  SRCPOSSCOPE();
  o.beginFunc("bool", "tryMemoize");
  std::string abortCond = "false";
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i)
    abortCond += " || !"+(*i)->name()+".isEntireBuffer()";
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i)
    abortCond += " || !"+(*i)->name()+".isEntireBuffer()";
  o.beginIf(abortCond);
  o.write("return false;");
  o.endIf();
  o.write("MemoizationInstance<"+jalib::XToString(_from.size())+","+jalib::XToString(_to.size())+"> _memo;");
  o.write("static MemoizationSite<"+jalib::XToString(_from.size())+","+jalib::XToString(_to.size())+"> _cache;");
  for(size_t i=0; i!=_from.size(); ++i)
    o.write(_from[i]->name()+".exportTo(_memo.input("+jalib::XToString(i)+"));");
  for(size_t i=0; i!=_to.size(); ++i)
    o.write(_to[i]->name()+".exportTo(_memo.output("+jalib::XToString(i)+"));");
  o.beginIf("_cache.memoize(_memo)");
  for(size_t i=0; i!=_to.size(); ++i)
    o.write(_to[i]->name()+".copyFrom(_memo.output("+jalib::XToString(i)+"));");
  o.write("return true;");
  o.elseIf();
  o.write("return false;");
  o.endIf();
  o.endFunc();
}

void petabricks::Transform::markSplitSizeUse(CodeGenerator& o){
  SRCPOSSCOPE();
  if(!_usesSplitSize){
    _usesSplitSize=true;
    o.createTunable(true, "system.cutoff.splitsize", _name + "_splitsize", 64, 1);
  }
}

void petabricks::Transform::extractSizeDefines(CodeGenerator& o, FreeVars fv, const char* inputsizestr){
  SRCPOSSCOPE();
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_FROMCFG))
      fv.insert(i->name());
  }
  
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->extractDefines(fv, o);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->extractDefines(fv, o);
  }
  
  //construct size specific config items
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    i->assignTunableDecls(_name+"_", o, inputsizestr);
  }
}

#ifdef HAVE_OPENCL
void petabricks::Transform::extractOpenClSizeDefines(CLCodeGenerator& o, unsigned int dims){
  FreeVars fv;
  SRCPOSSCOPE(); 
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->extractCLDefines(fv, o, dims);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->extractCLDefines(fv, o, dims);
  }
}
#endif

void petabricks::Transform::extractConstants(CodeGenerator& o){
  SRCPOSSCOPE();
#ifdef INPUT_SIZE_STR
  o.addMember("IndexT", INPUT_SIZE_STR,       "0");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.write(INPUT_SIZE_STR " += " + (*i)->name() + ".count();");
  }
#endif
#ifdef INPUT_PERIMETER_STR
  o.addMember("IndexT", INPUT_PERIMETER_STR,  "0");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.write(INPUT_PERIMETER_STR " += " + (*i)->name() + ".perimeter();");
  }
#endif
#ifdef OUTPUT_SIZE_STR
  o.addMember("IndexT", OUTPUT_SIZE_STR,      "0");
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    o.write(OUTPUT_SIZE_STR " += " + (*i)->name() + ".count();");
  }
#endif
  
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_FROMCFG) && i->shouldPass()){
      o.addMember(i->memberType(), i->name(), "1");
    }
  }

  //adds member for all _constants
  extractSizeDefines(o, FreeVars(), TRANSFORM_N_STR"()");

  Map(&MatrixDef::verifyDefines, o, _from);
  Map(&MatrixDef::verifyDefines, o, _to);
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    (*i)->allocateTemporary(o, false, false);
  } 
}

void petabricks::Transform::registerMainInterface(CodeGenerator& o){
  SRCPOSSCOPE();
  //TODO: generate as a binary search
  if(_templateargs.empty()){
    std::string n = name()+"_main::instance()";
    o.beginIf("name == \""+name()+"\"");
    o.write("return "+n+";");
    o.endIf();
  }else{
    size_t choiceCnt = tmplChoiceCount();
    for(size_t c=0; c<choiceCnt; ++c){
      std::string n = tmplName(c)+"_main::instance()";
      std::string ifcond = "name == \""+tmplName(c)+"\"";
      ifcond += " || name == \""+name()+"<"+jalib::XToString(c)+">\"";
      if(c==0)
        ifcond += " || name==\""+name()+"\"";
      o.beginIf(ifcond);
      o.write("return "+n+";");
      o.endIf();
    }
  }
}
  

void petabricks::Transform::generateMainInterface(CodeGenerator& o, const std::string& nextMain){ 
  SRCPOSSCOPE();
  std::vector<std::string> argNames = normalArgNames();
  
  o.beginClass(_name+"_main", "petabricks::PetabricksRuntime::Main");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->varDeclCodeRO(o);
  }
//for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
//  (*i)->varDeclCodeRW(o);
//}
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->varDeclCodeRW(o);
  }

  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_FROMCFG) && i->shouldPass()){
      o.addMember(i->memberType(), i->name(), "1");
    }
  }

  o.beginFunc("std::string", "helpString");
  {
    std::ostringstream os;
    os <<"return \"";
    for( OrderedFreeVars::const_iterator i=_parameters.begin()
       ; i!=_parameters.end()
       ; ++i )
      os << (*i) << " ";
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i)
      os << (*i)->name() << " ";
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i)
      os << (*i)->name() << " ";
    os <<"\";";
    o.write(os.str());
  }
  o.endFunc();
  
  o.beginFunc("int", "numInputs");
  o.write("return "+jalib::XToString(_from.size()+_parameters.size())+";");
  o.endFunc();
  
  o.beginFunc("int", "numOutputs");
  o.write("return "+jalib::XToString(_to.size())+";");
  o.endFunc();

  declTransformNFunc(o);

  int firstInput=(int)_parameters.size();
  int firstOutput=firstInput+(int)_from.size();

  o.beginFunc("void", "readInputs", std::vector<std::string>(1, "ArgListT argv"));
  {
    int a=0;
    for( OrderedFreeVars::const_iterator i=_parameters.begin()
       ; i!=_parameters.end()
       ; ++i )
    {
      o.addMember("IndexT", *i,       "0");
      o.write(*i + " = jalib::StringToInt(argv["+jalib::XToString(a++)+"]);");
    }
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      (*i)->readFromFileCode(o,"argv["+jalib::XToString(a++)+"].c_str()");
    }
    FreeVars t;
    t.insertAll(_parameters);
    extractSizeDefines(o, t, TRANSFORM_N_STR"()");
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->allocateTemporary(o, true, false);
    }
  }
  o.endFunc();
  
  o.beginFunc("void", "readOutputs", std::vector<std::string>(1, "ArgListT argv"));
  {
    int a=firstOutput;
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->readFromFileCode(o,"argv["+jalib::XToString(a++)+"].c_str()");
    }
  }
  o.endFunc();

  o.beginFunc("void", "reallocate", std::vector<std::string>(1,"IndexT _size_inputs"));
  {
    FreeVars t;
    for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
      if(i->hasFlag(ConfigItem::FLAG_SIZEVAR) && !i->hasFlag(ConfigItem::FLAG_FROMCFG)){
        o.write(i->name()+" = _size_inputs;");
        t.insert(i->name());
      }
    }
    extractSizeDefines(o, t, "_size_inputs");
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      (*i)->allocateTemporary(o, true, true);
    }
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->allocateTemporary(o, true, true);
    }
  }
  o.endFunc();
  
  o.beginFunc("void", "deallocate");
  {
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      o.write((*i)->name()+" = "+(*i)->matrixTypeName()+"();");
    }
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      o.write((*i)->name()+" = "+(*i)->matrixTypeName()+"();");
    }
  }
  o.endFunc();

  o.beginFunc("void", "randomizeInputs");
  {
    if(_generator==""){
      for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
        o.write((*i)->name() + ".randomize();");
      }
    }else{
      std::vector<std::string> args;
      for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
        args.push_back((*i)->name()+".forceMutable()");
      }
      o.comment("Call generator "+_generator);
      o.write(_generator+"_main& _gen = *"+_generator+"_main::instance();");
      o.write("_gen.reallocate("TRANSFORM_N_STR"());");
      o.write("_gen.randomize();");
      o.call("_gen.setOutputs", args);
      o.write("_gen.compute();");
    }
  }
  o.endFunc();
  
  o.beginFunc("void", "randomizeOutputs");
  {
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      o.write((*i)->name() + ".randomize();");
    }
  }
  o.endFunc();
  

  o.beginFunc("void", "writeInputs", std::vector<std::string>(1, "ArgListT argv"));
  {
    int a=firstInput;
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      (*i)->writeToFileCode(o,"argv["+jalib::XToString(a++)+"].c_str()");
    }
  }
  o.endFunc();

  o.beginFunc("void", "writeOutputs", std::vector<std::string>(1, "ArgListT argv"));
  {
    int a=firstOutput;
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->writeToFileCode(o,"argv["+jalib::XToString(a++)+"].c_str()");
    }
  }
  o.endFunc();
  
  
  std::vector<std::string> outputArgTypes;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    outputArgTypes.push_back("const "+(*i)->matrixTypeName()+"& _"+(*i)->name());
  }
  o.beginFunc("void", "setOutputs", outputArgTypes);
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    std::string n = (*i)->name();
    o.write("this->"+n+" = _"+n+";");
  }
  o.endFunc();
  

  o.beginFunc("void", "compute");
  o.setcall("DynamicTaskPtr p",name()+TX_DYNAMIC_POSTFIX, argNames);
  o.write("petabricks::enqueue_and_wait(p);");
  o.endFunc();
  
  o.beginFunc("const char*", "name");
  o.write("return \""+_name+"\";");
  o.endFunc();

  o.staticMember();
  o.beginFunc(_name+"_main*", "instance");
  o.write("static "+_name+"_main i;");
  o.write("return &i;");
  o.endFunc();
  
  o.beginFunc("petabricks::TunableListT", "accuracyVariables", std::vector<std::string>(1,"int _size"));
  o.write("TunableListT _tl;");
  o.write("int _bin = petabricks::size_to_bin(_size);");
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_ACCURACY)){
      if(i->hasFlag(ConfigItem::FLAG_SIZESPECIFIC)){
        o.write("_tl.push_back(& TRANSFORM_LOCAL("+i->name()+")[_bin]);");
      }else{
        o.write("_tl.push_back(& TRANSFORM_LOCAL("+i->name()+"));");
      }
    }
  }
  o.write("return _tl;");
  o.endFunc();
  
  o.beginFunc("ElementT", "accuracy");
  if(_accuracyMetric != "")
  {
    o.write("MatrixRegion0D _acc = MatrixRegion0D::allocate();");
    std::vector<std::string> args = argnames();
    args.insert(args.begin(), "_acc");
    o.setcall("DynamicTaskPtr p", _accuracyMetric+TX_DYNAMIC_POSTFIX, args);
    o.write("petabricks::enqueue_and_wait(p);");
    if(isAccuracyInverted())
      o.write("return -1*_acc.cell();");
    else
      o.write("return _acc.cell();");
  }else{
    o.write("return DEFAULT_ACCURACY;");
  }
  o.endFunc();
  
  o.beginFunc("ElementT", "accuracyTarget");
  if(!_accuracyBins.empty()){
    o.write("return ACCURACY_TARGET;");
  }else{
    o.write("return DEFAULT_ACCURACY;");
  }
  o.endFunc();
  
  o.beginFunc("void", "hash", std::vector<std::string>(1,"jalib::HashGenerator& _hashgen"));
  {
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      o.write((*i)->name() + ".hash(_hashgen);");
    }
  }
  o.endFunc();

  o.beginFunc("petabricks::PetabricksRuntime::Main*", "nextTemplateMain");
  o.write("return "+nextMain+";");
  o.endFunc();
  o.endClass();
  
  if(!_accuracyBins.empty()){
    o.beginFunc("ElementT", _name+ "_"ACCTARGET_STR);
    o.write("return "+jalib::XToString(_curAccTarget)+";");
    o.endFunc();
  }

}

std::vector<std::string> petabricks::Transform::maximalArgList() const{
  SRCPOSSCOPE();
  std::vector<std::string> tmp; 
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->argDeclRO(tmp);
  }  
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    (*i)->argDeclRW(tmp);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->argDeclRW(tmp);
  }
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->shouldPass())
      tmp.push_back("const IndexT " + i->name());
  }
  return tmp;
}
  
std::map<std::string, petabricks::TransformPtr> petabricks::Transform::theTransformMap(){
  static std::map<std::string, petabricks::TransformPtr> m;
  return m;
}

