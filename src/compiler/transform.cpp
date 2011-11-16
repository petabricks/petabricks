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
      o->hos() << ("#define " + _templateargs[i]->name() + " " + jalib::XToString(val) + "\n");
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
        o.hos() << ("#undef " + _templateargs[i]->name() + "\n");
      }

      _name = origName;
    }
    _templateChoice = -1;
    for(RuleFlavor::iterator rf=RuleFlavor::begin(); rf!=RuleFlavor::end(); ++rf) {
#ifdef DISABLE_DISTRIBUTED
      if(rf==RuleFlavor::DISTRIBUTED) continue;
#endif
      if(rf==RuleFlavor::OPENCL) continue;
      genTmplJumpTable(o, rf, normalArgs(rf), normalArgNames());
    }
    o.hos() << "typedef "+tmplName(0)+"_main "+_name+"_main;\n";
  }
}

void petabricks::Transform::genTmplJumpTable(CodeGenerator& o,
                    RuleFlavor rf,
                    const std::vector<std::string>& args,
                    const std::vector<std::string>& argNames)
{
  SRCPOSSCOPE();
  std::ostringstream formula;
  std::stringstream ss;
  std::vector<std::string> targs;
  std::vector<std::string> targnames;
  targs.push_back("petabricks::DynamicTaskPtr _completion");
  targnames.push_back("_completion");
  for(size_t i=0, mult=1; i<_templateargs.size(); ++i){
    targs.push_back("int "+_templateargs[i]->name());
    if(mult>1) formula << " + " << mult << "*";
    formula << '(' << _templateargs[i]->name() << '-' << _templateargs[i]->min() << ')';
    mult *= _templateargs[i]->range();
  }
  targs.insert(targs.end(), args.begin(), args.end());
  targnames.insert(targnames.end(), argNames.begin(), argNames.end());
  o.beginFunc( "void" , _name+"_"+rf.str(), targs);
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
    o.call(fn+"_"+rf.str(), targnames);
    o.endCase();
  }
  o.write("default: JASSERT(false);");
  o.endSwitch();
  o.endFunc();
}


std::vector<std::string> petabricks::Transform::normalArgs(RuleFlavor rf) const{
  std::vector<std::string> args;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->argDecl(args, rf, false, true);
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->argDecl(args, rf, true, true);
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

std::vector<std::string> petabricks::Transform::spawnArgs(RuleFlavor rf) const{
  std::vector<std::string> args = normalArgs(rf);
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

#ifndef SINGLE_SEQ_CUTOFF
  o.createTunable(true, "system.cutoff.sequential", _name + "_sequentialcutoff", 64);
  o.createTunable(true, "system.cutoff.distributed", _name + "_distributedcutoff", 512);
#endif

  _scheduler->generateGlobalCode(*this, o);

  for(RuleFlavor::iterator rf=RuleFlavor::begin(); rf!=RuleFlavor::end(); ++rf) {
#ifdef DISABLE_DISTRIBUTED
      if(rf==RuleFlavor::DISTRIBUTED) continue;
#endif
    if(rf==RuleFlavor::OPENCL) continue;
    generateTransformInstanceClass(o, rf);
  }

  generateMainInterface(o, nextMain);
  o.write("#undef TRANSFORM_LOCAL");
  o.comment("End of output for "+_name);
  o.cg().endTransform(_originalName, _name);
  o.newline();
  o.newline();
}

void petabricks::Transform::generateCrossCall(CodeGenerator& o, RuleFlavor fromflavor, RuleFlavor toflavor, bool spawn){


  std::vector<std::string> argNames = normalArgNames();

  if(fromflavor == RuleFlavor::DISTRIBUTED && toflavor < RuleFlavor::DISTRIBUTED) {
    //need to convert between data types
    std::ostringstream ss;
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      ss << "petabricks::is_data_local(" << (*i)->name() << ") && ";
    }
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      ss << "petabricks::is_data_local(" << (*i)->name() << ") && ";
    }
    ss << "true";
    for(size_t i=0; i!=argNames.size(); ++i){
      argNames[i] = "CONVERT_TO_LOCAL("+argNames[i]+")";
    }
    o.beginIf(ss.str());
  }


  std::string argNamesStr = jalib::JPrintable::stringStlList(argNames.begin(), argNames.end(), ", ");


  if(toflavor == RuleFlavor::SEQUENTIAL) {

    o.write(instClassName()+"_"+RuleFlavor(RuleFlavor::SEQUENTIAL).str()+"("+argNamesStr+").run();");

  }else if(toflavor == RuleFlavor::WORKSTEALING) {

    std::string wstaskstr = "new "+instClassName()+"_"+RuleFlavor(RuleFlavor::WORKSTEALING).str()+"("+argNamesStr+")";
    if(spawn){
      o.write("petabricks::spawn_hook("+wstaskstr+", _completion);");
    }else{
      o.write("petabricks::tx_call_workstealing("+wstaskstr+");");
    }

  }else if(toflavor == RuleFlavor::DISTRIBUTED) {

    std::string disttaskstr = "new "+instClassName()+"_"+RuleFlavor(RuleFlavor::DISTRIBUTED).str()+"("+argNamesStr+")";
    if(spawn){
      o.write("petabricks::spawn_hook("+disttaskstr+", _completion);");
    }else{
      o.write("petabricks::tx_call_distributed("+disttaskstr+");");
    }

  }else{
    UNIMPLEMENTED();
  }

  o.write("return;");

  if(fromflavor == RuleFlavor::DISTRIBUTED && toflavor < RuleFlavor::DISTRIBUTED) {
    o.endIf();
  }
}

void petabricks::Transform::generateTransformSelector(CodeGenerator& o, RuleFlavor rf, bool spawn){
#ifndef SINGLE_SEQ_CUTOFF
  static const std::string seqco  = "TRANSFORM_LOCAL(sequentialcutoff)";
  static const std::string distco = "TRANSFORM_LOCAL(distributedcutoff)";
#else
  static const std::string seqco  = "sequentialcutoff";
  static const std::string distco = "distributedcutoff";
#endif

#ifdef REGIONMATRIX_TEST
  const bool force_distrib = (rf==RuleFlavor::DISTRIBUTED);
#else
  const bool force_distrib = false;
#endif

  std::vector<std::string> args = normalArgs(rf);
  if(spawn){
    args.insert(args.begin(), "const DynamicTaskPtr& _completion");
  }

  std::vector<std::string> argNames = normalArgNames();
  std::string argNamesStr = jalib::JPrintable::stringStlList(argNames.begin(), argNames.end(), ", ");

  o.beginFunc("void", _name+"_"+rf.str(), args);

  if(!force_distrib) {
    if(rf > RuleFlavor::SEQUENTIAL) {
      declTransformNDirect(o, "_transform_n");

      o.beginIf("_transform_n < " + seqco);
      o.comment("switch to sequential version");
    }
    generateCrossCall(o, rf, RuleFlavor::SEQUENTIAL, spawn);
    if(rf > RuleFlavor::SEQUENTIAL) {
      o.endIf();
    }

    if(rf > RuleFlavor::WORKSTEALING) {
      o.beginIf("_transform_n < " + distco);
      o.comment("switch to shared memory version");
    }

    if(rf >= RuleFlavor::WORKSTEALING) {
      generateCrossCall(o, rf, RuleFlavor::WORKSTEALING, spawn);
    }
    if(rf > RuleFlavor::WORKSTEALING) {
      o.endIf();
    }
  }

  if(rf == RuleFlavor::DISTRIBUTED) {
    generateCrossCall(o, rf, RuleFlavor::DISTRIBUTED, spawn);
  }

  o.endFunc();
}

void petabricks::Transform::generateTransformInstanceClass(CodeGenerator& o, RuleFlavor rf){


  //TODO: readd this optimization
  //o.comment("Entry point that does the work inline");
  //generateTransformSelector(o, rf, false);

  o.comment("Entry point that *may* spawn a task");
  generateTransformSelector(o, rf, true);

  //allow rules to put code outside of class
  Map(&RuleInterface::generateDeclCode, *this, o, rf, _rules);

  o.beginClass(instClassName() + "_" + rf.str(), std::string() + "petabricks::TransformInstance_" + rf.str());

  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    o.addMember((*i)->typeName(rf), (*i)->name());
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.addMember((*i)->typeName(rf, true), (*i)->name());
  }

  if(_scheduler->size()>1 && rf != RuleFlavor::SEQUENTIAL){
    o.beginFunc("bool", "useContinuation");
    o.createTunable(true, "system.flag.unrollschedule", _name + "_unrollschedule", 1, 0, 1);
    o.write("return "+_name + "_unrollschedule == 0;");
    o.endFunc();
  }

  o.constructorBody("init();");
  o.beginFunc("void", "init");
  extractConstants(o, rf);
  o.endFunc();

/*#ifdef HAVE_OPENCL
  std::vector<std::string> empty;
  o.beginFunc("void", "releaseGpuObjects", empty, true);
  for(RuleList::iterator i = _rules.begin(); i != _rules.end(); ++i)
  {
    if((*i)->isEnabledGpuRule()){
      std::string id = jalib::XToString((*i)->getAssociatedId());
      o.beginIf("clkern_"+id+" != 0");
      o.write("clReleaseKernel(clkern_"+id+");");
      o.write("clReleaseProgram(clprog_"+id+");");
      o.endIf();
    }
  }
  o.endFunc();
#endif*/

  if(rf == RuleFlavor::SEQUENTIAL) {
    o.beginFunc("void", "run");
    if(_memoized){
      o.beginIf("tryMemoize()");
      o.write("return;");
      o.endIf();
    }
    _scheduler->generateCode(*this, o, rf);
    o.endFunc();
  }else{
    o.beginFunc("DynamicTaskPtr", "run");
    if(_memoized){
      o.beginIf("tryMemoize()");
      o.write("return NULL;");
      o.endIf();
    }
    if (rf == RuleFlavor::DISTRIBUTED) {
      o.write("if (_sender) { migrateRegions(*_sender); }");
    }

    _scheduler->generateCode(*this, o, rf);
    o.endFunc();
  }

  Map(&RuleInterface::generateTrampCode, *this, o, rf, _rules);

  declTransformNFunc(o);

  if(_memoized){
    declTryMemoizeFunc(o);
  }

  o.mergehelpers();


  if(rf==RuleFlavor::DISTRIBUTED){
    o.generateMigrationFunctions();
  }
  o.endClass();
}

void petabricks::Transform::declTransformNFunc(CodeGenerator& o){
  SRCPOSSCOPE();
  o.beginFunc("IndexT", TRANSFORM_N_STR);
  declTransformN(o, "_rv_n");
  o.write("return _rv_n;");
  o.endFunc();
}
void petabricks::Transform::declTransformN(CodeGenerator& o, const std::string& name){
  o.write("IndexT "+name+"=1;");
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_SIZEVAR))
      o.write(name+" = std::max<IndexT>("+name+", "+i->name()+");");
  }
}
void petabricks::Transform::declTransformNDirect(CodeGenerator& o, const std::string& name){
  o.write("IndexT "+name+"=1;");
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    for(size_t d=0; d<(*i)->numDimensions(); ++d) {
      o.write(name+" = std::max<IndexT>("+name+", "+(*i)->name()+".size("+jalib::XToString(d)+"));");
    }
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    for(size_t d=0; d<(*i)->numDimensions(); ++d) {
      o.write(name+" = std::max<IndexT>("+name+", "+(*i)->name()+".size("+jalib::XToString(d)+"));");
    }
  }
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
//TODO: don't need this
void petabricks::Transform::extractOpenClSizeDefines(CLCodeGenerator& o, unsigned int dims, std::map<std::string, std::string> &map){
  FreeVars fv;
  SRCPOSSCOPE();
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->extractCLDefines(fv, o, dims, map);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->extractCLDefines(fv, o, dims, map);
  }
}
#endif

void petabricks::Transform::extractConstants(CodeGenerator& o, RuleFlavor rf){
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
    (*i)->allocateTemporary(o, rf, false, false);
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

void petabricks::Transform::generateInitCleanup(CodeGenerator& init, CodeGenerator& /*cleanup*/){
  SRCPOSSCOPE();

  std::vector<std::string>::const_iterator i;
  for(i=_initCalls.begin(); i!=_initCalls.end(); ++i) {
    init.call(*i, "");
  }

}

void petabricks::Transform::generateMainInterface(CodeGenerator& o, const std::string& nextMain){
  SRCPOSSCOPE();

  //The flavor for inputs generated from main
#ifdef DISABLE_DISTRIBUTED
  RuleFlavor rf = RuleFlavor::WORKSTEALING;
#else
  RuleFlavor rf = RuleFlavor::DISTRIBUTED;
#endif

  std::vector<std::string> argNames = normalArgNames();

  o.beginClass(_name+"_main", "petabricks::PetabricksRuntime::Main");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->varDeclCode(o, rf, true);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->varDeclCode(o, rf, false);
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
      (*i)->readFromFileCode(o,"argv["+jalib::XToString(a++)+"].c_str()", rf);
    }
    FreeVars t;
    t.insertAll(_parameters);
    extractSizeDefines(o, t, TRANSFORM_N_STR"()");
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->allocateTemporary(o, rf, true, false);
    }
  }
  o.endFunc();

  o.beginFunc("void", "readOutputs", std::vector<std::string>(1, "ArgListT argv"));
  {
    int a=firstOutput;
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->readFromFileCode(o,"argv["+jalib::XToString(a++)+"].c_str()", rf);
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
      (*i)->allocateTemporary(o, rf, true, true);
    }
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->allocateTemporary(o, rf, true, true);
    }
  }
  o.endFunc();

  o.beginFunc("void", "deallocate");
  {
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      o.write((*i)->name()+" = "+(*i)->typeName(rf)+"();");
    }
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      o.write((*i)->name()+" = "+(*i)->typeName(rf)+"();");
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

  /*o.beginFunc("void", "copyOutputs");
  {
    int a=firstOutput;
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      o.write((*i)->name()+".useOnCpu();");
    }
  }
  o.endFunc();*/


  std::vector<std::string> outputArgTypes;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    outputArgTypes.push_back("const "+(*i)->typeName(rf)+"& _"+(*i)->name());
  }
  o.beginFunc("void", "setOutputs", outputArgTypes);
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    std::string n = (*i)->name();
    o.write("this->"+n+" = _"+n+";");
  }
  o.endFunc();


  o.beginFunc("void", "compute");
  o.write("DynamicTaskPtr p = new NullDynamicTask();");
  argNames.insert(argNames.begin(), "p");
  o.call(name()+"_"+rf.str(), argNames);
  argNames.erase(argNames.begin());
  o.write("petabricks::enqueue_and_wait(p);");
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    o.write((*i)->name()+".useOnCpu();");
  }
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
    o.write(rf.string()+"::MatrixRegion0D _acc = "+rf.string()+"::MatrixRegion0D::allocate();");
    std::vector<std::string> args = argnames();
    args.insert(args.begin(), "_acc");
    args.insert(args.begin(), "p");
    o.write("DynamicTaskPtr p = new NullDynamicTask();");
    o.call(_accuracyMetric+"_"+rf.string(), args);
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

std::vector<std::string> petabricks::Transform::maximalArgList(RuleFlavor rf) const{
  SRCPOSSCOPE();
  std::vector<std::string> tmp;
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->argDecl(tmp, rf, true);
  }
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    (*i)->argDecl(tmp, rf, false);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->argDecl(tmp, rf, false);
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

