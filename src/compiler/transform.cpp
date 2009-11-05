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
#include "transform.h"

#include "codegenerator.h"
#include "maximawrapper.h"
#include "staticscheduler.h"
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
  , _tuneId(0)
  , _scope(RIRScope::global()->createChildLayer())
  , _usesSplitSize(false)
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
  MaximaWrapper::instance().pushContext();

  if(_accuracyBins.size()>1){
    JWARNING(_templateargs.empty())(_name).Text("variable accuracy templates not yet supported");
    _templateargs.push_back(new TemplateArg(TEMPLATE_BIN_STR, 0, _accuracyBins.size()-1));
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
  //TODO: it is possible that some subset of the rules could make a complete choice
  //      at some point we may want to detect such subsets and create multiple
  //      WhereExpansionRules for each of those subsets
  // for now, just find first allowed rule dynamically
  RulePtr t = new WhereExpansionRule(rules);
  rules.clear();
  rules.insert(t);
  _rules.push_back(t);
}

void petabricks::Transform::compile(){ 
  MaximaWrapper::instance().pushContext();
  jalib::Map(&MatrixDef::exportAssumptions, _from);
  jalib::Map(&MatrixDef::exportAssumptions, _through);
  jalib::Map(&MatrixDef::exportAssumptions, _to);


  jalib::Map(&RuleInterface::compileRuleBody, *this, *_scope, _rules);

  JASSERT(!_scheduler);

  _scheduler=new StaticScheduler(_choiceGrid);
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    _scheduler->markInputMatrix(*i);
  }
  for(RuleList::const_iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->collectDependencies(_scheduler);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    _scheduler->markOutputMatrix(*i);
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
  
std::string petabricks::Transform::tmplName(int n, CodeGenerator* o) const {
  std::string name = _name+TMPL_IMPL_PFX;
  int choice=n;
  //add #defines
  for(size_t i=0; i<_templateargs.size(); ++i){
    int val=(choice%_templateargs[i]->range()) + _templateargs[i]->min();
    choice/=_templateargs[i]->range();
    if(o!=NULL)
      o->write("#define " + _templateargs[i]->name() + " " + jalib::XToString(val));
    name += "_" + jalib::XToString(val);
  }
  JASSERT(choice==0)(choice);
  return name;
}

void petabricks::Transform::generateCode(CodeGenerator& o){ 
  if(_templateargs.empty())
    generateCodeSimple(o); //normal case
  else {
    std::string origName = _name;
    //count number of times we need to explode it
    size_t choiceCnt = tmplChoiceCount();
    JWARNING(choiceCnt<15)(choiceCnt)(_name).Text("Explosion of choices for template... are you sure???");
    //for each possible way
    for(size_t c=0; c<choiceCnt; ++c){
      std::string nextName = "NULL";
      if(c+1 < choiceCnt) nextName = tmplName(c+1)+"_main::instance()";
      _name = tmplName(c, &o);

      JTRACE("generating template version")(c);
      generateCodeSimple(o, nextName);

      //remove defines
      for(size_t i=0; i<_templateargs.size(); ++i){
        o.write("#undef " + _templateargs[i]->name());
      }

      _name = origName;
    }
    genTmplJumpTable(o, true, normalArgs(), normalArgNames());
    genTmplJumpTable(o, false, normalArgs(), normalArgNames());
    o.write("typedef "+tmplName(0)+"_main "+_name+"_main;");
  }
}
  
void petabricks::Transform::genTmplJumpTable(CodeGenerator& o,
                    bool isStatic,
                    const std::vector<std::string>& args,
                    const std::vector<std::string>& argNames)
{
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
  _usesSplitSize=false;
  _tuneId = 0;
  std::vector<std::string> args = normalArgs();
  std::vector<std::string> argNames = normalArgNames();
  std::vector<std::string> returnStyleArgs = args;
  if(_to.size()==1) returnStyleArgs.erase(returnStyleArgs.begin());

  o.cg().beginTransform(_originalName, _name);
  o.comment("Begin output for transform " + _name);
  o.newline();
  
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_FROMCFG)){
      if(i->hasFlag(ConfigItem::FLAG_SIZESPECIFIC)){
        int tmp = i->initial();
        if(tmp==i->min()) tmp--;
        o.createTunableArray(i->category()+".array", _name+"_"+i->name(), MAX_INPUT_BITS, tmp, i->min()-1, i->max());
      }else{
        o.createTunable(i->hasFlag(ConfigItem::FLAG_TUNABLE), i->category(), _name+"_"+i->name(), i->initial(), i->min(), i->max());
      }
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
  o.createTunable(true, "system.cutoff.sequential", _name + "_sequentialcutoff", 0);
  o.beginIf(TRANSFORM_N_STR "() < TRANSFORM_LOCAL(sequentialcutoff)");
  o.write("runStatic();");
  o.write("return NULL;");
  o.endIf();
  _scheduler->generateCodeDynamic(*this, o);
  o.endFunc();

  o.beginFunc("void", "runStatic");
  _scheduler->generateCodeStatic(*this, o);
  o.endFunc();
  
  o.comment("Rule trampolines");
  Map(&RuleInterface::generateTrampCodeSimple, *this, o, _rules);
  o.newline();

  declTransformNFunc(o);

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
  o.beginFunc("IndexT", TRANSFORM_N_STR);
  o.write("IndexT _rv_n=1;");
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    if(i->hasFlag(ConfigItem::FLAG_SIZEVAR))
      o.write("_rv_n = std::max<IndexT>(_rv_n, "+i->name()+");");
  }
  o.write("return _rv_n;");
  o.endFunc();
}

void petabricks::Transform::markSplitSizeUse(CodeGenerator& o){
  if(!_usesSplitSize){
    _usesSplitSize=true;
    o.createTunable(true, "system.cutoff.splitsize", _name + "_splitsize", 64, 1);
  }
}

void petabricks::Transform::extractSizeDefines(CodeGenerator& o, FreeVars fv, const char* inputsizestr){
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
    if(i->hasFlag(ConfigItem::FLAG_SIZESPECIFIC)){
      o.write(i->name()+" = petabricks::interpolate_sizespecific("
                                       "TRANSFORM_LOCAL("+i->name()+"),"
                                       +inputsizestr +" ,"+
                                       jalib::XToString(i->min())+");");
    }else{
      if(i->shouldPass() && i->hasFlag(ConfigItem::FLAG_FROMCFG)){
       o.write(i->name()+" = TRANSFORM_LOCAL("+i->name()+");");
      }
    }
  }
}

void petabricks::Transform::extractConstants(CodeGenerator& o){
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
      o.addMember("IndexT", i->name(), "1");
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
      if(c==0){
        o.beginIf("name == \""+tmplName(c)+"\"" + " || name==\""+name()+"\"");
      }else{
        o.beginIf("name == \""+tmplName(c)+"\"");
      }
      o.write("return "+n+";");
      o.endIf();
    }
  }
}
  

void petabricks::Transform::generateMainInterface(CodeGenerator& o, const std::string& nextMain){ 
  std::vector<std::string> argNames = normalArgNames();
  
  int a = 0;
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
      o.addMember("IndexT", i->name(), "1");
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

  o.beginFunc("void", "read", std::vector<std::string>(1, "ArgListT argv"));
  {
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

  o.beginFunc("void", "randomize");
  {
    if(_generator==""){
      for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
        o.write((*i)->name() + ".randomize();");
      }
      //also randomize outputs
      for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
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

  o.beginFunc("void", "write", std::vector<std::string>(1, "ArgListT argv"));
  {
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
    o.write("return jalib::maxval<ElementT>();");
  }
  o.endFunc();
  
  o.beginFunc("ElementT", "accuracyTarget");
  if(!_accuracyBins.empty())
  {
    std::ostringstream t;
    t << "double targets[] = {";
    if(isAccuracyInverted())
      printStlList(t, _accuracyBins.rbegin(), _accuracyBins.rend(), ", ");
    else
      printStlList(t, _accuracyBins.begin(), _accuracyBins.end(), ", ");
    t << "};";
    o.write(t.str());
    if(_accuracyBins.size()==1)
      o.write("return targets[0];");
    else if(isAccuracyInverted())
      o.write("return -1*targets["TEMPLATE_BIN_STR"];");
    else
      o.write("return targets["TEMPLATE_BIN_STR"];");
  }else{
    o.write("return jalib::minval<ElementT>();");
  }
  o.endFunc();

  o.beginFunc("petabricks::PetabricksRuntime::Main*", "nextTemplateMain");
  o.write("return "+nextMain+";");
  o.endFunc();

  
  o.endClass();
}

std::vector<std::string> petabricks::Transform::maximalArgList() const{
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

