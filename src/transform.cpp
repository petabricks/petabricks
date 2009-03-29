/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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
#include "transform.h"
#include "jconvert.h"
#include "maximawrapper.h"
#include "codegenerator.h"
#include "staticscheduler.h"
#include "rirscope.h"

#include <algorithm>

namespace{
  //helper func
  template<typename T> void appendAll(T& dst, const T& src){
    dst.insert(dst.end(), src.begin(), src.end());
  }
}

void petabricks::Transform::addFrom(const MatrixDefList& l){
  appendAll(_from, l);
  for(MatrixDefList::const_iterator i=l.begin(); i!=l.end(); ++i){
    MatrixDefPtr& elmt = _matrices[(*i)->name()];
    if(elmt){
      JASSERT(false)(elmt)(*i).Text("Overlapping input/outputs not yet supported");
    }
    elmt = *i;
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
  if(!_from.empty()){ 
    o << "\nfrom ";   printStlList(o, _from.begin(),    _from.end(), ", ");
  }
  if(!_through.empty()){ 
    o << "\nthrough "; printStlList(o, _through.begin(), _through.end(), ", ");
  }
  if(!_to.empty()){ 
    o << "\nto ";     printStlList(o, _to.begin(),      _to.end(), ", ");
  }
  o << "\n{\n";
  printStlList(o, _rules);
  o << "}\n";
  o << "ChoiceGrid:\n" << _baseCases;
  o << "\n";
}

void petabricks::Transform::initialize() {
  MaximaWrapper::instance().pushContext();

  jalib::Map(&MatrixDef::initialize, *this, _from);
  jalib::Map(&MatrixDef::initialize, *this, _through);
  jalib::Map(&MatrixDef::initialize, *this, _to);

  jalib::Map(&MatrixDef::exportConstants, *this, _from);
  jalib::Map(&MatrixDef::exportConstants, *this, _through);
  jalib::Map(&MatrixDef::exportConstants, *this, _to);

  jalib::Map(&MatrixDef::exportAssumptions, _from);
  jalib::Map(&MatrixDef::exportAssumptions, _through);
  jalib::Map(&MatrixDef::exportAssumptions, _to);

  for(FreeVars::const_iterator i=_constants.begin(); i!=_constants.end(); ++i)
    MaximaWrapper::instance().declareInteger(*i);

  jalib::Map(&Rule::initialize,      *this, _rules);

  for(MatrixDefList::iterator m=_to.begin(); m!=_to.end(); ++m)
    fillBaseCases(*m);
  for(MatrixDefList::iterator m=_through.begin(); m!=_through.end(); ++m)
    fillBaseCases(*m);

  tester().setIOSizes(_from.size(), _to.size());

  if(isTemplate())
    RIRScope::global()->set(_name, RIRSymbol::SYM_TRANSFORM_TEMPLATE);
  else
    RIRScope::global()->set(_name, RIRSymbol::SYM_TRANSFORM);

  MaximaWrapper::instance().popContext();
}

void petabricks::Transform::fillBaseCases(const MatrixDefPtr& matrix) {
  RuleDescriptorListList boundaries;
  boundaries.resize( matrix->numDimensions() );
  RuleSet allowed;
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    if((*i)->canProvide(matrix))
      allowed.insert(*i);
  }

  for(size_t d=0; d<boundaries.size(); ++d){
    for(RuleSet::iterator i=allowed.begin(); i!=allowed.end(); ++i){
      (*i)->getApplicableRegionDescriptors(boundaries[d], matrix, d);
    }
    std::sort(boundaries[d].begin(), boundaries[d].end());
//     JTRACE("boundaryList")(d)(matrix);
//     #ifdef DEBUG
//     std::cerr << "\t";
//     printStlList(std::cerr, boundaries[d]);
//     std::cerr << std::endl;
//     #endif
  }
  ChoiceGridPtr tmp = ChoiceGrid::constructFrom(allowed, boundaries);
  if(matrix->numDimensions()>0){
    tmp->buildIndex(_baseCases[matrix]);
  }else{
    _baseCases[matrix][new SimpleRegion()] = tmp;
  }
}

void petabricks::Transform::compile(){ 
  MaximaWrapper::instance().pushContext();
  jalib::Map(&MatrixDef::exportAssumptions, _from);
  jalib::Map(&MatrixDef::exportAssumptions, _through);
  jalib::Map(&MatrixDef::exportAssumptions, _to);

  RIRScopePtr scope = RIRScope::global()->createChildLayer();

  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    scope->set(i->name(), RIRSymbol::SYM_CONFIG_TRANSFORM_LOCAL);
  }

  jalib::Map(&Rule::compileRuleBody, *this, *scope, _rules);

  JASSERT(!_scheduler);

  _scheduler=new StaticScheduler(_baseCases);
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
      _name = tmplName(c, &o);

      JTRACE("generating template version")(c);
      generateCodeSimple(o);

      //remove defines
      for(size_t i=0; i<_templateargs.size(); ++i){
        o.write("#undef " + _templateargs[i]->name());
      }

      _name = origName;
    }
    genTmplJumpTable(o, "void", "", normalArgs(), normalArgNames());
    genTmplJumpTable(o, "petabricks::DynamicTaskPtr", "spawn_", spawnArgs(), spawnArgNames());
  }
}
  
void petabricks::Transform::genTmplJumpTable(CodeGenerator& o,
                    const std::string& rt,
                    const std::string& prefix,
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
  o.beginFunc(rt, prefix+_name, targs);
  
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
    std::string fn = prefix+tmplName(c);
    o.beginCase(c);
    if(rt!="void") o.write("return ");
    o.call(fn, argNames);
    if(rt=="void") o.write("return;");
    o.endCase();
  }
  o.write("default: JASSERT(false);");
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


void petabricks::Transform::generateCodeSimple(CodeGenerator& o){ 
  std::vector<std::string> args = normalArgs();
  std::vector<std::string> argNames = normalArgNames();
  std::vector<std::string> returnStyleArgs = args;
  if(_to.size()==1) returnStyleArgs.erase(returnStyleArgs.begin());

  o.cg().beginTransform(_originalName, _name);
  o.comment("Begin output for transform " + _name);
  o.newline();
  
  for(ConfigItems::const_iterator i=_config.begin(); i!=_config.end(); ++i){
    o.createTunable(i->isTunable(), i->isTunable() ? "user.tunable" : "user.config", _name+"_"+i->name(), i->initial(), i->min(), i->max());
  }

  o.write("#define TRANSFORM_LOCAL(x) PB_CAT("+_name+"_, x)");
  o.newline();

  o.comment("User rules");
  Map(&Rule::generateDeclCodeSimple, *this, o, _rules);
  o.newline();

  o.beginClass(instClassName(), "petabricks::TransformInstance");
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    o.addMember((*i)->matrixTypeName(), (*i)->name());
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.addMember((*i)->constMatrixTypeName(), (*i)->name());
  }
  
  if(_scheduler->size()>1){
    o.beginFunc("bool", "useContinuation");
    o.createTunable(true, "system.unrollschedule", _name + "_unroll_schedule", 1, 0, 1);
    o.write("return "+_name + "_unroll_schedule == 0;");
    o.endFunc();
  }


  o.constructorBody("init();");
  o.beginFunc("void", "init");
  extractConstants(o);
  o.endFunc();

  o.beginFunc("DynamicTaskPtr", "runDynamic");
  o.createTunable(true, "system.seqcutoff", _name + "_sequential_cutoff", 0);
  o.beginIf(INPUT_SIZE_STR " < " + _name + "_sequential_cutoff");
  o.write("runStatic();");
  o.write("return NULL;");
  o.endIf();
  _scheduler->generateCodeDynamic(*this, o);
  o.endFunc();

  o.beginFunc("void", "runStatic");
  _scheduler->generateCodeStatic(*this, o);
  o.endFunc();
  
  o.comment("Rule trampolines");
  Map(&Rule::generateTrampCodeSimple, *this, o, _rules);
  o.newline();

  o.endClass();
  


//o.beginFunc("void", _name, args);
//argNames.push_back("DynamicTaskPtr::null()");
//o.setcall("DynamicTaskPtr "+taskname(), "spawn_"+_name, argNames);
//argNames.pop_back();
//o.write(taskname()+"->enqueue();");
//o.write(taskname()+"->waitUntilComplete();");
//o.endFunc();
//o.newline();

//if(_to.size()==1){
//  o.comment("Return style entry function");
//  o.beginFunc(_to.front()->matrixTypeName(), _name, returnStyleArgs);
//  extractSizeDefines(o);
//  _to.front()->allocateTemporary(o, false);
//  o.call(_name, argNames);
//  o.write("return "+_to.front()->name()+";");
//  o.endFunc();
//  o.newline();
//}
  generateMainInterface(o);
  o.write("#undef TRANSFORM_LOCAL");
  o.comment("End of output for "+_name);
  o.cg().endTransform(_originalName, _name);
  o.newline();
  o.newline();
}
void petabricks::Transform::markSplitSizeUse(CodeGenerator& o){
  if(!_usesSplitSize){
    _usesSplitSize=true;
    o.createTunable(true, "system.splitsize", _name + "_split_size", 64, 1);
    o.addMember("IndexT", SPLIT_CHUNK_SIZE, _name+"_split_size");
  }
}

void petabricks::Transform::extractSizeDefines(CodeGenerator& o){
  FreeVars fv;
//   o.comment("Extract matrix size parameters");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->extractDefines(fv, o);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->extractDefines(fv, o);
  }
}

void petabricks::Transform::extractConstants(CodeGenerator& o){
  o.addMember("IndexT", INPUT_SIZE_STR,       "0");
  o.addMember("IndexT", OUTPUT_SIZE_STR,      "0");
  o.addMember("IndexT", INPUT_PERIMETER_STR,  "0");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.write(INPUT_SIZE_STR " += " + (*i)->name() + ".count();");
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    o.write(OUTPUT_SIZE_STR " += " + (*i)->name() + ".count();");
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.write(INPUT_PERIMETER_STR " += " + (*i)->name() + ".perimeter();");
  }
  //int maxDims = 1;
  //maxDims = std::max<int>(maxDims, (*i)->numDimensions());
  extractSizeDefines(o);
  Map(&MatrixDef::verifyDefines, o, _from);
  Map(&MatrixDef::verifyDefines, o, _to);
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    (*i)->allocateTemporary(o, false);
  } }

void petabricks::Transform::registerMainInterface(CodeGenerator& o){
  if(_templateargs.empty())
    o.write("runtime.addTransform("+name()+"_main::instance());");
  else{
    size_t choiceCnt = tmplChoiceCount();
    for(size_t c=0; c<choiceCnt; ++c)
      o.write("runtime.addTransform("+tmplName(c)+"_main::instance());");
  }
}

void petabricks::Transform::generateMainInterface(CodeGenerator& o){ 
  std::vector<std::string> argNames = normalArgNames();
  int a = 1;
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
  std::string args[] = {"int argc", "const char** argv"};
  o.beginFunc("bool", "verifyArgs", std::vector<std::string>(args, args+2));
  {
    o.beginIf("argc!="+jalib::XToString(_to.size()+_from.size()+1));
    {
      std::ostringstream os;
      os <<"fprintf(stderr,\"USAGE: "+_name+" ";
      for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i)
        os << (*i)->name() << " ";
      for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i)
        os << (*i)->name() << " ";
      os <<"\\n\");";
      o.write(os.str());
    }
    o.endIf();
    o.write("return argc=="+jalib::XToString(_to.size()+_from.size()+1)+";");
  }
  o.endFunc();

  o.beginFunc("void", "read", std::vector<std::string>(args, args+2));
  {
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      (*i)->readFromFileCode(o,"argv["+jalib::XToString(a++)+"]");
    }
    extractSizeDefines(o);
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->allocateTemporary(o, true);
    }
  }
  o.endFunc();

  o.beginFunc("void", "randomInputs", std::vector<std::string>(1,"IndexT _size_inputs"));
  {
    for(FreeVars::const_iterator i=_constants.begin(); i!=_constants.end(); ++i){
      o.write("IndexT "+*i+" = _size_inputs;");
    }
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      (*i)->allocateTemporary(o, true);
    }
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->allocateTemporary(o, true);
    }
    for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      o.write((*i)->name() + ".storage()->randomize();");
    }
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      o.write((*i)->name() + ".storage()->randomize();");
    }
  }
  o.endFunc();

  o.beginFunc("void", "write", std::vector<std::string>(args, args+2));
  {
    for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      (*i)->writeToFileCode(o,"argv["+jalib::XToString(a++)+"]");
    }
  }
  o.endFunc();

  o.beginFunc("void", "compute", std::vector<std::string>());
  o.setcall("jalib::JRef<"+instClassName()+"> p","new "+instClassName(), argNames);
  o.write("DynamicTaskPtr t = p->runDynamic();");
  o.write("if(t){");
  o.write("  t->enqueue();");
  o.write("  t->waitUntilComplete();");
  o.write("}");
  o.endFunc();
  
  o.beginFunc("const char*", "name");
  o.write("return \""+_name+"\";");
  o.endFunc();

  o.staticMember();
  o.beginFunc(_name+"_main*", "instance");
  o.write("static "+_name+"_main i;");
  o.write("return &i;");
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
  for(FreeVars::const_iterator i=_constants.begin(); i!=_constants.end(); ++i){
    tmp.push_back("const IndexT " + *i);
  }
  return tmp;
}

