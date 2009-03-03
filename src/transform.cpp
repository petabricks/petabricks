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

#include <algorithm>

namespace{
  //helper func
  template<typename T> void appendAll(T& dst, const T& src){
    dst.insert(dst.end(), src.begin(), src.end());
  }
}

void hecura::Transform::addFrom(const MatrixDefList& l){
  appendAll(_from, l);
  for(MatrixDefList::const_iterator i=l.begin(); i!=l.end(); ++i){
    MatrixDefPtr& elmt = _matrices[(*i)->name()];
    if(elmt){
      JASSERT(false)(elmt)(*i).Text("Overlapping input/outputs not yet supported");
    }
    elmt = *i;
  }
}
void hecura::Transform::addThrough(const MatrixDefList& l){
  appendAll(_through, l);
  for(MatrixDefList::const_iterator i=l.begin(); i!=l.end(); ++i){
    MatrixDefPtr& elmt = _matrices[(*i)->name()];
    if(elmt){
      JASSERT(false)(elmt)(*i).Text("Overlapping input/outputs not yet supported");
    }
    elmt = *i;
  }
}
void hecura::Transform::addTo(const MatrixDefList& l){
  appendAll(_to, l);
  for(MatrixDefList::const_iterator i=l.begin(); i!=l.end(); ++i){
    MatrixDefPtr& elmt = _matrices[(*i)->name()];
    if(elmt){
      JASSERT(false)(elmt)(*i).Text("Overlapping input/outputs not yet supported");
    }
    elmt = *i;
  }
}
void hecura::Transform::setRules(const RuleList& l){
  JWARNING(_rules.size()==0)(_rules.size());
  appendAll(_rules, l);
}

void hecura::Transform::print(std::ostream& o) const {
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

void hecura::Transform::initialize() {
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

  MaximaWrapper::instance().popContext();
}

void hecura::Transform::fillBaseCases(const MatrixDefPtr& matrix) {
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

void hecura::Transform::generateCodeSimple(CodeGenerator& o){ 
  MaximaWrapper::instance().pushContext();
  jalib::Map(&MatrixDef::exportAssumptions, _from);
  jalib::Map(&MatrixDef::exportAssumptions, _through);
  jalib::Map(&MatrixDef::exportAssumptions, _to);

  StaticScheduler scheduler(_baseCases);
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    scheduler.markInputMatrix(*i);
  }
  for(RuleList::const_iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->collectDependencies(scheduler);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    scheduler.markOutputMatrix(*i);
  }

  scheduler.generateSchedule();

  std::vector<std::string> args;
  std::vector<std::string> argNames;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->argDeclRW(args);
    argNames.push_back((*i)->name());
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->argDeclRO(args);
    argNames.push_back((*i)->name());
  }
  std::vector<std::string> returnStyleArgs = args;
  if(_to.size()==1) returnStyleArgs.erase(returnStyleArgs.begin());

  o.comment("Begin output for transform " + _name);
  o.newline();
//   o.comment("Forward declarations");
//   o.declareFunc("void", _name, args);
//   if(_to.size()==1) o.declareFunc(_to.front()->matrixTypeName(), _name, returnStyleArgs);
//   o.newline();
  o.comment("User rules");
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->generateDeclCodeSimple(*this, o);
  }
  o.newline();

  o.comment("Rule trampolines");
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->generateTrampCodeSimple(*this, o);
  }
  o.newline();

  o.comment(_name+" entry function");
  args.push_back("const DynamicTaskPtr& _before");
  o.beginFunc("DynamicTaskPtr", "spawn_"+_name, args);
  args.pop_back();
  o.varDecl("IndexT " INPUT_SIZE_STR " = 0");
  o.varDecl("IndexT " OUTPUT_SIZE_STR " = 0");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.write(INPUT_SIZE_STR " += " + (*i)->name() + ".count();");
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    o.write(OUTPUT_SIZE_STR " += " + (*i)->name() + ".count();");
  }
  o.varDecl("IndexT _input_perimeter = 0");
  int maxDims = 1;
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    o.write("_input_perimeter += " + (*i)->name() + ".perimeter();");
    maxDims = std::max<int>(maxDims, (*i)->numDimensions());
  }
  o.createTunable(_name, _name + "_split_size", 64, 1);
  o.varDecl("IndexT " SPLIT_CHUNK_SIZE " = " + _name + "_split_size" );

  extractSizeDefines(o);
//   o.comment("Verify size of input/output");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->verifyDefines(o);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->verifyDefines(o);
  }
  if(!_through.empty())
//     o.comment("Allocate intermediate matrices");
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    (*i)->allocateTemporary(o, false);
  }
//   o.comment("Run computation");
  scheduler.generateCodeSimple(*this, o);
  
  o.write("return "+taskname()+";");
  o.endFunc();
  o.beginFunc("void", _name, args);
  argNames.push_back("DynamicTaskPtr::null()");
  o.setcall("DynamicTaskPtr "+taskname(), "spawn_"+_name, argNames);
  argNames.pop_back();
  o.write(taskname()+"->enqueue();");
  o.write(taskname()+"->waitUntilComplete();");
  o.endFunc();
  o.newline();

  if(_to.size()==1){
    o.comment("Return style entry function");
    o.beginFunc(_to.front()->matrixTypeName(), _name, returnStyleArgs);
    extractSizeDefines(o);
//     o.comment("Allocate to matrix");
    _to.front()->allocateTemporary(o, false);
//     o.comment("Call normal version");
    o.call(_name, argNames);
    o.write("return "+_to.front()->name()+";");
    o.endFunc();
    o.newline();
  }

  MaximaWrapper::instance().popContext();
}

void hecura::Transform::extractSizeDefines(CodeGenerator& o){
  FreeVars fv;
//   o.comment("Extract matrix size parameters");
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->extractDefines(fv, o);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->extractDefines(fv, o);
  }
}

void hecura::Transform::generateMainCode(CodeGenerator& o){ 
  std::vector<std::string> argNames;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    argNames.push_back((*i)->name());
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    argNames.push_back((*i)->name());
  }
  int a = 1;
  o.comment("Program main routine");
  std::string args[] = {"int argc", "const char** argv"};
  o.beginFunc("int", "main", std::vector<std::string>(args, args+2));
  o.write("class _mainclass : public hecura::HecuraRuntime::Main {");
  o.write("public:");
  o.incIndent();
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->varDeclCodeRO(o);
  }
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    (*i)->varDeclCodeRW(o);
  }
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->varDeclCodeRW(o);
  }
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
  o.call(_name, argNames);
  o.endFunc();

  o.decIndent();
  o.write("} mc;");
  o.write("hecura::HecuraRuntime runtime(mc);");
  o.write("return runtime.runMain(argc,argv);");
  o.endFunc();
}

std::vector<std::string> hecura::Transform::maximalArgList() const{
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
