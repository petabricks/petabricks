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
  o << "transform " << _name;
  if(!_from.empty()){ 
    o << "\nfrom ";   printStlList(o, _from.begin(),    _from.end(), ", ");
  }
  if(!_through.empty()){ 
    o << "\nthrough"; printStlList(o, _through.begin(), _through.end(), ", ");
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

  jalib::Map(&Rule::initialize,      *this, _rules);

  const RuleSet allRules(_rules.begin(), _rules.end());

  for(MatrixDefList::iterator m=_to.begin(); m!=_to.end(); ++m)
    fillBaseCases(allRules, *m);
  for(MatrixDefList::iterator m=_through.begin(); m!=_through.end(); ++m)
    fillBaseCases(allRules, *m);

  tester().setIOSizes(_from.size(), _to.size());

  MaximaWrapper::instance().popContext();
}

void hecura::Transform::fillBaseCases(const RuleSet& allRules, const MatrixDefPtr& matrix) {
  RuleDescriptorListList boundaries;
  boundaries.resize( _to.front()->numDimensions() );
  for(size_t d=0; d<boundaries.size(); ++d){
    for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
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
  ChoiceGridPtr tmp = ChoiceGrid::constructFrom(allRules, boundaries);
  tmp->buildIndex(_baseCases[matrix]);
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
 
  #ifdef DEBUG 
  {
    std::string schedulerGraph = scheduler.toString();
    std::cerr << schedulerGraph;
    FILE* fd = popen("dot -Grankdir=LR -Tpdf -o schedule.pdf", "w");
    fwrite(schedulerGraph.c_str(),1,schedulerGraph.length(),fd);
    pclose(fd);
  }
  #endif

  scheduler.generateSchedule();

  o.comment("Begin output for transform " + _name);
  o.newline();
  o.comment("User rules");
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->generateDeclCodeSimple(o);
  }
  o.newline();

  o.comment("Rule trampolines");
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->generateTrampCodeSimple(o);
  }
  o.newline();

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
  
  o.comment(_name+" entry function");
  o.beginFunc("void", _name, args);
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
    (*i)->allocateTemporary(o);
  }
//   o.comment("Run computation");
  scheduler.generateCodeSimple(*this, o);
  //_baseCases->generateCodeSimple(o, SimpleRegionPtr(new SimpleRegion()));
  o.endFunc();
  o.newline();

  if(_to.size()==1){
    args.erase(args.begin());
    o.comment("Return style entry function");
    o.beginFunc(_to.front()->matrixTypeName(), _name, args);
    extractSizeDefines(o);
//     o.comment("Allocate to matrix");
    _to.front()->allocateTemporary(o);
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
  o.beginFunc("int", "main", "int argc, const char** argv");
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
    o.write("return 1;");
  }
  o.endIf();
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->readFromFileCode(o,"argv["+jalib::XToString(a++)+"]");
  }
  extractSizeDefines(o);
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->allocateTemporary(o);
  }
  o.call(_name, argNames);
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->writeToFileCode(o,"argv["+jalib::XToString(a++)+"]");
  }
  o.write("return 0;");
  o.endFunc();
}

