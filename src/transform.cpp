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

  o.comment("Generated by " PACKAGE " PetaBricks Compiler (pbc) v" VERSION);
  o.write("#include \"matrix.h\"");
  o.newline();
  o.comment("User rules");
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->generateDeclCodeSimple(o);
  }
  o.newline();

//   o.comment("Input/output matrix definitions");
//   for(MatrixDefMap::iterator i=_matrices.begin(); i!=_matrices.end(); ++i){
//     i->second->generateCodeSimple(o);
//   }
//   o.newline();

  o.comment("Rule trampolines");
  for(RuleList::iterator i=_rules.begin(); i!=_rules.end(); ++i){
    (*i)->generateTrampCodeSimple(o);
  }
  o.newline();

  std::vector<std::string> args;
  for(MatrixDefList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->argDeclRW(args);
  }
  for(MatrixDefList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->argDeclRO(args);
  }
  
  o.comment("Entry function");
  o.beginFunc("void", _name, args);
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    (*i)->genAllocTmpCode(o);
  }
  scheduler.generateCodeSimple(o);
  //_baseCases->generateCodeSimple(o, SimpleRegionPtr(new SimpleRegion()));
  o.endFunc(); 

  MaximaWrapper::instance().popContext();
}
