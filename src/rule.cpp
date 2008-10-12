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
#include "rule.h"
#include "jconvert.h"
#include "transform.h"
#include "jasm.h"
#include "staticscheduler.h"
#include "maximawrapper.h"

volatile long theNextRuleId = 0;

hecura::Rule::Rule(const RegionPtr& to, const RegionList& from, const FormulaList& cond)
  : _id(jalib::atomicAdd<1>(&theNextRuleId))
  , _from(from)
  , _conditions(cond)
{
  _to.push_back(to);
}

hecura::FormulaPtr hecura::Rule::getOffsetVar(int id, const char* extra /*= NULL*/) const{
  if(extra==NULL) extra="";
  static const char* strs[] = {"x","y","z","d4","d5","d6","d7","d8","d9","d10"};
  JASSERT(id>=0 && id<(int)(sizeof(strs)/sizeof(char*)))(id);
  std::string name = "_r" + jalib::XToString(_id) + "_" + strs[id];
  return new FormulaVariable((extra+name).c_str()); //cache me!
}

void hecura::Rule::setBody(const char* str){
  JWARNING(_body=="")(_body);
  _body=str;
  //remove last char, a '}'
  _body[_body.length()-1] = ' ';
  _body=jalib::StringTrim(_body);
}

void hecura::RuleFlags::print(std::ostream& os) const {
  if(priority != PRIORITY_DEFAULT){
    os << "priority(" << priority << ") ";
  }
  if(rotations != NOROTATE){
    os << "rotations(";
    if((ROTATE_90  & rotations)!=0) os << " 90";
    if((ROTATE_180 & rotations)!=0) os << " 180";
    if((ROTATE_270 & rotations)!=0) os << " 270";
    if((MIRROR_X   & rotations)!=0) os << " mirrorx";
    if((MIRROR_Y   & rotations)!=0) os << " mirrory";
    os << " ) ";
  }
}

void hecura::Rule::print(std::ostream& os) const {
  _flags.print(os);
  os << "rule " << _id;
  if(!_from.empty()){
    os << "\nfrom(";  printStlList(os,_from.begin(),_from.end(), ", "); os << ")"; 
  } 
  if(!_to.empty()){
    os << "\nto(";    printStlList(os,_to.begin(),_to.end(), ", "); os << ")";
  } 
  if(!_conditions.empty()){
    os << "\nwhere ";  printStlList(os,_conditions.begin(),_conditions.end(), ", "); 
  } 
  if(!_definitions.empty()){
    os << "\ndefinitions ";  printStlList(os,_definitions.begin(),_definitions.end(), ", "); 
  }
  os << "\napplicableregion " << _applicanbleRegion;
  os << "\ndepends: \n";
  for(MatrixDependencyMap::const_iterator i=_depends.begin(); i!=_depends.end(); ++i){
    os << "  " << i->first << ": " << i->second << "\n";
  }
  os << "provides: \n";
  for(MatrixDependencyMap::const_iterator i=_provides.begin(); i!=_provides.end(); ++i){
    os << "  " << i->first << ": " << i->second << "\n";
  }
  os << "{\n" << _body << "\n}\n\n";
}

void hecura::Rule::initialize(Transform& trans) {
  MaximaWrapper::instance().pushContext();

  jalib::Map(&Region::initialize, trans, _from);
  jalib::Map(&Region::initialize, trans, _to);
  _conditions.normalize();
  JASSERT(_to.size()==1)(_to.size())
    .Text("Currently only one output region per rule is supported.");

  FormulaList centerEqs = _to.front()->calculateCenter();
  std::set<std::string> vars = centerEqs.getFreeVariables();
  const FreeVars& cv = trans.constants();
  for(FreeVars::const_iterator i=cv.begin(); i!=cv.end(); ++i)
    vars.erase(*i);

  for(size_t i=0; i<centerEqs.size(); ++i)
    centerEqs[i] = new FormulaEQ(getOffsetVar(i), centerEqs[i]);

  for( std::set<std::string>::const_iterator i = vars.begin(); i!=vars.end(); ++i )
  {
    FormulaListPtr v = MaximaWrapper::instance().solve(centerEqs, *i);
    JASSERT(v->size()>0)(v)(*i).Text("failed to solve for i in v");
    _definitions.push_back( trimImpossible(v) );
  }

  JTRACE("FOO");
  printStlList(std::cerr,_definitions.begin(),_definitions.end(), ", ");

  _from.makeRelativeTo(_definitions);
  _to.makeRelativeTo(_definitions);
  _conditions.makeRelativeTo(_definitions);

  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    SimpleRegionPtr ar = (*i)->getApplicableRegion(*this, _definitions);
    if(_applicanbleRegion)
      _applicanbleRegion = _applicanbleRegion->intersect(ar);
    else
      _applicanbleRegion = ar;
  }
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    SimpleRegionPtr ar = (*i)->getApplicableRegion(*this, _definitions);
    if(_applicanbleRegion)
      _applicanbleRegion = _applicanbleRegion->intersect(ar);
    else
      _applicanbleRegion = ar;
  }

  addAssumptions();

  //fill dependencies
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->collectDependencies(*this,_depends);
  }
  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->collectDependencies(*this,_provides);
  }

  MaximaWrapper::instance().popContext();
}

void hecura::Rule::getApplicableRegionDescriptors(RuleDescriptorList& output, 
                                                  const MatrixDefPtr& matrix, 
                                                  int dimension) {
  MatrixDependencyMap::const_iterator i = _provides.find(matrix);
  if(i!=_provides.end()){
    FormulaPtr beginPos = i->second->region()->minCoord()[dimension];
    FormulaPtr endPos = i->second->region()->maxCoord()[dimension];
    endPos = MaximaWrapper::instance().normalize(endPos);
    output.push_back(RuleDescriptor(RuleDescriptor::RULE_BEGIN, this, matrix, beginPos));
    output.push_back(RuleDescriptor(RuleDescriptor::RULE_END,   this, matrix, endPos));
  }
}

hecura::FormulaPtr hecura::Rule::trimImpossible(const FormulaList& l){
  JASSERT(l.size()==1)(l).Text("trimming formulas not yet implemented");
  return l.front();
}

bool hecura::RuleDescriptor::operator< (const RuleDescriptor& that) const{
  const char* op = "<";
  if(_type==RULE_END && that._type==RULE_BEGIN) op = "<=";
  return MaximaWrapper::instance().compare(this->_formula, op , that._formula);
}

bool hecura::RuleDescriptor::isSamePosition(const FormulaPtr& that) const{
   return MaximaWrapper::instance().compare(this->_formula, "=" , that);
}

void hecura::Rule::generateDeclCodeSimple(CodeGenerator& o){
  std::string rt = "void";
  std::vector<std::string> args;
  if(isReturnStyle()){
    rt = "ElementT";
    JASSERT(_to.size()==1)(_to.size());
  }else{
    for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      args.push_back((*i)->generateSignatureCode(o,false));
    }
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    args.push_back((*i)->generateSignatureCode(o,true));
  }
  o.beginFunc(rt, implcodename(), args);
  o.write(_body);
  o.endFunc();
}

void hecura::Rule::generateTrampCodeSimple(CodeGenerator& o){
  CoordinateFormula begin;
  CoordinateFormula end;
  std::vector<std::string> args;

  std::set<MatrixDefPtr> used;
  for(MatrixDependencyMap::const_iterator i=_provides.begin(); i!=_provides.end(); ++i){
    used.insert(i->first);
    i->first->argDeclRW(args);
  }
  for(MatrixDependencyMap::const_iterator i=_depends.begin(); i!=_depends.end(); ++i){
    if(used.find(i->first)==used.end())
      i->first->argDeclRO(args);
  }

  //populate min
  for(int i=0; i<dimensions(); ++i){
    FormulaPtr tmp = getOffsetVar(i,"begin");
    begin.push_back(tmp);
    args.push_back("IndexT "+tmp->toString());
  }
  //populate max
  for(int i=0; i<dimensions(); ++i){
    FormulaPtr tmp = getOffsetVar(i,"end");
    end.push_back(tmp);
    args.push_back("IndexT "+tmp->toString());
  }

  o.beginFunc("void",trampcodename(), args);
  for(size_t i=0; i<begin.size(); ++i){
    o.beginFor(getOffsetVar(i)->toString(), begin[i], end[i]);
  }
  generateTrampCellCodeSimple(o);
  for(size_t i=0; i<begin.size(); ++i){
    o.endFor();
  }
  o.endFunc();
}

void hecura::Rule::generateTrampCellCodeSimple(CodeGenerator& o){
  std::vector<std::string> args;
  if(!isReturnStyle()){
    for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      args.push_back((*i)->generateAccessorCode(o));
    }
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    args.push_back((*i)->generateAccessorCode(o));
  }
  if(isReturnStyle()){
    o.setcall(_to.front()->generateAccessorCode(o),implcodename(), args);
  }else{
    o.call(implcodename(), args);
  }
}

void hecura::Rule::generateCallCodeSimple(CodeGenerator& o, const SimpleRegionPtr& region){
  std::vector<std::string> args;
  std::set<MatrixDefPtr> used;
  for(MatrixDependencyMap::const_iterator i=_provides.begin(); i!=_provides.end(); ++i){
    used.insert(i->first);
    args.push_back(i->first->name());
  }
  for(MatrixDependencyMap::const_iterator i=_depends.begin(); i!=_depends.end(); ++i){
    if(used.find(i->first)==used.end())
      args.push_back(i->first->name());
  }
  args.push_back(region->toString());
  o.call(trampcodename(), args);
}


int hecura::Rule::dimensions() const {
  return (int)_applicanbleRegion->dimensions();
}

void hecura::Rule::collectDependencies(MatrixDependencyMap& map) const {
  JASSERT(false);
}

void hecura::Rule::addAssumptions() const {
  for(int i=0; i<dimensions(); ++i){
    MaximaWrapper::instance().assume(new FormulaGE(getOffsetVar(i), _applicanbleRegion->minCoord()[i]));
    MaximaWrapper::instance().assume(new FormulaLE(getOffsetVar(i), _applicanbleRegion->maxCoord()[i]));
  }
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i)
    (*i)->addAssumptions();
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i)
    (*i)->addAssumptions();
  for(FormulaList::const_iterator i=_conditions.begin(); i!=_conditions.end(); ++i)
    MaximaWrapper::instance().assume(*i);
  for(FormulaList::const_iterator i=_definitions.begin(); i!=_definitions.end(); ++i)
    MaximaWrapper::instance().assume(*i);
}

void hecura::Rule::collectDependencies(StaticScheduler& scheduler){
  for( MatrixDependencyMap::const_iterator p=_provides.begin()
     ; p!=_provides.end()
     ; ++p)
  {
    ScheduleNodeSet pNode = scheduler.lookupNode(p->first, p->second->region());
    for( MatrixDependencyMap::const_iterator d=_depends.begin()
       ; d!=_depends.end()
       ; ++d)
    {
      ScheduleNodeSet dNode = scheduler.lookupNode(d->first, d->second->region());
      for(ScheduleNodeSet::iterator a=pNode.begin(); a!=pNode.end(); ++a)
        for(ScheduleNodeSet::iterator b=dNode.begin(); b!=dNode.end(); ++b)
          (*a)->addDependency(*b);
    }
  }
  //TODO collect edge/direction dependencies
}
