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
#include <algorithm>

volatile long theNextRuleId = 0;

hecura::Rule::Rule(const RegionPtr& to, const RegionList& from, const FormulaList& cond)
  : _id(jalib::atomicAdd<1>(&theNextRuleId))
  , _from(from)
  , _conditions(cond)
{
  _flags.isReturnStyle = true;
  _to.push_back(to);
}

hecura::Rule::Rule(const RegionList& to, const RegionList& from, const FormulaList& cond)
  : _id(jalib::atomicAdd<1>(&theNextRuleId))
  , _from(from)
  , _to(to)
  , _conditions(cond)
{
  _flags.isReturnStyle = false;
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
//   JASSERT(_to.size()==1)(_to.size())
//     .Text("Currently only one output region per rule is supported.");

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

  _from.makeRelativeTo(_definitions);
  _to.makeRelativeTo(_definitions);
  _conditions.makeRelativeTo(_definitions);

  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    SimpleRegionPtr ar = (*i)->getApplicableRegion(*this, _definitions, true);
    if(_applicanbleRegion)
      _applicanbleRegion = _applicanbleRegion->intersect(ar);
    else
      _applicanbleRegion = ar;
  }
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    SimpleRegionPtr ar = (*i)->getApplicableRegion(*this, _definitions, false);
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
  std::string strA = this->_formula->toString();
  std::string strB = that._formula->toString(); 
  const char* op = "<";
  if(_type==RULE_END && that._type==RULE_BEGIN) op = "<=";
  
  //optimization and zero handling
  if(strA==strB) return strcmp(op,"<=")==0;
  if(strA=="0")  return true;
  if(strB=="0")  return false;

  return MaximaWrapper::instance().compare(this->_formula, op , that._formula);
}

bool hecura::RuleDescriptor::isSamePosition(const FormulaPtr& that) const{
   return MaximaWrapper::instance().compare(this->_formula, "=" , that);
}

void hecura::Rule::generateDeclCodeSimple(Transform& trans, CodeGenerator& o){
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

  for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i)
    args.push_back("const IndexT "+(*i));

  for(int i=0; i<dimensions(); ++i)
    args.push_back("const IndexT "+getOffsetVar(i)->toString());

  o.beginFunc(rt, implcodename(), args);
  for(FormulaList::const_iterator i=_definitions.begin(); i!=_definitions.end(); ++i){
    o.write("const IndexT "+(*i)->explodePrint()+";");
  }

  o.write(_body);
  o.endFunc();
}

void hecura::Rule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o){
  CoordinateFormula begin, widths;
  CoordinateFormula end;
  std::vector<std::string> args;
  std::vector<std::string> argsByRef;

  std::set<MatrixDefPtr> used;
  for(MatrixDependencyMap::const_iterator i=_provides.begin(); i!=_provides.end(); ++i){
    used.insert(i->first);
    i->first->argDeclRW(args);
    i->first->argDeclRW(argsByRef, true);
  }
  for(MatrixDependencyMap::const_iterator i=_depends.begin(); i!=_depends.end(); ++i){
    if(used.find(i->first)==used.end()){
      i->first->argDeclRO(args);
      i->first->argDeclRO(argsByRef, true);
    }
  }

  IterationOrderList order(dimensions(), IterationOrder::ANY);
  removeInvalidOrders(order);
  //TODO: call learner
//   trans.learner().makeIterationChoice(order, _matrix, _region);

  for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i){
    args.push_back("const IndexT "+(*i));
    argsByRef.push_back("const IndexT "+(*i));
  }

  //populate begin
  for(int i=0; i<dimensions(); ++i){
    FormulaPtr tmp = getOffsetVar(i,"begin");
    begin.push_back(tmp);
    args.push_back("const IndexT "+tmp->toString());
    argsByRef.push_back("const IndexT "+tmp->toString());
  }
  //populate end
  for(int i=0; i<dimensions(); ++i){
    FormulaPtr tmp = getOffsetVar(i,"end");
    end.push_back(tmp);
    args.push_back("const IndexT "+tmp->toString());
    argsByRef.push_back("const IndexT "+tmp->toString());
  }

  o.beginFunc("void",trampcodename(), argsByRef);

//   FreeVars fv;
//   for(MatrixDependencyMap::const_iterator i=_depends.begin(); i!=_depends.end(); ++i)
//     i->first->extractDefines(fv, o);
//   for(MatrixDependencyMap::const_iterator i=_provides.begin(); i!=_provides.end(); ++i)
//     i->first->extractDefines(fv, o);

  for(size_t i=0; i<begin.size(); ++i){
    FormulaPtr b=begin[i];
    FormulaPtr e=end[i];
    FormulaPtr w=_to[0]->getSizeOfRuleIn(i);
    if(order[i]==IterationOrder::BACKWARD)
      o.beginReverseFor(getOffsetVar(i)->toString(), b, e, w);
    else
      o.beginFor(getOffsetVar(i)->toString(), b, e, w);
    //TODO, better support for making sure given range is a multiple of size
  }
  generateTrampCellCodeSimple(trans, o);
  for(size_t i=0; i<begin.size(); ++i){
    o.endFor();
  }
  o.endFunc();

  TaskCodeGenerator& task = o.createTask(trampcodename(), args);
  task.beginRunFunc();
  task.call(trampcodename(), task.argnames());
  task.write("return NULL;");
  task.endFunc();
  task.beginSizeFunc();
  FormulaPtr f = FormulaInteger::zero();
  for(int i=0; i<dimensions(); ++i){
    FormulaPtr b = getOffsetVar(i,"begin");
    FormulaPtr e = getOffsetVar(i,"end");
    f=new FormulaMultiply(f, new FormulaSubtract(e, b));
  }
  task.write("return "+f->toString()+";");
  task.endFunc();
}

void hecura::Rule::generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o){
  std::vector<std::string> args;
  if(!isReturnStyle()){
    for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      args.push_back((*i)->generateAccessorCode(o));
    }
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    args.push_back((*i)->generateAccessorCode(o));
  }

  for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i)
    args.push_back(*i);

  for(int i=0; i<dimensions(); ++i)
    args.push_back(getOffsetVar(i)->toString());

  if(isReturnStyle()){
    o.setcall(_to.front()->generateAccessorCode(o),implcodename(), args);
  }else{
    o.call(implcodename(), args);
  }
}

std::vector<std::string> hecura::Rule::getCallArgs(Transform& trans, const SimpleRegionPtr& region){
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

  for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i)
    args.push_back((*i));

  for( CoordinateFormula::const_iterator i=region->minCoord().begin()
       ; i!=region->minCoord().end()
       ; ++i)
  {
    args.push_back((*i)->toString());
  }
  for( CoordinateFormula::const_iterator i=region->maxCoord().begin()
       ; i!=region->maxCoord().end()
       ; ++i)
  {
    args.push_back((*i)->toString());
  }
  return args;
}

void hecura::Rule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  std::vector<std::string> args = getCallArgs(trans, region);
  o.call(trampcodename(), args);
}

void hecura::Rule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  std::vector<std::string> args = getCallArgs(trans, region);
  o.setcall(name,"new "+trampcodename()+"_task", args);
}


int hecura::Rule::dimensions() const {
//   return (int)_applicanbleRegion->dimensions();
  int m=0;
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    m=std::max(m, (int)(*i)->dimensions());
  }
  return m;
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
          (*a)->addDependency(*b, this, d->second->direction());
    }

    //null depedency on all other output regions
    for( MatrixDependencyMap::const_iterator pp=_provides.begin()
      ; pp!=_provides.end()
      ; ++pp)
    {
      if(p!=pp){
        ScheduleNodeSet dNode = scheduler.lookupNode(pp->first, pp->second->region());
        for(ScheduleNodeSet::iterator a=pNode.begin(); a!=pNode.end(); ++a)
          for(ScheduleNodeSet::iterator b=dNode.begin(); b!=dNode.end(); ++b)
            (*a)->addDependency(*b, this, DependencyDirection(dimensions()));
      }
    }
  }
  //TODO collect edge/direction dependencies
}

void hecura::Rule::removeInvalidOrders(IterationOrderList& o){
  for( MatrixDependencyMap::const_iterator p=_provides.begin()
     ; p!=_provides.end()
     ; ++p)
  {
    MatrixDependencyMap::const_iterator d = _depends.find(p->first);
    if(d!=_depends.end()){
      const DependencyDirection& dir = d->second->direction();
      JASSERT(dir.size()==o.size());
      for(size_t i=0; i<dir.size(); ++i){
        if((dir[i]&DependencyDirection::D_GT)!=0){
          o[i] &= ~IterationOrder::FORWARD;
          JTRACE("Forward iteration not allowed")(id());
        }
        if((dir[i]&DependencyDirection::D_LT)!=0){
          o[i] &= ~IterationOrder::BACKWARD;
          JTRACE("Backward iteration not allowed")(id());
        }
      }
    }
  }
}

