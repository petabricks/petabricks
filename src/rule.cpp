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
#include "rircompilerpass.h"
#include <algorithm>

petabricks::RIRBlockCopyRef parseRuleBody(const std::string& str);

jalib::AtomicT theNextRuleId = 0;



petabricks::Rule::Rule(const RegionPtr& to, const RegionList& from, const FormulaList& cond)
  : _id(jalib::atomicAdd<1>(&theNextRuleId))
  , _from(from)
  , _conditions(cond)
{
  _flags.isReturnStyle = true;
  _to.push_back(to);
}

petabricks::Rule::Rule(const RegionList& to, const RegionList& from, const FormulaList& cond)
  : _id(jalib::atomicAdd<1>(&theNextRuleId))
  , _from(from)
  , _to(to)
  , _conditions(cond)
{
  _flags.isReturnStyle = false;
}

petabricks::FormulaPtr petabricks::Rule::getOffsetVar(int id, const char* extra /*= NULL*/) const{
  if(extra==NULL) extra="";
  static const char* strs[] = {"x","y","z","d4","d5","d6","d7","d8","d9","d10"};
  JASSERT(id>=0 && id<(int)(sizeof(strs)/sizeof(char*)))(id);
  std::string name = "_r" + jalib::XToString(_id) + "_" + strs[id];
  return new FormulaVariable((extra+name).c_str()); //cache me!
}

void petabricks::Rule::setBody(const char* str){
  JWARNING(_bodysrc=="")(_bodysrc);
  _bodysrc=str;
  _bodysrc[_bodysrc.length()-1] = ' ';
}

void petabricks::Rule::compileRuleBody(Transform& tx, RIRScope& scope){
  RIRBlockCopyRef bodyir = parseRuleBody(_bodysrc);
#ifdef DEBUG
  std::cerr << "BEFORE compileRuleBody:\n" << bodyir << std::endl;
  {
    DebugPrintPass p1;
    bodyir->accept(p1);
  }
  std::cerr << "--------------------\n";
#endif
  {
    ExpansionPass p2(scope.createChildLayer());
    bodyir->accept(p2);
  }
  {
    AnalysisPass p3(*this, tx.name(), scope.createChildLayer());
    bodyir->accept(p3);
  }

  _bodyirStatic = bodyir;
 #ifdef DEBUG
  std::cerr << "--------------------\nAFTER compileRuleBody:\n" << bodyir << std::endl;
  {
    DebugPrintPass p1;
    bodyir->accept(p1);
  }
  std::cerr << "--------------------\n";
#endif
  
  _bodyirDynamic = bodyir;
}

void petabricks::RuleFlags::print(std::ostream& os) const {
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

void petabricks::Rule::print(std::ostream& os) const {
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
  //os << "SRC = {" << _bodysrc << "}\n";
  os << "BodyIR= {" << _bodyirDynamic << "}\n";
}

namespace {// file local
  struct CmpRegionsByDimensions {
    bool operator() ( const petabricks::RegionPtr& a, const petabricks::RegionPtr& b ){
      return a->dimensions() > b->dimensions();
    }
  };
}

void petabricks::Rule::initialize(Transform& trans) {
  MaximaWrapper::instance().pushContext();

  jalib::Map(&Region::initialize, trans, _from);
  jalib::Map(&Region::initialize, trans, _to);
  _conditions.normalize();
//   JASSERT(_to.size()==1)(_to.size())
//     .Text("Currently only one output region per rule is supported.");

  std::sort(_to.begin(), _to.end(), CmpRegionsByDimensions());

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

void petabricks::Rule::getApplicableRegionDescriptors(RuleDescriptorList& output, 
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

petabricks::FormulaPtr petabricks::Rule::trimImpossible(const FormulaList& l){
  JASSERT(l.size()==1)(l).Text("trimming formulas not yet implemented");
  return l.front();
}

bool petabricks::RuleDescriptor::operator< (const RuleDescriptor& that) const{
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

bool petabricks::RuleDescriptor::isSamePosition(const FormulaPtr& that) const{
   return MaximaWrapper::instance().compare(this->_formula, "=" , that);
}

void petabricks::Rule::generateDeclCodeSimple(Transform& trans, CodeGenerator& o){

  if(isRecursive()){
    o.beginClass(implcodename(trans)+TX_DYNAMIC_POSTFIX, "petabricks::RuleInstance");

    for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
      o.addMember((*i)->genTypeStr(false), (*i)->name());
    }
    for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
      o.addMember((*i)->genTypeStr(true), (*i)->name());
    }
    for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i){
      o.addMember("const IndexT", *i);
    }
    for(int i=0; i<dimensions(); ++i){
      o.addMember("const IndexT", getOffsetVar(i)->toString());
    }
    for(FormulaList::const_iterator i=_definitions.begin(); i!=_definitions.end(); ++i){
      o.addMember("const IndexT",(*i)->lhs()->toString(), (*i)->rhs()->toString());
    }
    o.addMember("DynamicTaskPtr", "_completion", "new NullDynamicTask()");

    o.define("SPAWN", "PB_SPAWN");
    o.define("CALL",  "PB_SPAWN");
    o.define("SYNC",  "PB_SYNC");
    o.define("DEFAULT_RV",  "_completion");
    o.beginFunc("petabricks::DynamicTaskPtr", "runDynamic");
    {
      LiftVardeclPass p3(o);
      _bodyirDynamic->accept(p3);
    }
    { 
      DynamicBodyPrintPass dbpp(o);
      _bodyirDynamic->accept(dbpp);
    }
    o.write("return DEFAULT_RV;");
    o.endFunc();
    o.undefineAll();

    o.endClass();
  }
  
  std::vector<std::string> args;
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    args.push_back((*i)->generateSignatureCode(false));
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    args.push_back((*i)->generateSignatureCode(true));
  }
  for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i){
    args.push_back("const IndexT "+(*i));
  }
  for(int i=0; i<dimensions(); ++i){
    args.push_back("const IndexT "+getOffsetVar(i)->toString());
  }

  //static version
  o.beginFunc("void", implcodename(trans)+TX_STATIC_POSTFIX, args);
  for(FormulaList::const_iterator i=_definitions.begin(); i!=_definitions.end(); ++i){
    o.addMember("const IndexT",(*i)->lhs()->toString(), (*i)->rhs()->toString());
  }
  o.define("SPAWN", "PB_STATIC_CALL");
  o.define("CALL",  "PB_STATIC_CALL");
  o.define("SYNC",  "PB_NOP");
  o.define("DEFAULT_RV",  "");
  o.write(_bodyirStatic->toString());
  o.undefineAll();
  o.endFunc();
}

void petabricks::Rule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic){
  CoordinateFormula begin, widths;
  CoordinateFormula end;
  std::vector<std::string> taskargs;
  std::vector<std::string> matrixNames;
  std::vector<std::string> argsByRef;
  std::vector<std::string> argnames;

  IterationOrderList order(dimensions(), IterationOrder::ANY);
  removeInvalidOrders(order);
  //TODO: call learner
//   trans.learner().makeIterationChoice(order, _matrix, _region);

  taskargs.push_back("const jalib::JRef<"+trans.instClassName()+"> transform");

  //populate begin
  for(int i=0; i<dimensions(); ++i){
    FormulaPtr tmp = getOffsetVar(i,"begin");
    begin.push_back(tmp);
    taskargs.push_back("const IndexT "+tmp->toString());
    argsByRef.push_back("const IndexT "+tmp->toString());
    argnames.push_back(tmp->toString());
  }
  //populate end
  for(int i=0; i<dimensions(); ++i){
    FormulaPtr tmp = getOffsetVar(i,"end");
    end.push_back(tmp);
    taskargs.push_back("const IndexT "+tmp->toString());
    argsByRef.push_back("const IndexT "+tmp->toString());
    argnames.push_back(tmp->toString());
  }

  if(isStatic)
    o.beginFunc("void", trampcodename(trans)+TX_STATIC_POSTFIX, argsByRef);
  else
    o.beginFunc("petabricks::DynamicTaskPtr", trampcodename(trans)+TX_DYNAMIC_POSTFIX, argsByRef);

  if(!isStatic && !isRecursive()){
    //shortcut
    o.comment("rule is not recursive, so no sense in dynamically scheduling it");
    o.call(trampcodename(trans)+TX_STATIC_POSTFIX, argnames);
    o.write("return NULL;");
  }else{
    if(!isStatic) o.write("DynamicTaskPtr _spawner = new NullDynamicTask();");
    
    for(size_t i=0; i<begin.size(); ++i){
      FormulaPtr b=begin[i];
      FormulaPtr e=end[i];
      FormulaPtr w=getSizeOfRuleIn(i);
      if(order[i]==IterationOrder::BACKWARD)
        o.beginReverseFor(getOffsetVar(i)->toString(), b, e, w);
      else
        o.beginFor(getOffsetVar(i)->toString(), b, e, w);
      //TODO, better support for making sure given range is a multiple of size
    }
    generateTrampCellCodeSimple(trans, o, isStatic);
    //TODO, loop carry deps?
    for(size_t i=0; i<begin.size(); ++i){
      o.endFor();
    }
    
    if(!isStatic) o.write("return _spawner;");
  }
  o.endFunc();

  if(!isStatic){
    TaskCodeGenerator& task = o.createTask(trampcodename(trans), taskargs, "SpatialDynamicTask", "_task");
    task.beginRunFunc();
    {
      if(isRecursive()){
        task.setcall("DynamicTaskPtr _task", "transform->"+trampcodename(trans)+TX_DYNAMIC_POSTFIX, argnames);
        task.write("return _task;");
      }else{
        task.call("transform->"+trampcodename(trans)+TX_STATIC_POSTFIX, argnames);
        task.write("return NULL;");
      }
    }
    task.endFunc();
    task.beginSizeFunc();
    FormulaPtr f = FormulaInteger::one();
    for(int i=0; i<dimensions(); ++i){
      FormulaPtr b = getOffsetVar(i,"begin");
      FormulaPtr e = getOffsetVar(i,"end");
      f=new FormulaMultiply(f, new FormulaSubtract(e, b));
    }
    task.write("return "+f->toString()+";");
    task.endFunc();

    task.beginBeginPosFunc();
    task.beginSwitch("d");
    for(int i=0; i<dimensions(); ++i){
      task.beginCase( i );
      task.write("return "+getOffsetVar(i,"begin")->toString()+";");
      task.endCase();
    }
    task.write("default: return 0;");
    task.endSwitch();
    task.endFunc();

    task.beginEndPosFunc();
    task.beginSwitch("d");
    for(int i=0; i<dimensions(); ++i){
      task.beginCase( i );
      task.write("return "+getOffsetVar(i,"end")->toString()+";");
      task.endCase();
    }
    task.write("default: return 0;");
    task.endSwitch();
    task.endFunc();

    task.beginDimensionsFunc();
    task.write("return "+jalib::XToString(dimensions())+";");
    task.endFunc();

    task.beginSpatialSplitFunc();
    {//spatialSplit(SpatialTaskList& _list, int _dim, int _thresh)
      task.write("IndexT _wdth = "+task.name()+"::endPos(_dim)-"+task.name()+"::beginPos(_dim);");
      task.beginIf("_wdth > _thresh && _thresh > 8");
      task.beginSwitch("_dim");
      for(int i=0; i<dimensions(); ++i){
        task.beginCase(i);
        task.write("{"); 
        task.incIndent();
        task.write("SpatialTaskPtr _t;");
        std::string t_begin = getOffsetVar(i,"t_begin")->toString();
        std::string t_end = getOffsetVar(i,"t_end")->toString();
        task.varDecl("IndexT "+t_begin +"="+getOffsetVar(i,"begin")->toString());
        task.varDecl("IndexT "+t_end +"="+t_begin+"+_thresh");
        std::vector<std::string> args;
        args.push_back("transform");
        for(int d=0; d<dimensions(); ++d){
          if(d==i)
            args.push_back(t_begin);
          else
            args.push_back(getOffsetVar(d,"begin")->toString());
        }
        for(int d=0; d<dimensions(); ++d){
          if(d==i)
            args.push_back("std::min("+t_end+","+getOffsetVar(i,"end")->toString()+")");
          else
            args.push_back(getOffsetVar(d,"end")->toString());
        }
        task.write("for(;"+t_begin+"<"+getOffsetVar(i,"end")->toString()+"; "+t_begin+"+=_thresh,"+t_end+"+=_thresh){");
        task.incIndent();
        task.setcall("_t", "new "+trampcodename(trans)+"_task", args);
        task.write("_list.push_back(_t);");
        task.endFor();
        task.endCase();
        task.write("}");
        task.decIndent();
      }
      task.write("default: _list.push_back(this); return;");
      task.endSwitch();

  //     task.write("JTRACE(\"spatialSplit complete\")(_dim)(_wdth)(_thresh)(_wdth/_thresh)(_list.size());");
      task.elseIf();
      task.write("_list.push_back(this);");
  //     task.write("JTRACE(\"spatialSplit disabled\")(_list.size())(_dim)(_wdth)(size());");
      task.endIf();
    }
    task.endFunc();
  }

}

void petabricks::Rule::generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic){
  std::vector<std::string> args;
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    args.push_back((*i)->generateAccessorCode());
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    args.push_back((*i)->generateAccessorCode());
  }
  for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i)
    args.push_back(*i);

  for(int i=0; i<dimensions(); ++i)
    args.push_back(getOffsetVar(i)->toString());

  if(!isStatic){
    o.setcall("jalib::JRef<"+implcodename(trans)+TX_DYNAMIC_POSTFIX+"> _rule", "new "+implcodename(trans)+TX_DYNAMIC_POSTFIX, args);
    o.write("DynamicTaskPtr _task = _rule->runDynamic();");
    o.beginIf("_task");
    o.write("_spawner->dependsOn(_task);");
    o.write("_task->enqueue();");
    o.endIf();
  }else{
    o.call(implcodename(trans)+TX_STATIC_POSTFIX, args);
  }
}

std::vector<std::string> petabricks::Rule::getCallArgs(Transform& trans, const SimpleRegionPtr& region){
  std::vector<std::string> args;
//std::set<MatrixDefPtr> used;
//for(MatrixDependencyMap::const_iterator i=_provides.begin(); i!=_provides.end(); ++i){
//  used.insert(i->first);
//  args.push_back(i->first->name());
//}
//for(MatrixDependencyMap::const_iterator i=_depends.begin(); i!=_depends.end(); ++i){
//  if(used.find(i->first)==used.end())
//    args.push_back(i->first->name());
//}

//for(FreeVars::const_iterator i=trans.constants().begin(); i!=trans.constants().end(); ++i)
//  args.push_back((*i));

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

void petabricks::Rule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  std::vector<std::string> args = getCallArgs(trans, region);
  o.call(trampcodename(trans)+TX_STATIC_POSTFIX, args);
}

void petabricks::Rule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  std::vector<std::string> args = getCallArgs(trans, region);
  args.insert(args.begin(), "this");
  o.setcall(name,"new "+trampcodename(trans)+"_task", args);
}


int petabricks::Rule::dimensions() const {
//   return (int)_applicanbleRegion->dimensions();
  int m=0;
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    m=std::max(m, (int)(*i)->dimensions());
  }
  return m;
}

void petabricks::Rule::addAssumptions() const {
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

void petabricks::Rule::collectDependencies(StaticScheduler& scheduler){
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

void petabricks::Rule::removeInvalidOrders(IterationOrderList& o){
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

std::string petabricks::Rule::implcodename(Transform& trans) const {
  return trans.name()+"_rule" + jalib::XToString(_id-trans.ruleIdOffset());
}
std::string petabricks::Rule::trampcodename(Transform& trans) const {
  return trans.name()+"_apply_rule" + jalib::XToString(_id-trans.ruleIdOffset());
}

