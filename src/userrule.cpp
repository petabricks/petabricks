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
#include "userrule.h"
#include "jconvert.h"
#include "transform.h"
#include "staticscheduler.h"
#include "maximawrapper.h"
#include "rircompilerpass.h"
#include "iterationorders.h"
#include <algorithm>


petabricks::UserRule::UserRule(const RegionPtr& to, const RegionList& from, const FormulaList& cond)
  : _from(from)
  , _conditions(cond)
{
  _flags.isReturnStyle = true;
  _to.push_back(to);
}

petabricks::UserRule::UserRule(const RegionList& to, const RegionList& from, const FormulaList& cond)
  : _from(from)
  , _to(to)
  , _conditions(cond)
{
  _flags.isReturnStyle = false;
}

namespace{
  static const char* theOffsetVarStrs[] = {"x","y","z","d4","d5","d6","d7","d8","d9","d10"};
  std::string _getOffsetVarStr(int ruleid, int dim, const char* extra) {
    if(extra==NULL) extra="";
    JASSERT(dim>=0 && dim<(int)(sizeof(theOffsetVarStrs)/sizeof(char*)))(dim);
    std::string name = "_r" + jalib::XToString(ruleid) + "_" + theOffsetVarStrs[dim];
    return extra+name;
  }
}
  
petabricks::FormulaPtr petabricks::RuleInterface::getOffsetVar(int dim, const char* extra /*= NULL*/) const{
  return new FormulaVariable(_getOffsetVarStr(_id, dim, extra).c_str()); //cache me!
}
  
int petabricks::RuleInterface::offsetVarToDimension(const std::string& var, const char* extra /*=NULL*/) const
{
  for(size_t dim=0; dim<(sizeof(theOffsetVarStrs)/sizeof(char*)); ++dim){
    if(_getOffsetVarStr(_id, dim, extra)==var)
      return dim;
  }
  JASSERT(false)(var).Text("unknown variable name");
  return 0;
}


void petabricks::UserRule::setBody(const char* str){
  JWARNING(_bodysrc=="")(_bodysrc);
  _bodysrc=str;
  _bodysrc[_bodysrc.length()-1] = ' ';
}

void petabricks::UserRule::compileRuleBody(Transform& tx, RIRScope& scope){
  RIRBlockCopyRef bodyir = RIRBlock::parse(_bodysrc);
#ifdef DEBUG
  std::cerr << "BEFORE compileRuleBody:\n" << bodyir << std::endl;
  {
    DebugPrintPass p1;
    bodyir->accept(p1);
  }
  std::cerr << "--------------------\n";
#endif
  {
    ExpansionPass p2(tx, *this, scope.createChildLayer());
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

void petabricks::UserRule::print(std::ostream& os) const {
  _flags.print(os);
  os << "UserRule " << _id;
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
  os << "\napplicableregion " << _applicableRegion;
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

void petabricks::UserRule::initialize(Transform& trans) {
  MaximaWrapper::instance().pushContext();

  MatrixDefList extraFrom = trans.defaultVisibleInputs();
  for(MatrixDefList::const_iterator i=extraFrom.begin(); i!=extraFrom.end(); ++i){
    RegionPtr r = new Region((*i)->name().c_str(), FormulaList(), "all", FormulaList());
    r->setName((*i)->name().c_str());
    _from.push_back(r);
  }

  jalib::Map(&Region::initialize, trans, _from);
  jalib::Map(&Region::initialize, trans, _to);
  _conditions.normalize();
//   JASSERT(_to.size()==1)(_to.size())
//     .Text("Currently only one output region per rule is supported.");

  std::sort(_to.begin(), _to.end(), CmpRegionsByDimensions());

  FormulaList centerEqs = _to.front()->calculateCenter();
  FreeVars vars = centerEqs.getFreeVariables();
  const FreeVars& cv = trans.constants();
  vars.eraseAll(cv);

  for(size_t i=0; i<centerEqs.size(); ++i)
    centerEqs[i] = new FormulaEQ(getOffsetVar(i), centerEqs[i]);

  for( FreeVars::const_iterator i = vars.begin(); i!=vars.end(); ++i )
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
    if(_applicableRegion)
      _applicableRegion = _applicableRegion->intersect(ar);
    else
      _applicableRegion = ar;
  }
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    SimpleRegionPtr ar = (*i)->getApplicableRegion(*this, _definitions, false);
    if(_applicableRegion)
      _applicableRegion = _applicableRegion->intersect(ar);
    else
      _applicableRegion = ar;
  }
  
  FormulaList condtmp;
  condtmp.swap(_conditions);
  //simplify simple where clauses 
  for(FormulaList::iterator i=condtmp.begin(); i!=condtmp.end(); ++i){
    FreeVars fv;
    (*i)->getFreeVariables(fv);
    fv.eraseAll(trans.constants());
    FormulaPtr l,r;
    (*i)->explodeEquality(l,r);
    if(fv.size() == 1){
      //for a single var where we can just update the applicable region
      std::string var = *fv.begin();
      int dim =  offsetVarToDimension(var);
      FormulaPtr varf = new FormulaVariable(var);
      FormulaPtr criticalPoint = trimImpossible(MaximaWrapper::instance().solve(new FormulaEQ(l,r), var))->rhs();
      bool smaller, larger, eq;
      MAXIMA.pushContext();
      MAXIMA.assume(*i);
      smaller=MAXIMA.tryCompare(varf, "<", criticalPoint)!=MaximaWrapper::NO;
      eq=     MAXIMA.tryCompare(varf, "=", criticalPoint)!=MaximaWrapper::NO;
      larger= MAXIMA.tryCompare(varf, ">", criticalPoint)!=MaximaWrapper::NO;
      MAXIMA.popContext();
      JASSERT(smaller|eq|larger)(criticalPoint)(*i).Text("where clause is never true");

      if(!smaller){
        FormulaPtr& f = _applicableRegion->minCoord()[dim];
        if(eq)
          f=MAXIMA.max(f, criticalPoint);
        else
          f=MAXIMA.max(f, criticalPoint->plusOne());
      }
      if(!larger){
        FormulaPtr& f = _applicableRegion->maxCoord()[dim];
        if(eq)
          f=MAXIMA.min(f, criticalPoint->plusOne());
        else
          f=MAXIMA.min(f, criticalPoint);
      }
      
      JTRACE("where clause handled")(*i)(criticalPoint)(_applicableRegion);
    }else{
      //test if we can statically prove it
      int rslt = MAXIMA.is((*i)->printAsAssumption());
      if(rslt!=MaximaWrapper::YES){
        //otherwise handle it dynamically
        _conditions.push_back(*i);
      }else{
        JTRACE("where clause statically eliminated")(*i);
      }
    }
  }

  addAssumptions();

  //fill dependencies
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->collectDependencies(*this,_depends);
  }
  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->collectDependencies(*this,_provides)
;
  }

  MaximaWrapper::instance().popContext();
}

void petabricks::UserRule::getApplicableRegionDescriptors(RuleDescriptorList& output, 
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

void petabricks::UserRule::generateDeclCodeSimple(Transform& trans, CodeGenerator& o){

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
      LiftVardeclPass p3(trans,*this, o);
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

void petabricks::UserRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic){
  IterationDefinition iterdef(*this, isSingleCall());
  std::vector<std::string> taskargs = iterdef.args();
  std::vector<std::string> matrixNames = iterdef.args();
  std::vector<std::string> argsByRef = iterdef.args();
  std::vector<std::string> argnames = iterdef.argnames();

  taskargs.insert(taskargs.begin(), "const jalib::JRef<"+trans.instClassName()+"> transform");

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
    
    iterdef.genLoopBegin(o);
    generateTrampCellCodeSimple(trans, o, isStatic);
    iterdef.genLoopEnd(o);
    
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

void petabricks::UserRule::generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, bool isStatic){
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

std::vector<std::string> petabricks::UserRule::getCallArgs(Transform& trans, const SimpleRegionPtr& region){
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

//for( CoordinateFormula::const_iterator i=region->minCoord().begin()
//     ; i!=region->minCoord().end()
//     ; ++i)
//{
//  args.push_back((*i)->toString());
//}
//for( CoordinateFormula::const_iterator i=region->maxCoord().begin()
//     ; i!=region->maxCoord().end()
//     ; ++i)
//{
//  args.push_back((*i)->toString());
//}
  return region->argnames();
}

void petabricks::UserRule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  std::vector<std::string> args = getCallArgs(trans, region);
  o.call(trampcodename(trans)+TX_STATIC_POSTFIX, args);
}

void petabricks::UserRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  std::vector<std::string> args = getCallArgs(trans, region);
  args.insert(args.begin(), "this");
  o.setcall(name,"new "+trampcodename(trans)+"_task", args);
}


int petabricks::UserRule::dimensions() const {
//   return (int)_applicableRegion->dimensions();
  int m=0;
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    m=std::max(m, (int)(*i)->dimensions());
  }
  return m;
}

void petabricks::UserRule::addAssumptions() const {
  for(int i=0; i<dimensions(); ++i){
    MaximaWrapper::instance().assume(new FormulaGE(getOffsetVar(i), _applicableRegion->minCoord()[i]));
    MaximaWrapper::instance().assume(new FormulaLE(getOffsetVar(i), _applicableRegion->maxCoord()[i]));
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

void petabricks::UserRule::collectDependencies(StaticScheduler& scheduler){
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

void petabricks::UserRule::removeInvalidOrders(IterationOrderList& o){
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

std::string petabricks::UserRule::implcodename(Transform& trans) const {
  return trans.name()+"_rule" + jalib::XToString(_id-trans.ruleIdOffset());
}
std::string petabricks::UserRule::trampcodename(Transform& trans) const {
  return trans.name()+"_apply_rule" + jalib::XToString(_id-trans.ruleIdOffset());
}

