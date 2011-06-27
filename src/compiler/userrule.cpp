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
//#define FORCE_OPENCL
//#define OPENCL_LOGGING

//#define TRACE(x) std::cout << "Trace " << x << "\n"

#define TRACE JTRACE

#include "userrule.h"

#include "maximawrapper.h"
#include "rircompilerpass.h"
#include "scheduler.h"
#include "transform.h"
#include "syntheticrule.h"
#include "gpurule.h"

#include "common/jconvert.h"

#include <algorithm>
#include <stdlib.h>
#include <set>


petabricks::UserRule::UserRule(const RegionPtr& to, const RegionList& from, const MatrixDefList& through, const FormulaList& cond)
  : _from(from)
  , _through(through)
  , _conditions(cond)
{
  _flags.isReturnStyle = true;
  _to.push_back(to);
}

petabricks::UserRule::UserRule(const RegionList& to, const RegionList& from, const MatrixDefList& through, const FormulaList& cond)
  : _from(from)
  , _to(to)
  , _through(through)
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
  SRCPOSSCOPE();
  for(size_t dim=0; dim<(sizeof(theOffsetVarStrs)/sizeof(char*)); ++dim){
    if(_getOffsetVarStr(_id, dim, extra)==var)
      return dim;
  }
  JASSERT(false)(var).Text("unknown variable name");
  return 0;
}


void petabricks::UserRule::setBody(const char* str, const jalib::SrcPos& p){
  JWARNING(_bodysrc=="")(_bodysrc)(p);
  _bodysrc=str;
  _bodysrc[_bodysrc.length()-1] = ' ';
  _bodysrcPos.tagPosition(p.clone());
}

void petabricks::UserRule::compileRuleBody(Transform& tx, RIRScope& parentScope){
  SRCPOSSCOPE();

  jalib::Map(&Region::validate, _from);
  jalib::Map(&Region::validate, _to);

  RIRScopePtr scope = parentScope.createChildLayer();
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    (*i)->addArgToScope(scope);
  }
  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->addArgToScope(scope);
  }
  DebugPrintPass    print;
  ExpansionPass     expand(tx, *this, scope);
  AnalysisPass      analysis(*this, tx.name(), scope);
#ifdef HAVE_OPENCL
  OpenClCleanupPass opencl(*this, scope);
  OpenClFunctionRejectPass openclfnreject(*this, scope);
  GpuRenamePass gpurename;
  bool failgpu = false;
#endif
  RIRBlockCopyRef bodyir = RIRBlock::parse(_bodysrc, &_bodysrcPos);

#ifdef DEBUG
  std::cerr << "--------------------\nBEFORE compileRuleBody:\n" << bodyir << std::endl;
  bodyir->accept(print);
  std::cerr << "--------------------\n";
#endif

  bodyir->accept(expand);
  bodyir->accept(analysis);

#ifdef DEBUG
  std::cerr << "--------------------\nEXPANDED compileRuleBody:\n" << bodyir << std::endl;
  bodyir->accept(print);
  std::cerr << "--------------------\n";
#endif

  for(RuleFlavor::iterator i=RuleFlavor::begin(); i!=RuleFlavor::end(); ++i) {
    _bodyir[i] = bodyir;
    RuleFlavorSpecializePass pass(i);
    _bodyir[i]->accept(pass);
  }

#ifdef HAVE_OPENCL
  if(isOpenClRule() && getSelfDependency().isNone()){
    try
    {
      _bodyir[RuleFlavor::OPENCL] = bodyir;
      _bodyir[RuleFlavor::OPENCL]->accept(openclfnreject);
      _bodyir[RuleFlavor::OPENCL]->accept(opencl);
      _bodyir[RuleFlavor::OPENCL]->accept(gpurename);
      std::cerr << "--------------------\nAFTER compileRuleBody:\n" << bodyir << std::endl;
      bodyir->accept(print);
      std::cerr << "--------------------\n";

      if(!passBuildGpuProgram(tx)) {
				std::cout << "(>) RULE REJECTED BY TryBuildGpuProgram: " << id() << "\n";
        failgpu = true;
      }
    }
    catch( OpenClCleanupPass::NotValidSource e )
    {
      std::cout << "(>) RULE REJECTED BY OpenClCleanupPass: " << id() << "\n";
      failgpu = true;
    }
    catch( OpenClFunctionRejectPass::NotValidSource e )
    {
      std::cout << "(>) RULE REJECTED BY OpenClFunctionRejectPass: " << id() << "\n";
      failgpu = true;
    }

  }
	else if(isOpenClRule()) {
		std::cout << "(>) RULE REJECTED BY Self Dependency \n";
		failgpu = true;
	}
  else
  {
    std::cout << "(>) NO OPENCL SUPPORT FOR RULE " << id() << "\n";
    failgpu = true;
  }

  if( failgpu )
  {
    if(_gpuRule) _gpuRule->disableRule();
    _gpuRule = NULL;
    _bodyir[RuleFlavor::OPENCL] = NULL;
  }

#endif
}

#ifdef HAVE_OPENCL
bool petabricks::UserRule::passBuildGpuProgram(Transform& trans) {
	//return false;
  TrainingDeps* tmp = new TrainingDeps();
  CLCodeGenerator clcodegen(tmp);

  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  generateOpenCLKernel( trans, clcodegen, iterdef );

  cl_int err;
  std::string s = clcodegen.os().str();
  const char *clsrc = s.c_str();

  std::cout << clsrc << std::endl;
  cl_context ctx = OpenCLUtil::getContext( );
  cl_program clprog  = clCreateProgramWithSource( ctx, 1, (const char **)&clsrc, NULL, &err );
  if(err != CL_SUCCESS)
    return false;
  err = clBuildProgram( clprog, 0, NULL, NULL, NULL, NULL);
  return (err == CL_SUCCESS);
}
#endif

void petabricks::UserRule::print(std::ostream& os) const {
  SRCPOSSCOPE();
  _flags.print(os);
  os << "UserRule " << _id << " " << _label;
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
  if(!_duplicateVars.empty()){
    os << "\nduplicateVars ";  printStlList(os,_duplicateVars.begin(),_duplicateVars.end(), ", ");
  }
  os << "\nisRecursive " << isRecursive();
  os << "\nisOpenCLRule " << isOpenClRule();
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
  os << "BodyIR= {" << _bodyir[RuleFlavor::WORKSTEALING] << "}\n";
}

namespace {// file local
  struct CmpRegionsByDimensions {
    bool operator() ( const petabricks::RegionPtr& a, const petabricks::RegionPtr& b ){
      return a->dimensions() > b->dimensions();
    }
  };
}

void petabricks::UserRule::initialize(Transform& trans) {
  SRCPOSSCOPE();
  MaximaWrapper::instance().pushContext();

  MatrixDefList extraFrom = trans.defaultVisibleInputs();
  for(MatrixDefList::const_iterator i=extraFrom.begin(); i!=extraFrom.end(); ++i){
    RegionPtr r = new Region((*i)->name().c_str(), FormulaList(), "all", FormulaList());
    r->setName((*i)->name().c_str());
    _from.push_back(r);
  }

  jalib::Map(&Region::initialize, trans, _from);
  jalib::Map(&Region::initialize, trans, _to);
  jalib::Map(&Region::assertNotInput,    _to);
  _conditions.normalize();
//   JASSERT(_to.size()==1)(_to.size())
//     .Text("Currently only one output region per rule is supported.");

  std::sort(_to.begin(), _to.end(), CmpRegionsByDimensions());

  FormulaList centerEqs = _to.front()->calculateCenter();
  FreeVars vars = centerEqs.getFreeVariables();
  vars.eraseAll(trans.constants());

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

  buildApplicableRegion(trans, _applicableRegion, true);

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

      JTRACE("where clause handled")(*i)(criticalPoint)(_applicableRegion)(smaller)(eq)(larger);
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
    (*i)->collectDependencies(trans, *this,_depends);
  }
  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->collectDependencies(trans, *this,_provides);
  }

  //expand through() clause
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    _bodysrc=(*i)->genericTypeName()+" "+(*i)->name()+" = "+(*i)->genericAllocateStr()+";\n"+_bodysrc;
  }

  MaximaWrapper::instance().popContext();
}

void petabricks::UserRule::buildApplicableRegion(Transform& trans, SimpleRegionPtr& ar, bool allowOptional){
  SRCPOSSCOPE();
  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    JASSERT(!(*i)->isOptional())((*i)->name())
      .Text("optional regions are not allowed in outputs");
    SimpleRegionPtr t = (*i)->getApplicableRegion(trans, *this, _definitions, true);
    ar = ar ? ar->intersect(t) : t;
  }
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    if(allowOptional && (*i)->isOptional())
      continue;
    SimpleRegionPtr t = (*i)->getApplicableRegion(trans, *this, _definitions, false);
    ar = ar ? ar->intersect(t) : t;
  }
  JASSERT(ar);
}

void petabricks::UserRule::performExpansion(Transform& trans){
  SRCPOSSCOPE();
  if(isDuplicated()){
    JTRACE("expanding duplicates")(duplicateCount());
    JASSERT(getDuplicateNumber()==0)(getDuplicateNumber());
    size_t dc = duplicateCount();
    for(size_t i=1; i<dc; ++i){
      trans.addRule(new DuplicateExpansionRule(this, i));
    }
  }
 #ifdef HAVE_OPENCL
  if(!hasWhereClause() && getMaxOutputDimension() > 0){
    _gpuRule = new GpuRule( this );
    trans.addRule( _gpuRule );
  }
 #endif
}

void petabricks::UserRule::getApplicableRegionDescriptors(RuleDescriptorList& output,
                                                          const MatrixDefPtr& matrix,
                                                          int dimension,
                                                          const RulePtr& rule
                                                          ) {
  SRCPOSSCOPE();
  MatrixDependencyMap::const_iterator i = _provides.find(matrix);
  if(i!=_provides.end()){
    FormulaPtr beginPos = i->second->region()->minCoord()[dimension];
    FormulaPtr endPos = i->second->region()->maxCoord()[dimension];
    endPos = MaximaWrapper::instance().normalize(endPos);
    output.push_back(RuleDescriptor(RuleDescriptor::RULE_BEGIN, rule, matrix, beginPos));
    output.push_back(RuleDescriptor(RuleDescriptor::RULE_END,   rule, matrix, endPos));
  }
}

void petabricks::UserRule::generateDeclCode(Transform& trans, CodeGenerator& o, RuleFlavor rf){
  /* code generated here goes at the top level of the file before the instance class */
  SRCPOSSCOPE();
  if(rf == RuleFlavor::SEQUENTIAL) {
    generateDeclCodeSequential(trans, o);
    return;
  }
#ifdef HAVE_OPENCL
  if(rf == RuleFlavor::OPENCL) {
    generateDeclCodeOpenCl(trans, o);
    return;
  }
#endif
  if(rf == RuleFlavor::WORKSTEALING && !isRecursive()) {
    //don't generate workstealing code for leaf nodes
    return;
  }

  std::string classname = implcodename(trans)+"_"+ rf.str();
  o.beginClass(classname, "petabricks::RuleInstance");

  std::vector<std::string> to_cells;
  std::vector<std::string> from_cells;

  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    if ((rf == RuleFlavor::DISTRIBUTED) &&
        ((*i)->getRegionType() == Region::REGION_CELL)) {
      // will read CellProxy to ElementT in before running the task
      o.addMember((*i)->genTypeStr(rf, false), "_cellproxy_" + (*i)->name());
      o.addMember("ElementT", (*i)->name(), "0");
      to_cells.push_back((*i)->name());
    } else {
      o.addMember((*i)->genTypeStr(rf, false), (*i)->name());
    }
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    if ((rf == RuleFlavor::DISTRIBUTED) &&
        ((*i)->getRegionType() == Region::REGION_CELL)) {
      // will read CellProxy to const ElementT in before running the task
      o.addMember((*i)->genTypeStr(rf, true), "_cellproxy_" + (*i)->name());
      o.addMember("const ElementT", (*i)->name(), "0");
      from_cells.push_back((*i)->name());
    } else {
      o.addMember((*i)->genTypeStr(rf, true), (*i)->name());
    }
  }
  for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
    if(i->shouldPass()){
      o.addMember("const "+i->passType(), i->name());
    }
  }
  for(ConfigItems::const_iterator i=_duplicateVars.begin(); i!=_duplicateVars.end(); ++i){
      o.addMember("const IndexT", i->name());
  }
  for(int i=0; i<dimensions(); ++i){
    o.addMember("const IndexT", getOffsetVar(i)->toString());
  }
  for(FormulaList::const_iterator i=_definitions.begin(); i!=_definitions.end(); ++i){
    o.addMember("const IndexT",(*i)->lhs()->toString(), (*i)->rhs()->toString());
  }

  o.addMember("DynamicTaskPtr", "_completion", "new NullDynamicTask()");

  if (rf == RuleFlavor::DISTRIBUTED) {
    o.addMember("DynamicTaskPtr", "_cleanUpTask", "new MethodCallTask<"+classname+", &"+classname+"::cleanUp>(this)");
  }

  o.define("PB_FLAVOR", rf.str());
  o.define("SPAWN", "PB_SPAWN");
  o.define("CALL",  "PB_SPAWN");
  o.define("SYNC",  "PB_SYNC");

  if (rf == RuleFlavor::DISTRIBUTED) {
    o.define("DEFAULT_RV", "_cleanUpTask");
    o.define("RETURN", "PB_RETURN_DISTRIBUTED");
    o.define("RETURN_VOID", "PB_RETURN_VOID_DISTRIBUTED");
  } else {
    o.define("DEFAULT_RV", "_completion");
    o.define("RETURN", "PB_RETURN");
    o.define("RETURN_VOID", "PB_RETURN_VOID");
  }
  o.beginFunc("petabricks::DynamicTaskPtr", "runDynamic");

  if (rf == RuleFlavor::DISTRIBUTED) {
    o.comment("read all cellproxy to ElementT");
    for (unsigned int i = 0; i < to_cells.size(); i++) {
      o.write(to_cells[i] + " = _cellproxy_" + to_cells[i] + ";");
    }
    for (unsigned int i = 0; i < from_cells.size(); i++) {
      o.write("const_cast<ElementT&> (" + from_cells[i] + ") = _cellproxy_" + from_cells[i] + ";");
    }
  }

  RIRBlockCopyRef bodytmp = _bodyir[rf];
  o.beginUserCode(rf);
  {
    LiftVardeclPass p3(trans,*this, o);
    bodytmp->accept(p3);
  }
  {
    DynamicBodyPrintPass dbpp(o);
    bodytmp->accept(dbpp);
  }

  if (rf == RuleFlavor::DISTRIBUTED) {
    o.write("_cleanUpTask->dependsOn(_completion);");
    o.write("_completion->enqueue();");
  }
  o.write("return DEFAULT_RV;");
  o.endUserCode();
  o.endFunc();

  if (rf == RuleFlavor::DISTRIBUTED) {
    o.beginFunc("petabricks::DynamicTaskPtr", "cleanUp");
    o.comment("write _to back to cellproxy");
    for (unsigned int i = 0; i < to_cells.size(); i++) {
      o.write("_cellproxy_" + to_cells[i] + " = " + to_cells[i] + ";");
    }
    o.write("return NULL;");
    o.endFunc();
  }

  o.undefineAll();

  o.endClass();
}

void petabricks::UserRule::generateDeclCodeSequential(Transform& trans, CodeGenerator& o) {
  RuleFlavor rf = RuleFlavor::SEQUENTIAL;

  /* code generated here goes at the top level of the file before the instance class */
  std::vector<std::string> args;
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    args.push_back((*i)->generateSignatureCode(rf, false));
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    args.push_back((*i)->generateSignatureCode(rf, true));
  }
  for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
    if(i->shouldPass()){
      args.push_back("const "+i->passType()+" "+i->name());
    }
  }
  for(ConfigItems::const_iterator i=_duplicateVars.begin(); i!=_duplicateVars.end(); ++i){
    args.push_back("const IndexT "+i->name());
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
  o.define("RETURN", "PB_RETURN");
  o.define("RETURN_VOID", "PB_RETURN_VOID");
  o.beginUserCode(rf);
  o.write(_bodyir[RuleFlavor::SEQUENTIAL]->toString());
  o.endUserCode();
  o.undefineAll();
  o.endFunc();
}

void petabricks::UserRule::generateDeclCodeOpenCl(Transform& /*trans*/, CodeGenerator& /*o*/) {
  /* code generated here goes at the top level of the file before the instance class */
}

void petabricks::UserRule::generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor){
  SRCPOSSCOPE();
  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  std::vector<std::string> taskargs = iterdef.packedargs();
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::vector<std::string> packedargnames = iterdef.packedargnames();

  taskargs.insert(taskargs.begin(), "const jalib::JRef<"+trans.instClassName()+"> transform");

  o.beginFunc("petabricks::DynamicTaskPtr", trampcodename(trans)+"_"+flavor.str(), packedargs);

  for(size_t i=0; i<_duplicateVars.size(); ++i){
    o.varDecl("const IndexT "+_duplicateVars[i].name() + " = " + jalib::XToString(_duplicateVars[i].initial()));
  }

  #ifdef FORCE_OPENCL
  // TEMPORARY -- hard-wire things so that the OpenCL rule is always called
  if( ( RuleFlavor::SEQUENTIAL == flavor ) && isOpenClRule( ) )
    {
      o.comment("Forcing OpenCL impl of rule...");
      o.write("return ");
      o.call(trampcodename(trans)+TX_OPENCL_POSTFIX, packedargnames);
      o.write("}");
      o.comment("END Forcing OpenCL impl of rule...");
      return;
    }
  // END TEMPORARY
  #endif

  // LOGGING
  #ifdef OPENCL_LOGGING
  if( RuleFlavor::SEQUENTIAL == flavor )
  {
    o.write( "printf( \"ruleN_static applied from (%d,%d) to (%d,%d)\\n\", _iter_begin[0], _iter_begin[1], _iter_end[0], _iter_end[1] );\n" );
  }
  #ifdef HAVE_OPENCL
  else if( RuleFlavor::OPENCL == flavor )
  {
    o.write( "printf( \"ruleN_opencl applied from (%d,%d) to (%d,%d)\\n\", _iter_begin[0], _iter_begin[1], _iter_end[0], _iter_end[1] );\n" );
  }
  #endif
  #endif
  // END LOGGING

  #ifdef HAVE_OPENCL
  if( RuleFlavor::OPENCL == flavor )
  {
    o.os() << "cl_int err;\n";
    o.os() << "cl_kernel clkern = " << trans.name() << "_instance::get_kernel_" << id() << "();\n";

    int arg_pos = 0;

    //o.os( ) << "printf( \"- TRACE 10\\n\" );\n";

    o.os( ) << "std::cerr << \"Prepare GPU\" << std::endl;\n";
		o.os( ) << "if( ";
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      if(i != _to.begin( )) {
        o.os() << "|| ";
      }
      o.os( ) << (*i)->matrix( )->name( ) << ".bytes() == 0";
    }
    o.os( ) << ") {\n";
    o.os( ) << "return NULL;\n";
    o.os( ) << "}\n";


    std::set<std::string> set;

    o.comment( "Create memory objects for outputs." );
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      std::string matrix_name = (*i)->matrix( )->name( );
      std::set<std::string>::iterator matrix_it = set.find(matrix_name);
      if(matrix_it == set.end()) {
        set.insert(matrix_name);
        (*i)->setBuffer(true);
        //o.os( ) << "std::cerr << \"BEFORE EVERYTHING\" << std::endl;\n";
        //o.os( ) << "MatrixIO(\"/dev/stderr\",\"w\").write(" << (*i)->matrix( )->name( ) << ");\n";

        o.os( ) << "MatrixRegion<" << (*i)->dimensions() << ", " STRINGIFY(MATRIX_ELEMENT_T) "> normalized_" << (*i)->name( )
                << " = " << matrix_name << ".asGpuBuffer(_iter_begin, _iter_end);\n";
        o.os( ) << "cl_mem devicebuf_" << (*i)->name( )
                << " = clCreateBuffer( OpenCLUtil::getContext( ), CL_MEM_WRITE_ONLY, "
                << "normalized_" << (*i)->name( ) << ".bytes( ),"
                << "(void*) normalized_" << (*i)->name( ) << ".base( ), &err );\n";
        //o.os( ) << "std::cerr << \"" << (*i)->matrix( )->name( ) << "\" << std::endl;\n";
        //o.os( ) << "std::cerr << normalized_" << (*i)->name( ) << ".bytes( ) << std::endl;\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create output memory object\");\n";

        // Bind to kernel.
        o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(cl_mem), (void*)&devicebuf_" << (*i)->name( ) << " );\n\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to bind kernel arguments.\" );\n\n";
      }
      else {
        (*i)->setBuffer(false);
      }
    }
    //o.os( ) << "std::cout << std::endl;\n";

    // Create memory objects for inputs.
    o.comment( "Create memory objects for inputs." );
    set.clear();
    for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
    {
      std::string matrix_name = (*i)->matrix( )->name( );
      std::set<std::string>::iterator matrix_it = set.find(matrix_name);
      if(matrix_it == set.end()) {
        set.insert(matrix_name);
        (*i)->setBuffer(true);
        //o.os( ) << "std::cerr << \"INPUT\" << std::endl;\n";
        //o.os( ) << "MatrixIO(\"/dev/stderr\",\"w\").write(" << (*i)->matrix( )->name( ) << ");\n";

        o.os( ) << "MatrixRegion<" << (*i)->dimensions() << ", const " STRINGIFY(MATRIX_ELEMENT_T) "> normalized_" << (*i)->name( )
                << " = " << matrix_name << ".asGpuBuffer(_iter_begin, _iter_end);\n";
        o.os( ) << "cl_mem devicebuf_" << (*i)->name( )
                << " = clCreateBuffer( OpenCLUtil::getContext( ), CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, "
                << "normalized_" << (*i)->name( ) << ".bytes( ),"
                << "(void*) normalized_" << (*i)->name( ) << ".base( ), &err );\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create input memory object for" << (*i)->name( ) << ".\" );\n";

        // Bind to kernel.
        o.os( ) << "clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(cl_mem), (void*)&devicebuf_" << (*i)->name( ) << " );\n\n";
      }
      else {
        (*i)->setBuffer(false);
      }
    }

    // Pass config parameters
    for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
      if(i->shouldPass()) {
        o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &" << i->name() << " );\n";
      }
    }

    RegionList::const_iterator output = _to.begin( );
    // Bind rule dimension arguments to kernel.
    for( int i = 0; i < iterdef.dimensions( ); ++i )
    {
      o.os( ) << "int ruledim_" << i << " = " << (*output)->matrix( )->name( ) << ".size(" << i << ");\n";
      o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &ruledim_" << i << " );\n";
      o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &_iter_begin[" << i << "]);\n";
      o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &_iter_end[" << i << "]);\n";
    }

    // Bind matrix dimension arguments to kernel.
    int count = 0;
    for( RegionList::const_iterator it = _to.begin( ); it != _to.end( ); ++it )
    {
      if((*it)->isBuffer()) {
        for( int i = 0; i < (int) (*it)->size() - 1; ++i ) {
          o.os( ) << "int ruledim_out" << count << "_" << i << " = " << (*it)->matrix( )->name() << ".size(" << i << ");\n";
          o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &ruledim_out" << count << "_" << i << " );\n";
        }
        count++;
      }
    }

    count = 0;
    set.clear();
    for( RegionList::const_iterator it = _from.begin( ); it != _from.end( ); ++it )
    {
      if((*it)->isBuffer()) {
        for( int i = 0; i < (int) (*it)->size() - 1; ++i ) {
          o.os( ) << "int ruledim_in" << count << "_" << i << " = " << (*it)->matrix( )->name() << ".size(" << i << ");\n";
          o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &ruledim_in" << count << "_" << i << " );\n";
        }
        count++;
      }
    }

    o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to bind kernel arguments.\" );\n\n";

    // Invoke kernel.
    /** \todo Need to generalize for >1 GPUs and arbitrary dimensionality. */
    o.comment( "Invoke kernel." );

    o.os( ) << "size_t workdim[] = { ";
    for( int i = 0; i < iterdef.dimensions( ); ++i )
    {
      if(i > 0) {
        o.os() << ", ";
      }
      //o.os( ) << "_iter_end[" << i << "]-_iter_begin[" << i << "]";
      o.os( ) << (*output)->matrix( )->name( ) << ".size(" << i << ")";
    }
    o.os( ) << "};\n";

    o.os( ) << "std::cerr << \"RUN GPU\" << std::endl;\n";

    #ifdef OPENCL_LOGGING
    o.os( ) << "std::cout << \"Work dimensions: \" << workdim[0] << \" x \" << workdim[1] << \"\\n\";\n";
    #endif
    o.os( ) << "err = clEnqueueNDRangeKernel( OpenCLUtil::getQueue( 0 ), clkern, " << iterdef.dimensions( ) << ", 0, workdim, NULL, 0, NULL, NULL );\n";
    //o.os( ) << "clFinish(OpenCLUtil::getQueue( 0 ));\n";
    //o.os( ) << "std::cerr << \"After RUN\" << std::endl;\n";
    #ifndef OPENCL_LOGGING
    o.os( ) << "if( CL_SUCCESS != err ) ";
    #endif
    o.os( ) << "std::cout << \"Kernel execution error #\" << err << \": \" << OpenCLUtil::errorString(err) << std::endl;\n";
    o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to execute kernel.\" );\n";

    // Copy results back to host memory.
    o.comment( "Copy results back to host memory." );
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      o.os( ) << "clEnqueueReadBuffer( OpenCLUtil::getQueue( 0 ), devicebuf_"
              << (*i)->name( ) << ", CL_TRUE, 0, "
              << "normalized_" << (*i)->name( ) <<  ".bytes(), "
              << "normalized_" << (*i)->name( ) << ".base(), "
              << "0, NULL, NULL );\n";
      o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to read output buffer.\" );\n";
    }
    o.os( ) << "\n";

    // Free memory
    o.comment( "Free memory." );
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      if((*i)->isBuffer())
        o.os( ) << "clReleaseMemObject( devicebuf_" << (*i)->name( ) << " );\n";
    }
    for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
    {
      if((*i)->isBuffer())
        o.os( ) << "clReleaseMemObject( devicebuf_" << (*i)->name( ) << " );\n";
    }

    // Create memory objects for outputs
    o.comment( "Copy back outputs (if they were already normalized, copyTo detects src==dst and does nothing)" );
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      //o.os( ) << "std::cerr << \"BEFORE copy\" << std::endl;\n";
      //o.os( ) << "MatrixIO(\"/dev/stderr\",\"w\").write(" << (*i)->matrix( )->name( ) << ");\n";

      o.os( ) << "normalized_" << (*i)->name( )
              << ".copyTo(" << (*i)->matrix( )->name( ) << ", _iter_begin, _iter_end);\n";

      //o.os( ) << "std::cerr << \"AFTER copy\" << std::endl;\n";
      //o.os( ) << "MatrixIO(\"/dev/stderr\",\"w\").write(" << (*i)->matrix( )->name( ) << ");\n";
    }

    o.write( "return NULL;\n}\n\n" );
    return;
  } else {
#else
  if(true) {
#endif
    if(RuleFlavor::SEQUENTIAL != flavor) {
      o.write("DynamicTaskPtr _spawner = new NullDynamicTask();");
      o.write("DynamicTaskPtr _last = NULL;");
    }

    iterdef.unpackargs(o);

    if(isSingleElement()){
      trans.markSplitSizeUse(o);
      o.beginIf("petabricks::split_condition<"+jalib::XToString(dimensions())+">("SPLIT_CHUNK_SIZE","COORD_BEGIN_STR","COORD_END_STR")");
      iterdef.genSplitCode(o, trans, *this, flavor);
      // return written in get split code
      o.elseIf();
    }

    iterdef.genLoopBegin(o);
    generateTrampCellCodeSimple( trans, o, flavor );
    iterdef.genLoopEnd(o);

    if(RuleFlavor::SEQUENTIAL != flavor){
      o.write("_spawner->dependsOn(_last);");
      o.write("return _spawner;");
    }
    else o.write("return NULL;");

    if(isSingleElement())
      o.endIf();
  }
  o.endFunc();
}

#ifdef HAVE_OPENCL

void petabricks::UserRule::generateOpenCLKernel( Transform& trans, CLCodeGenerator& clo, IterationDefinition& iterdef )
{
  SRCPOSSCOPE();
  // This is only null if code generation failed (that is, the rule is
  // unsupported.)
  if( !isOpenClRule() )
    return;

  TRACE( "10" );

  std::vector<std::string> from_matrices, to_matrices;
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    to_matrices.push_back( (*i)->name( ) );
  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
    from_matrices.push_back( (*i)->name( ) );

  clo.beginKernel(_to, _from, iterdef.dimensions(), trans);

  TRACE( "20" );

  // Get indices.
  for( int i = 0; i < iterdef.dimensions( ); ++i )
    clo.os( ) << "unsigned int " << _getOffsetVarStr( _id, i, NULL ) << " = get_global_id( " << i << " );\n";

  // Define rule index variables (from _definitions).
  for( FormulaList::iterator it = _definitions.begin( ); it != _definitions.end( ); ++it )
    clo.os( ) << STRINGIFY(MATRIX_INDEX_T) " " << (*it)->lhs()->toString() << " = " << (*it)->rhs()->toString() << ";\n";

  // Define Sizes
  //trans.extractOpenClSizeDefines(clo, iterdef.dimensions());

  // Conditional to ensure we are about to work on a valid part of the buffer.
  if(iterdef.dimensions()>0)
    clo.os( ) << "if( ";
  for( int i = 0; i < iterdef.dimensions( ); ++i )
  {
    //clo.os( ) << _getOffsetVarStr( _id, i, NULL ) << " < dim_d" << i << " ";
    clo.os( ) << _getOffsetVarStr( _id, i, NULL ) << " >= dim_d" << i << "_begin && ";
    clo.os( ) << _getOffsetVarStr( _id, i, NULL ) << " < dim_d" << i << "_end ";
    if( i != ( iterdef.dimensions( ) - 1 ) )
    clo.os( ) << "&& ";
  }
  if(iterdef.dimensions()>0)
    clo.os( ) << ") {\n";

  TRACE( "30" );

  // Generate indices into input and output arrays.
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
  {
    // Build & normalize formula for index.
    /*FormulaPtr idx_formula = FormulaInteger::zero( );
    for( int j = (*i)->minCoord( ).size( ) - 1; j >= 0; --j )
    {
      std::stringstream sizevar;
      sizevar << "dim_" << (*i)->name( ) << "_d" << j;
      idx_formula = new FormulaAdd( (*i)->minCoord( ).at( j ),
              new FormulaMultiply( new FormulaVariable( sizevar.str( ) ), idx_formula ) );
    }
    idx_formula = MaximaWrapper::instance( ).normalize( idx_formula );*/

    int minCoordSize = (*i)->minCoord().size();
    FormulaPtr idx_formula = new FormulaAdd((*i)->minCoord().at(minCoordSize - 1), FormulaInteger::zero());
    for( int j = minCoordSize - 2; j >= 0; --j )
    {
      std::stringstream sizevar;
      sizevar << "dim_" << (*i)->name( ) << "_d" << j;
      idx_formula = new FormulaAdd( (*i)->minCoord( ).at( j ),
            new FormulaMultiply( new FormulaVariable( sizevar.str( ) ), idx_formula ) );
    }

    clo.os( ) << "unsigned int idx_" << (*i)->name( ) << " = ";
    idx_formula->print( clo.os( ) );
    clo.os( ) << ";\n";
  }
  TRACE( "40" );
  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
  {
    // Build & normalize formula for index.
    /*FormulaPtr idx_formula = FormulaInteger::zero( );
    for( int j = (*i)->minCoord( ).size( ) - 1; j >= 0; --j )
    {
      std::stringstream sizevar;
      sizevar << "dim_" << (*i)->name( ) << "_d" << j;
      idx_formula = new FormulaAdd( (*i)->minCoord( ).at( j ),
            new FormulaMultiply( new FormulaVariable( sizevar.str( ) ), idx_formula ) );

      std::cout << (*i)->minCoord( ).at( j ) << std::endl;
    }
    idx_formula = MaximaWrapper::instance( ).normalize( idx_formula );*/

    FormulaPtr idx_formula = FormulaInteger::zero( );
    int minCoordSize = (*i)->minCoord().size();
    if(minCoordSize > 0) {
      idx_formula = new FormulaAdd((*i)->minCoord().at(minCoordSize - 1), idx_formula);
      for( int j = minCoordSize - 2; j >= 0; --j )
      {
        std::stringstream sizevar;
        sizevar << "dim_" << (*i)->name( ) << "_d" << j;
        idx_formula = new FormulaAdd( (*i)->minCoord( ).at( j ),
              new FormulaMultiply( new FormulaVariable( sizevar.str( ) ), idx_formula ) );
      }
    }

    clo.os( ) << "unsigned int idx_" << (*i)->name( ) << " = ";
    idx_formula->print( clo.os( ) );
    clo.os( ) << ";\n";
  }
  TRACE( "50" );

  // Load inputs to rule.
  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
  {
    clo.os( ) << /*STRINGIFY( MATRIX_ELEMENT_T )*/ "float" << " " << (*i)->name( ) << " = _region_" << (*i)->name( ) << "[idx_" <<
(*i)->name( ) << "];\n";
  }

  TRACE( "60" );

  // Quick hack -- generate a macro that will store the output from the rule.
  /** \todo this mechanism won't work with rules with multiple outputs */
  {
    RegionList::const_iterator i = _to.begin( );
    clo.os( ) << "#define RETURN(x) _region_" << (*i)->name( ) << "[idx_" << (*i)->name( ) << "] = x; return\n";
  }

  // Support for multiple-output rules
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
  {
    clo.os() << "#define " << (*i)->name( ) << " _region_" << (*i)->name( ) << "[idx_" << (*i)->name( ) << "]\n";
  }

  // Generate OpenCL implementation of rule logic.
 #ifdef DEBUG
  std::cerr << "--------------------\nAFTER GPU PASSES:\n" << _bodyir[RuleFlavor::OPENCL] << std::endl;
  {
    if( _bodyir[RuleFlavor::OPENCL] )
    {
      DebugPrintPass pdebug;
      _bodyir[RuleFlavor::OPENCL]->accept(pdebug);
    }
    else
    {
      std::cerr << " ( No OpenCL code was generated. )\n";
    }
  }
  std::cerr << "--------------------\n";
 #endif
  clo.write( _bodyir[RuleFlavor::OPENCL]->toString( ) );

  TRACE( "70" );

  //clo.os( ) << "OUT[idx_OUT] = IN[idx_IN];\n";

  // Close conditional and kernel.
  if(iterdef.dimensions()>0)
    clo.os( ) << "}\n";
  clo.endKernel( );
}

#endif

void petabricks::UserRule::generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, RuleFlavor flavor){
  SRCPOSSCOPE();
#if HAVE_OPENCL
  JASSERT( RuleFlavor::OPENCL != flavor );
#endif

  if( ( RuleFlavor::WORKSTEALING == flavor ) && !isRecursive( ) ) {
    //use sequential code if rule doesn't make calls
    flavor = RuleFlavor::SEQUENTIAL;
  }

  std::vector<std::string> args;
  for(RegionList::const_iterator i=_to.begin(); i!=_to.end(); ++i){
    args.push_back((*i)->generateAccessorCode());
  }
  for(RegionList::const_iterator i=_from.begin(); i!=_from.end(); ++i){
    args.push_back((*i)->generateAccessorCode());
  }
  for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
    if(i->shouldPass())
      args.push_back(i->name());
  }
  for(ConfigItems::const_iterator i=_duplicateVars.begin(); i!=_duplicateVars.end(); ++i){
    args.push_back(i->name());
  }

  for(int i=0; i<dimensions(); ++i)
    args.push_back(getOffsetVar(i)->toString());

  if(RuleFlavor::SEQUENTIAL != flavor){
    std::string classname = implcodename(trans)+"_"+flavor.str();
    o.setcall("jalib::JRef<"+classname+"> _rule", "new "+classname, args);
    //o.write("DynamicTaskPtr _task = _rule->runDynamic();");
    o.write("DynamicTaskPtr _task = new MethodCallTask<"+classname+", &"+classname+"::runDynamic>(_rule);");
    o.beginIf("_task");
    o.beginIf("_last");
    o.write("_task->dependsOn(_last);");
    o.endIf();
    o.write("_last = _task;");
    //o.write("_spawner->dependsOn(_task);");
    o.write("_task->enqueue();");
    o.endIf();
  }else{
    o.call(implcodename(trans)+TX_STATIC_POSTFIX, args);
  }
}

void petabricks::UserRule::generateCallCode(const std::string& name,
                                            Transform& trans,
                                            CodeGenerator& o,
                                            const SimpleRegionPtr& region,
                                            RuleFlavor flavor){
  SRCPOSSCOPE();
  switch(flavor) {
  case RuleFlavor::SEQUENTIAL:
    o.callSpatial(trampcodename(trans)+TX_STATIC_POSTFIX, region);
    break;
  case RuleFlavor::WORKSTEALING:
  case RuleFlavor::DISTRIBUTED:
    o.mkSpatialTask(name, trans.instClassName(), trampcodename(trans)+"_"+flavor.str(), region);
    break;
  default:
    UNIMPLEMENTED();
  }
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
  SRCPOSSCOPE();
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
  SRCPOSSCOPE();
  for( MatrixDependencyMap::const_iterator p=_provides.begin()
     ; p!=_provides.end()
     ; ++p)
  {
    ChoiceDepGraphNodeSet pNode = scheduler.lookupNode(p->first, p->second->region());
    for( MatrixDependencyMap::const_iterator d=_depends.begin()
       ; d!=_depends.end()
       ; ++d)
    {
      ChoiceDepGraphNodeSet dNode = scheduler.lookupNode(d->first, d->second->region());
      for(ChoiceDepGraphNodeSet::iterator a=pNode.begin(); a!=pNode.end(); ++a)
        for(ChoiceDepGraphNodeSet::iterator b=dNode.begin(); b!=dNode.end(); ++b)
          (*a)->addDependency(*b, this, d->second->direction());
    }

    //null depedency on all other output regions
    for( MatrixDependencyMap::const_iterator pp=_provides.begin()
      ; pp!=_provides.end()
      ; ++pp)
    {
      if(p!=pp){
        ChoiceDepGraphNodeSet dNode = scheduler.lookupNode(pp->first, pp->second->region());
        for(ChoiceDepGraphNodeSet::iterator a=pNode.begin(); a!=pNode.end(); ++a)
          for(ChoiceDepGraphNodeSet::iterator b=dNode.begin(); b!=dNode.end(); ++b)
            (*a)->addDependency(*b, this, DependencyDirection(std::max(1,dimensions()), DependencyDirection::D_MULTIOUTPUT));
      }
    }
  }
  //TODO collect edge/direction dependencies
}

petabricks::DependencyDirection petabricks::UserRule::getSelfDependency() const {
  SRCPOSSCOPE();
  DependencyDirection rv(dimensions());
  for( MatrixDependencyMap::const_iterator p=_provides.begin()
     ; p!=_provides.end()
     ; ++p)
  {
    MatrixDependencyMap::const_iterator d = _depends.find(p->first);
    if(d!=_depends.end()){
      const DependencyDirection& dir = d->second->direction();
      rv.addDirection(dir);
    }
  }
  return rv;
}

std::string petabricks::UserRule::implcodename(Transform& trans) const {
  return trans.name()+"_rule" + jalib::XToString(_id-trans.ruleIdOffset());
}
std::string petabricks::UserRule::trampcodename(Transform& trans) const {
  std::string s = trans.name()+"_apply_rule" + jalib::XToString(_id-trans.ruleIdOffset());
  for(size_t i=0; i<_duplicateVars.size(); ++i){
    long t = _duplicateVars[i].initial().i();
    if(_duplicateVars[i].min()<0)
      t-=_duplicateVars[i].min().i(); //handle negative case
    s += "_" + _duplicateVars[i].name() + jalib::XToString(t);
  }
  return s;
}

size_t petabricks::UserRule::duplicateCount() const {
  int c = 1;
  for(size_t i=0; i<_duplicateVars.size(); ++i)
    c*=_duplicateVars[i].range();
  return c;
}
size_t petabricks::UserRule::setDuplicateNumber(size_t c) {
  SRCPOSSCOPE();
  size_t prev = getDuplicateNumber();
#ifdef DEBUG
  size_t origC=c;
#endif
  for(size_t i=0; i<_duplicateVars.size(); ++i){
    ConfigItem& dv = _duplicateVars[i];
    dv.setInitial( int( dv.min().i() + (c % dv.range())));
    c /= dv.range();
  }
#ifdef DEBUG
  //make sure we did it right
  JASSERT(getDuplicateNumber()==origC)(getDuplicateNumber())(origC);
#endif
  return prev;
}
size_t petabricks::UserRule::getDuplicateNumber() {
  SRCPOSSCOPE();
  int lastRange = 1;
  int c = 0;
  for(ssize_t i=_duplicateVars.size()-1; i>=0; --i){
    ConfigItem& dv = _duplicateVars[i];
    c*=lastRange;
    c+=dv.initial().i()-dv.min().i();
    lastRange = dv.range();
  }
  return c;
}

