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
//#define GPU_TRACE 1

#define MAX_BLOCK_SIZE 16
//#define BLOCK_SIZE_X 16
//#define BLOCK_SIZE_Y 16

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
  /*std::cerr << "--------------------\nBEFORE compileRuleBody:\n" << bodyir << std::endl;
  bodyir->accept(print);
  std::cerr << "--------------------\n";*/
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
#ifdef DEBUG
      /*std::cerr << "--------------------\nAFTER compileRuleBody:\n" << bodyir << std::endl;
      bodyir->accept(print);
      std::cerr << "--------------------\n";*/
      TRACE("ENABLE/DISABLE GPU RULE");
      std::cerr << "----------------------------------" << std::endl;
      this->print(std::cout);
      std::cerr << "----------------------------------" << std::endl;
#endif
  if(isOpenClRule() && getSelfDependency().isNone()){
    try {
      _bodyir[RuleFlavor::OPENCL] = bodyir;
      _bodyir[RuleFlavor::OPENCL]->accept(openclfnreject);
      _bodyir[RuleFlavor::OPENCL]->accept(opencl);
      _bodyir[RuleFlavor::OPENCL]->accept(gpurename);

      _bodyirLocalMem = bodyir;
      _bodyirLocalMem->accept(openclfnreject);
      prepareBuffers();
      collectGpuLocalMemoryData();
      opencl.setLocalMemoryData(_nameMap, _minCoordOffsets, _maxCoordOffsets, _id);
      _bodyirLocalMem->accept(opencl);
      _bodyirLocalMem->accept(gpurename);
      std::cerr << "--------------------\nAFTER compileRuleBody:\n" << bodyir << std::endl;
      bodyir->accept(print);
      std::cerr << "--------------------\n";

      if(!passBuildGpuProgram(tx)) {
	std::cout << "(>) RULE REJECTED BY TryBuildGpuProgram: RULE " << id() << "\n";
        failgpu = true;
      }
    }
    catch( OpenClCleanupPass::NotValidSource e )
    {
      std::cout << "(>) RULE REJECTED BY OpenClCleanupPass: RULE " << id() << "\n";
      failgpu = true;
    }
    catch( OpenClFunctionRejectPass::NotValidSource e )
    {
      std::cout << "(>) RULE REJECTED BY OpenClFunctionRejectPass: RULE " << id() << "\n";
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
void petabricks::UserRule::collectGpuLocalMemoryData() {

  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  unsigned int dim = iterdef.dimensions();
  RegionList::const_iterator it = _to.begin( );

  // Only use local memory when dimension <= 2;
  if(dim > 2)
    return;


  for( RegionList::const_iterator it = _from.begin( ); it != _from.end( ); ++it )
  {
    if((*it)->isBuffer() && (*it)->dimensions() == dim) {
      SimpleRegionPtr region = _fromBoundingBox[(*it)->matrix()];
      bool local = true;
      FormulaList min, max;
      for(int i = 0; i < (int) dim; i++) {
        FormulaPtr min_diff = new FormulaSubtract(region->minCoord().at(i), getOffsetVar(i));
        min_diff = MAXIMA.normalize(min_diff);
        FreeVarsPtr vars = min_diff->getFreeVariables();
        if(vars->size() > 0) {
          local = false;
          break;
        }


        FormulaPtr max_diff = new FormulaSubtract(region->maxCoord().at(i), getOffsetVar(i));
        max_diff = MAXIMA.normalize(max_diff);
        vars = max_diff->getFreeVariables();
        if(vars->size() > 0) {
          local = false;
          break;
        }

        min.push_back(min_diff);
        max.push_back(max_diff);

      }

      if(local && MAXIMA.comparePessimistically(region->symbolicSize(), ">", FormulaInteger::one()) ) {
        std::cout << "ADD " << (*it)->matrix()->name() << std::endl;
        std::string matrix = (*it)->matrix()->name();
        _nameMap[(*it)->name()] = matrix;
        _minCoordOffsets[matrix] = min;
        _maxCoordOffsets[matrix] = max;
      }
    }
  }
}

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

  for(MatrixToRegionMap::const_iterator i=_fromBoundingBox.begin(); i!=_fromBoundingBox.end(); ++i) {
    os << "\nfromBoundingBox " << i->first 
       << " " << i->second;
  }

  if(!_dataDependencyVectorMap.empty()) {
    os << "\ndata dependency vector map: " << _dataDependencyVectorMap;
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

  buildFromBoundingBox();

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
    (*i)->collectDependencies(trans, *this, _depends);
  }
  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->collectDependencies(trans, *this, _provides);
  }
  
  computeDataDependencyVector();
  
  //expand through() clause
  for(MatrixDefList::const_iterator i=_through.begin(); i!=_through.end(); ++i){
    _bodysrc=(*i)->genericTypeName()+" "+(*i)->name()+" = "+(*i)->genericAllocateStr()+";\n"+_bodysrc;
  }

  MaximaWrapper::instance().popContext();
}

/**
 * Compute the data dependency vector for two region as the difference of the
 * dimensions of the two regions
 */
petabricks::CoordinateFormula petabricks::UserRule::computeDDVAsDifference(const RegionPtr inputRegion,
                                                                           const RegionPtr outputRegion
                                                                          ) const {
  JASSERT(inputRegion->dimensions()==outputRegion->dimensions());
  
  CoordinateFormula& inputMinCoord = inputRegion->minCoord();
  CoordinateFormula& outputMinCoord = outputRegion->minCoord();
  size_t dimensions=inputRegion->dimensions();
  
  CoordinateFormulaPtr newDataDependencyVector = new CoordinateFormula();
  
  for(size_t i=0; i<dimensions; ++i) {
    FormulaPtr difference = new FormulaSubtract(inputMinCoord[i], 
                                             outputMinCoord[i]);
    difference = MaximaWrapper::instance().normalize(difference);
    newDataDependencyVector->push_back(difference);
  }
  return newDataDependencyVector;
}

/**
 * Computes the distance between the given output region and all the input
 * regions coming from the same original matrix
 */
void petabricks::UserRule::computeDDVForGivenOutput(const RegionPtr outputRegion
                                                   ) {
  for(RegionList::const_iterator i=_from.begin(), e=_from.end(); i!=e; ++i ) {
    const RegionPtr inputRegion = *i;
    
    if(outputRegion->matrix()->name() != inputRegion->matrix()->name()) {
      continue;
    }
    
    CoordinateFormula ddv = computeDDVAsDifference(inputRegion,
                                                    outputRegion);
    
    MatrixDefPtr inputMatrixDef=inputRegion->matrix();
    DataDependencyVectorMap::value_type newElement(inputMatrixDef,ddv);
    _dataDependencyVectorMap.insert(newElement);
  }
  
}

/**
 * Computes the distance between input and output for each dimension 
 * of each region that is used both as input and output
 */
void petabricks::UserRule::computeDataDependencyVector() {
  //Loop on outputs (_to) first, because they usually are less then inputs
  for(RegionList::const_iterator i=_to.begin(), e=_to.end(); i != e; ++i) {
    const RegionPtr outputRegion = *i;
    
    computeDDVForGivenOutput(outputRegion);
    
  }
}


void petabricks::UserRule::buildFromBoundingBox(){
  SRCPOSSCOPE();
  MatrixToRegionMap::iterator bbi;
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    JTRACE("building from bb")(_fromBoundingBox[(*i)->matrix()])(*(*i));
    if(_fromBoundingBox[(*i)->matrix()]) {
      _fromBoundingBox[(*i)->matrix()] = _fromBoundingBox[(*i)->matrix()]->regionUnion(*i);
    }else{
      _fromBoundingBox[(*i)->matrix()] = new SimpleRegion(*(*i));
    }
  }
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
    o.define("DEFAULT_RV", "cleanUpTaskTmp");
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
  o.write("RETURN_VOID;");
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

void petabricks::UserRule::prepareBuffers() {
  std::set<std::string> set;
	for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i ) {
		std::string matrix_name = (*i)->matrix( )->name( );
    std::set<std::string>::iterator matrix_it = set.find(matrix_name);
    if(matrix_it == set.end()) {
			set.insert(matrix_name);
			(*i)->setBuffer(true);
		}
		else {
			(*i)->setBuffer(false);
		}
	}

	for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i ) {
		std::string matrix_name = (*i)->matrix( )->name( );
    std::set<std::string>::iterator matrix_it = set.find(matrix_name);
    if(matrix_it == set.end()) {
			set.insert(matrix_name);
			(*i)->setBuffer(true);
		}
		else {
			(*i)->setBuffer(false);
		}
	}
}

void petabricks::UserRule::generateDeclCodeOpenCl(Transform& /*trans*/, CodeGenerator& /*o*/) {
  /* code generated here goes at the top level of the file before the instance class */
}

void petabricks::UserRule::generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor){
  SRCPOSSCOPE();

#ifdef HAVE_OPENCL
  if(RuleFlavor::WORKSTEALING_OPENCL == flavor) {
    generateMultiOpenCLTrampCodes(trans, o);
    return;
  }
  if(RuleFlavor::DISTRIBUTED_OPENCL == flavor) {
    o.comment("UserRule::generateTrampCode RuleFlavor::DISTRIBUTED_OPENCL");
    //TODO
    return;
  }
#endif

  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  std::vector<std::string> taskargs = iterdef.packedargs();
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::vector<std::string> packedargnames = iterdef.packedargnames();

  taskargs.insert(taskargs.begin(), "const jalib::JRef<"+trans.instClassName()+"> transform");

  o.beginFunc("petabricks::DynamicTaskPtr", trampcodename(trans)+"_"+flavor.str(), packedargs);

#ifdef HAVE_OPENCL
    for(RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i) {
      o.write((*i)->matrix()->name()+".useOnCpu();");
      //o.write("MatrixIO().write("+(*i)->matrix()->name()+");");
    }
#endif

  for(size_t i=0; i<_duplicateVars.size(); ++i){
    o.varDecl("const IndexT "+_duplicateVars[i].name() + " = " + jalib::XToString(_duplicateVars[i].initial()));
  }

#ifdef HAVE_OPENCL
  if(RuleFlavor::SEQUENTIAL_OPENCL == flavor)
  {
    o.write(trampcodename(trans)+TX_STATIC_POSTFIX + "(_iter_begin, _iter_end);");
    o.write("return NULL;");
    o.endFunc();
    return;
    o.write("std::cout << \"<<<RUN SEQUENTIAL>>>\" << std::endl;");
    o.os() << "cl_int err;\n";
    o.os() << "cl_kernel clkern = " "get_kernel_" << id() << "_nolocal();\n"; //TODO global get_kernel

    int arg_pos = 0;

    //o.os( ) << "printf( \"- TRACE 10\\n\" );\n";
		o.os( ) << "if( ";
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      if(i != _to.begin( )) {
        o.os() << " && ";
      }
      o.os( ) << (*i)->matrix( )->name( ) << ".bytes() == 0";
    }
    o.os( ) << ") {\n";
    o.os( ) << "return NULL;\n";
    o.os( ) << "}\n";

    // clSetKernelArg needs to be conformed with CLCodeGenerator::beginKernel

    o.comment( "Create memory objects for outputs." );
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {

      std::string matrix_name = (*i)->matrix( )->name( );
      if((*i)->isBuffer()) {

        o.os( ) << "MatrixRegion<" << (*i)->dimensions() << ", " STRINGIFY(MATRIX_ELEMENT_T) "> normalized_" << (*i)->name( ) 
                << " = " << matrix_name << ".asGpuOutputBuffer(_iter_begin, _iter_end);\n";
        o.os( ) << "cl_mem devicebuf_" << (*i)->name( ) 
                << " = clCreateBuffer( OpenCLUtil::getContext( ), CL_MEM_READ_WRITE, " 
                << "normalized_" << (*i)->name( ) << ".bytes( ), NULL, &err );\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create output memory object for" << (*i)->name( ) << ".\" );\n";
        //o.os( ) << "std::cerr << \"" << (*i)->matrix( )->name( ) << "\" << std::endl;\n";
        //o.os( ) << "std::cerr << normalized_" << (*i)->name( ) << ".bytes( ) << std::endl;\n";

        // Bind to kernel.
        o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(cl_mem), (void*)&devicebuf_" << (*i)->name( ) << " );\n\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to bind kernel arguments.\" );\n\n";
      }
    }

    // Create memory objects for inputs.
    o.comment( "Create memory objects for inputs." );
    for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
    {
      std::string matrix_name = (*i)->matrix( )->name( );
      if((*i)->isBuffer()) {
#ifdef DEBUG
        o.os( ) << "std::cout << \"INPUT\" << std::endl;\n";
        o.os( ) << "MatrixIO().write(" << (*i)->matrix( )->name( ) << ");\n";
#endif

        o.os( ) << "MatrixRegion<" << (*i)->dimensions() << ", const " STRINGIFY(MATRIX_ELEMENT_T) "> normalized_" << (*i)->name( ) 
                << " = " << matrix_name << ".asGpuInputBuffer();\n";

        o.os( ) << "cl_mem devicebuf_" << (*i)->name( ) << ";\n";
#ifndef NVIDIA
        o.write("std::cout << \"use host_ptr\" << std::endl;");
        o.beginIf(matrix_name+".isEntireBuffer()");
        o.os( ) << "devicebuf_" << (*i)->name( ) 
                << " = clCreateBuffer( OpenCLUtil::getContext( ), CL_MEM_USE_HOST_PTR, "
                << "normalized_" << (*i)->name( ) << ".bytes( ),"
                << "(void*) normalized_" << (*i)->name( ) << ".base( ), &err );\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create input memory object for" << (*i)->name( ) << ".\" );\n";
        o.elseIf();
#endif
        o.write("std::cout << \"not use host_ptr\" << std::endl;");
        o.os( ) << "devicebuf_" << (*i)->name( ) 
                << " = clCreateBuffer( OpenCLUtil::getContext( ), CL_MEM_READ_WRITE, "
                << "normalized_" << (*i)->name( ) << ".bytes( ), NULL, &err );\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create input memory object for" << (*i)->name( ) << ".\" );\n";
        o.write("err = clEnqueueWriteBuffer(OpenCLUtil::getQueue(0), devicebuf_"+ (*i)->name( ) +", CL_TRUE, 0, normalized_"+(*i)->name()+".bytes( ), normalized_"+ (*i)->name( )+".base( ), 0, NULL, NULL);");
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to copy input memory object\");\n";

#ifndef NVIDIA
        o.endIf();
#endif

        // Bind to kernel.
        o.os( ) << "clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(cl_mem), (void*)&devicebuf_" << (*i)->name( ) << " );\n\n";
      }
    }

    // Pass config parameters
    for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
      if(i->shouldPass()) {
        o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &" << i->name() << " );\n";
      }
    }

    // Bind rule dimension arguments to kernel.
    for( int i = 0; i < iterdef.dimensions( ); ++i )
    {
      //o.os( ) << "int ruledim_" << i << " = " << (*output)->matrix( )->name( ) << ".size(" << i << ");\n";
      //o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &ruledim_" << i << " );\n";
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
    for( RegionList::const_iterator it = _from.begin( ); it != _from.end( ); ++it )
    {
      if((*it)->isBuffer()) {
        for( int i = 0; i < (int) (*it)->size(); ++i ) {
          o.os( ) << "int ruledim_in" << count << "_" << i << " = " << (*it)->matrix( )->name() << ".size(" << i << ");\n";
          o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &ruledim_in" << count << "_" << i << " );\n";
        }
        count++;
      }
    }
    o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to bind kernel arguments.\" );\n\n";

    // Invoke kernel.
    o.comment( "Invoke kernel." );

    RegionPtr rep = *(_to.begin());
    o.os( ) << "size_t workdim[] = { ";
    if(isSingleCall()) {
      o.os() << "1";
    }
    else if(rep->getRegionType() == Region::REGION_ROW) {
      o.os( ) << rep->matrix( )->name( ) << ".size(1)"; //TODO: check
    }
    else if(rep->getRegionType() == Region::REGION_COL) {
      o.os( ) << rep->matrix( )->name( ) << ".size(0)"; //TODO: check
    }
    //else if(rep->getRegionType() == Region::REGION_BOX) {
    //}
    else {
      for( int i = 0; i < iterdef.dimensions( ); ++i )
      {
        if(i > 0) {
          o.os() << ", ";
        }
        //o.os( ) << "_iter_end[" << i << "]-_iter_begin[" << i << "]";
        o.os( ) << rep->matrix( )->name( ) << ".size(" << i << ")";
      }
    }
    o.os( ) << "};\n";

    #ifdef OPENCL_LOGGING
    o.os( ) << "std::cout << \"Work dimensions: \" << workdim[0] << \" x \" << workdim[1] << \"\\n\";\n";
    #endif

    o.os( ) << "err = clEnqueueNDRangeKernel( OpenCLUtil::getQueue( 0 ), clkern, " << iterdef.dimensions( ) << ", 0, workdim, NULL, 0, NULL, NULL );\n";
    //o.os( ) << "clFinish(OpenCLUtil::getQueue( 0 ));\n";
    #ifndef OPENCL_LOGGING
    o.os( ) << "if( CL_SUCCESS != err ) ";
    #endif
    o.os( ) << "std::cout << \"Kernel execution error #\" << err << \": \" << OpenCLUtil::errorString(err) << std::endl;\n";
    o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to execute kernel.\" );\n";

    // Copy results back to host memory.
    o.comment( "Copy results back to host memory." );
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      if((*i)->isBuffer()) {
        o.os( ) << "clEnqueueReadBuffer( OpenCLUtil::getQueue( 0 ), devicebuf_" 
                << (*i)->name( ) << ", CL_TRUE, 0, " 
                << "normalized_" << (*i)->name( ) <<  ".bytes(), " 
                << "normalized_" << (*i)->name( ) << ".base(), "
                << "0, NULL, NULL );\n";
        o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to read output buffer.\" );\n";
      }
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
      if((*i)->isBuffer()) {
        //o.os( ) << "std::cerr << \"BEFORE copy\" << std::endl;\n";
        //o.os( ) << "MatrixIO(\"/dev/stderr\",\"w\").write(" << (*i)->matrix( )->name( ) << ");\n";

        o.os( ) << "normalized_" << (*i)->name( ) 
                << ".copyTo(" << (*i)->matrix( )->name( ) << ", _iter_begin, _iter_end);\n";

#ifdef DEBUG
        o.os( ) << "std::cout << \"normalize\" << std::endl;\n";
        o.os( ) << "MatrixIO().write(normalized_" << (*i)->name( ) << ");\n";
        o.os( ) << "std::cout << \"AFTER copy\" << std::endl;\n";
        o.os( ) << "MatrixIO().write(" << (*i)->matrix( )->name( ) << ");\n";
#endif
      }
    }

    o.write( "return NULL;\n}\n\n" );
    return;
  } else {
#else
  if(true) {
#endif

    if(RuleFlavor::SEQUENTIAL != flavor 
#ifdef HAVE_OPENCL
        && RuleFlavor::SEQUENTIAL_OPENCL != flavor
#endif
        ) {
      o.write("DynamicTaskPtr _spawner = new NullDynamicTask();");
      o.write("DynamicTaskPtr _last = NULL;");
    }

    if(RuleFlavor::DISTRIBUTED != flavor) {
      std::string outputDimensionCheck;
      for( RegionList::const_iterator i = _to.begin(); i != _to.end(); ++i) {
        if(i != _to.begin()) {
          outputDimensionCheck = outputDimensionCheck + " && ";
        }
        outputDimensionCheck = outputDimensionCheck + (*i)->matrix()->name() + ".bytes() == 0";
      }

      o.beginIf(outputDimensionCheck);
      if(RuleFlavor::SEQUENTIAL != flavor) o.write("return _spawner;");
      else o.write("return NULL;");
      o.endIf();
    }

    iterdef.unpackargs(o);

    if(isSingleElement()){
      trans.markSplitSizeUse(o);
      Heuristic blockNumberHeur = HeuristicManager::instance().getHeuristic("UserRule_blockNumber");
      blockNumberHeur.setMin(2);
      unsigned int blockNumber = blockNumberHeur.eval(ValueMap()); /**< The number of blocks the loop will be
                                                                    * splitted into */
      JTRACE("LOOP BLOCKING")(blockNumber);
      o.beginIf("petabricks::split_condition<"+jalib::XToString(dimensions())+", "+jalib::XToString(blockNumber)+">("SPLIT_CHUNK_SIZE","COORD_BEGIN_STR","COORD_END_STR")");
      iterdef.genSplitCode(o, trans, *this, flavor, blockNumber);
      // return written in get split code
      o.elseIf();
    }

    iterdef.genLoopBegin(o);
    generateTrampCellCodeSimple( trans, o, flavor );
    iterdef.genLoopEnd(o);

#ifdef HAVE_OPENCL
    for(RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i) {
      if((*i)->isBuffer())
        o.write((*i)->matrix()->name()+".storageInfo()->modifyOnCpu();");
    }
#endif
    
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

void petabricks::UserRule::generateMultiOpenCLTrampCodes(Transform& trans, CodeGenerator& o){
  SRCPOSSCOPE();
  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::string codename = trampcodename(trans) + TX_OPENCL_POSTFIX;

  generateOpenCLCallCode(trans,o);
  generateOpenCLPrepareCode(codename,packedargs,o);

  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i ) {
    if((*i)->isBuffer())
      generateOpenCLCopyInCode(codename,packedargs,o,*i);
  }

  generateOpenCLRunCode(trans, o);

  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i ) {
    if((*i)->isBuffer())
      generateOpenCLCopyOutCode(codename,o,*i);
  }
}

void petabricks::UserRule::generateOpenCLCallCode(Transform& trans,  CodeGenerator& o){
  SRCPOSSCOPE();  
  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  std::vector<std::string> packedargs = iterdef.packedargs();
  packedargs.push_back("int nodeID");
  packedargs.push_back("RegionNodeGroupMapPtr map");
  packedargs.push_back("int gpuCopyOut");
  std::string codename = trampcodename(trans) + TX_OPENCL_POSTFIX;
  std::string objectname = trans.instClassName() + "_workstealing"; //TODO: handle _distributed
  std::string dimension = jalib::XToString(iterdef.dimensions());

  std::string prepareclass = "petabricks::GpuSpatialMethodCallTask<"+objectname
                           + ", " + dimension
                           + ", &" + objectname + "::" + codename + "_prepare"
                           + ">";
  std::string runclass = "petabricks::GpuSpatialMethodCallTask<"+objectname
                           + ", " + dimension
                           + ", &" + objectname + "::" + codename + "_run"
                           + ">";

  o.beginFunc("petabricks::DynamicTaskPtr", codename+"_createtasks", packedargs);
  
  std::string outputDimensionCheck;
  for( RegionList::const_iterator i = _to.begin(); i != _to.end(); ++i) {
    if(i != _to.begin()) {
      outputDimensionCheck = outputDimensionCheck + " && ";
    }
    outputDimensionCheck = outputDimensionCheck + (*i)->matrix()->name() + ".bytes() == 0";
  }

  o.write("DynamicTaskPtr end = new NullDynamicTask();");

  o.beginIf(outputDimensionCheck);
  o.write("return end;");
  o.endIf();

  o.write("GpuTaskInfoPtr taskinfo = new GpuTaskInfo(nodeID, map, gpuCopyOut);");
  o.write("DynamicTaskPtr prepare = new "+prepareclass+"(this,_iter_begin, _iter_end, taskinfo, GpuDynamicTask::PREPARE);");
  o.write("prepare->enqueue();");

  int id = 0;
  for(RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i ) {
    if((*i)->isBuffer()){
      std::string copyinclass = "petabricks::GpuSpatialMethodCallTask<"+objectname
                              + ", " + dimension
                              + ", &" + objectname + "::" + codename + "_copyin_" + (*i)->name()
                              + ">";
      std::string taskid = jalib::XToString(id);
      o.write("DynamicTaskPtr copyin_"+taskid+" = new "+copyinclass+"(this,_iter_begin, _iter_end, taskinfo, GpuDynamicTask::COPYIN, "+(*i)->matrix()->name()+".storageInfo());");
      o.write("copyin_"+taskid+"->enqueue();");
      id++;
    }
  }

  o.write("\nDynamicTaskPtr run = new "+runclass+"(this,_iter_begin, _iter_end, taskinfo, GpuDynamicTask::RUN);");

  o.beginIf("gpuCopyOut == 1");
  o.write("run->enqueue();");
  id = 0;
  for(RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i ) {
    if((*i)->isBuffer()){
      o.beginIf("map->find(\""+(*i)->matrix()->name()+"\") != map->end()");
      std::string copyinclass = "petabricks::GpuCopyOutMethodCallTask<"+objectname
                              + ", " + dimension
                              + ", &" + objectname + "::" + codename + "_copyout_" + (*i)->name()
                              + ">";
      std::string taskid = jalib::XToString(id);
      o.write("DynamicTaskPtr copyout_"+taskid+" = new "+copyinclass+"(this, taskinfo, "+(*i)->matrix()->name()+".storageInfo());");
      o.write("end->dependsOn(copyout_"+taskid+");");
      o.write("copyout_"+taskid+"->enqueue();");
      id++;
      o.endIf();
    }
  }
  o.elseIf();
  o.write("end->dependsOn(run);");
  o.write("run->enqueue();");
  o.endIf();
  o.write("return end;");
  o.endFunc();
}

void petabricks::UserRule::generateOpenCLPrepareCode(std::string& codename, std::vector<std::string>& packedargs, CodeGenerator& o){
  SRCPOSSCOPE();
  o.beginFunc("petabricks::DynamicTaskPtr", codename+"_prepare", packedargs);

  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i ) {
    std::string matrix_name = (*i)->matrix()->name();
    if((*i)->isBuffer()) {
      o.write("GpuManager::_currenttaskinfo->addToMatrix(" + matrix_name + ".storageInfo());");
      o.write(matrix_name + ".storageInfo()->setName(std::string(\""+matrix_name+"\"));");
    }
  }
  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i ) {
    std::string matrix_name = (*i)->matrix()->name();
    if((*i)->isBuffer()) {
      o.write("GpuManager::_currenttaskinfo->addFromMatrix(" + matrix_name + ".storageInfo());");
    }
  }

  o.write( "return NULL;" );
  o.endFunc();
}

void petabricks::UserRule::generateOpenCLCopyInCode(std::string& codename, std::vector<std::string>& packedargs, CodeGenerator& o, RegionPtr region){
  SRCPOSSCOPE();
  std::string name = region->matrix()->name();
  o.beginFunc("petabricks::DynamicTaskPtr", codename+"_copyin_"+region->name(), packedargs);
  o.write(name+".useOnCpu();");
  o.write("MatrixStorageInfoPtr storage_"+name+" = "+name+".storageInfo();");
#ifdef GPU_TRACE
  //o.write("MatrixIO().write("+name+");");
#endif
  //o.write("MatrixIO().write("+name+".asGpuInputBuffer());");
  o.write("cl_int err = clEnqueueWriteBuffer(GpuManager::_queue, storage_"+name+"->getClMem(), CL_FALSE, 0, storage_"+name+"->bytes(), "+name+".getGpuInputBufferPtr(), 0, NULL, NULL);");
  o.write("clFlush(GpuManager::_queue);");
  /*o.write("JASSERT(CL_INVALID_CONTEXT != err).Text( \"Failed to write to buffer.\");");
  o.write("JASSERT(CL_INVALID_VALUE != err).Text( \"Failed to write to buffer.\");");
  o.write("JASSERT(CL_INVALID_BUFFER_SIZE != err).Text( \"Failed to write to buffer.\");");
  o.write("JASSERT(CL_DEVICE_MAX_MEM_ALLOC_SIZE != err).Text( \"Failed to write to buffer.\");");
  o.write("JASSERT(CL_INVALID_HOST_PTR != err).Text( \"Failed to write to buffer.\");");
  o.write("JASSERT(CL_MEM_OBJECT_ALLOCATION_FAILURE != err).Text( \"Failed to write to buffer.\");");
  o.write("JASSERT(CL_OUT_OF_HOST_MEMORY != err).Text( \"Failed to write to buffer.\");");*/
  o.write("JASSERT(CL_SUCCESS == err)(err).Text( \"Failed to write to buffer.\");");
  o.write( "return NULL;" );
  o.endFunc();
}

void petabricks::UserRule::generateOpenCLRunCode(Transform& trans, CodeGenerator& o){
  SRCPOSSCOPE();
  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  std::vector<std::string> packedargs = iterdef.packedargs();

  o.beginFunc("petabricks::DynamicTaskPtr", trampcodename(trans) + TX_OPENCL_POSTFIX + "_run", packedargs);
  o.write("cl_int err = CL_SUCCESS;");

    RegionPtr rep = *(_to.begin());
    o.os( ) << "size_t workdim[] = { ";
    if(isSingleCall()) {
      o.os() << "1";
    }
    else if(rep->getRegionType() == Region::REGION_ROW) {
      o.os( ) << rep->matrix( )->name( ) << ".size(1)"; //TODO: check
    }
    else if(rep->getRegionType() == Region::REGION_COL) {
      o.os( ) << rep->matrix( )->name( ) << ".size(0)"; //TODO: check
    }
    //else if(rep->getRegionType() == Region::REGION_BOX) {
    //}
    else {
      for( int i = 0; i < iterdef.dimensions( ); ++i )
      {
        if(i > 0) {
          o.os() << ", ";
        }
        //o.os( ) << "_iter_end[" << i << "]-_iter_begin[" << i << "]";
        o.os( ) << rep->matrix( )->name( ) << ".size(" << i << ")";
      }
    }
    o.os( ) << "};\n";


  if(canUseLocalMemory()) {
    o.os() << "cl_kernel clkern;\n";
#ifdef GPU_TRACE
    o.write("std::cout << \"use_localmem \" << use_localmem << std::endl;");
    o.write("std::cout << \"workdim[0] \" << workdim[0] << std::endl;");
    o.write("std::cout << \"workdim[1] \" << workdim[1] << std::endl;");
    o.write("std::cout << \"opencl_blocksize \" << opencl_blocksize << std::endl;");
#endif
    if(iterdef.dimensions( ) == 1) {
      o.os() << "if(use_localmem == 1 && workdim[0] % opencl_blocksize*opencl_blocksize == 0) {";
    }
    else {
      o.os() << "if(use_localmem == 1 && workdim[0] % opencl_blocksize == 0 && workdim[1] % opencl_blocksize == 0) {";
    }
    o.os() << "clkern = " << "get_kernel_" << id() << "_local();\n";
#ifdef GPU_TRACE
    o.write("std::cout << \"LOCAL\" << std::endl;");
#endif
    o.os() << "} else {\n";
    o.os() << "clkern = " << "get_kernel_" << id() << "_nolocal();\n";
#ifdef GPU_TRACE
    o.write("std::cout << \"NO LOCAL\" << std::endl;");
#endif
    o.os() << "}\n";
  }
  else {
    o.os() << "cl_kernel clkern = " << "get_kernel_" << id() << "_nolocal();\n";
#ifdef GPU_TRACE
    o.write("std::cout << \"NO CHOICE\" << std::endl;");
#endif
  }

	//o.write("cl_program clprog = "+trans.name()+"_instance::get_program_"+jalib::XToString(id())+"();");
	//o.write("JASSERT( CL_SUCCESS == err ).Text( \"Failed to create kernel.\" );");
	//o.write("cl_kernel clkern = clCreateKernel(clprog, \"kernel_main\", &err );");
	//o.write("std::cout << \"kernel = \" << clkern << std::endl;");
	//o.write("std::cout << \"queue = \" << GpuManager::_queue << std::endl << std::endl;");

  // clSetKernelArg needs to be conformed with CLCodeGenerator::beginKernel

  int arg_pos = 0;
  // Pass clmem args
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i ) {
    if((*i)->isBuffer())
      o.write("err |= clSetKernelArg(clkern, "+jalib::XToString(arg_pos++)+", sizeof(cl_mem), "+(*i)->matrix()->name()+".storageInfo()->getClMemPtr());");
  }

  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i ) {
    if((*i)->isBuffer())
      o.write("err |= clSetKernelArg(clkern, "+jalib::XToString(arg_pos++)+", sizeof(cl_mem), "+(*i)->matrix()->name()+".storageInfo()->getClMemPtr());");
  }
  o.os( ) << "JASSERT( CL_SUCCESS == err )(err).Text( \"Failed to bind kernel arguments.\" );\n\n";

  // Pass config parameters
  for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
    if(i->shouldPass()) {
      o.os( ) << "err |= clSetKernelArg( clkern, " << arg_pos++ << ", sizeof(int), &" << i->name() << " );\n";
    }
  }

  // Bind rule dimension arguments to kernel.
  RegionList::const_iterator output = _to.begin( );
  for( int i = 0; i < iterdef.dimensions( ); ++i )
  {
    //o.os( ) << "int ruledim_" << i << " = " << (*output)->matrix( )->name( ) << ".size(" << i << ");\n";
    //o.os( ) << "err |= clSetKernelArg(clkern, " << arg_pos++ << ", sizeof(int), &ruledim_" << i << " );\n";
    o.os( ) << "err |= clSetKernelArg(clkern, " << arg_pos++ << ", sizeof(int), &_iter_begin[" << i << "]);\n";
    o.os( ) << "err |= clSetKernelArg(clkern, " << arg_pos++ << ", sizeof(int), &_iter_end[" << i << "]);\n";
#ifdef GPU_TRACE
    o.write("std::cout << \"iter_begin["+jalib::XToString(i)+"] = \" << _iter_begin["+jalib::XToString(i)+"] << std::endl;");
    o.write("std::cout << \"iter_end["+jalib::XToString(i)+"] = \" << _iter_end["+jalib::XToString(i)+"] << std::endl;");
#endif
  }

  // Bind matrix dimension arguments to kernel.
  int count = 0;
  for( RegionList::const_iterator it = _to.begin( ); it != _to.end( ); ++it )
  {
    std::string name = (*it)->matrix()->name();
    if((*it)->isBuffer()) {
      for( int i = 0; i < (int) (*it)->size() - 1; ++i ) {
        o.os( ) << "int ruledim_out" << count << "_" << i << " = " << name << ".size(" << i << ");\n";
        o.os( ) << "err |= clSetKernelArg(clkern, " << arg_pos++ << ", sizeof(int), &ruledim_out" << count << "_" << i << " );\n";
      }
      count++;
      //o.write(name+".storageInfo()->incCoverage("+name+".intervalSize(_iter_begin, _iter_end));");
      o.write(name+".storageInfo()->incCoverage(_iter_begin, _iter_end, "+name+".intervalSize(_iter_begin, _iter_end));");
    }
  }

  count = 0;
  for( RegionList::const_iterator it = _from.begin( ); it != _from.end( ); ++it )
  {
    if((*it)->isBuffer()) {
      for( int i = 0; i < (int) (*it)->size(); ++i ) {
        o.os( ) << "int ruledim_in" << count << "_" << i << " = " << (*it)->matrix( )->name() << ".size(" << i << ");\n";
        o.os( ) << "err |= clSetKernelArg(clkern, " << arg_pos++ << ", sizeof(int), &ruledim_in" << count << "_" << i << " );\n";
      }
      count++;
    }
  }
  o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to bind kernel arguments.\" );\n\n";

    // Invoke kernel.
    o.comment( "Invoke kernel." );


    /*o.os() << "if(";
    for( int i = 0; i < iterdef.dimensions( ); ++i )
    {
        if(i > 0) {
          o.os() << " && ";
        }
      o.os( ) << "workdim[" << i << "] % localdim[" << i <<"] == 0";
    }
    o.os() << ") {\n";*/

    //o.os( ) << "std::cout << \"RUN GPU\" << std::endl;\n";
    //o.write("std::cout << \"queue = \" << GpuManager::_queue << std::endl;");
    if(canUseLocalMemory()) {
      if(iterdef.dimensions( ) == 1) {
        o.os() << "if(use_localmem == 1 && workdim[0] % opencl_blocksize*opencl_blocksize == 0) {\n";
        o.os( ) << "size_t localdim[] = {opencl_blocksize*opencl_blocksize};\n";
        o.os( ) << "int blocksize = opencl_blocksize*opencl_blocksize;\n";
      }
      else {
        o.os() << "if(use_localmem == 1 && workdim[0] % opencl_blocksize == 0 && workdim[1] % opencl_blocksize == 0) {\n";
        o.os( ) << "size_t localdim[] = {opencl_blocksize, opencl_blocksize};\n";
        o.os( ) << "int blocksize = opencl_blocksize;\n";
      }

      o.os( ) << "err = clSetKernelArg(clkern, " << arg_pos++ << ", sizeof(int), &blocksize);\n";
      o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to bind kernel arguments.\" );\n\n";
      o.os( ) << "err = clEnqueueNDRangeKernel(GpuManager::_queue, clkern, " << iterdef.dimensions( ) << ", 0, workdim, localdim, 0, NULL, NULL );\n";
      o.os() << "} else {\n";
      o.os( ) << "err = clEnqueueNDRangeKernel(GpuManager::_queue, clkern, " << iterdef.dimensions( ) << ", 0, workdim, NULL, 0, NULL, NULL );\n";
      o.os() << "}\n";
    }
    else {
      o.os( ) << "err = clEnqueueNDRangeKernel(GpuManager::_queue, clkern, " << iterdef.dimensions( ) << ", 0, workdim, NULL, 0, NULL, NULL );\n";
    }
    //o.os() << "}\n";
    o.write("clFinish(GpuManager::_queue);"); //TODO:clFlush but need to make sure we don't change kernel args before it runs

    #ifndef OPENCL_LOGGING
    o.os( ) << "if( CL_SUCCESS != err ) ";
    #endif
    o.os( ) << "std::cout << \"Kernel execution error #\" << err << \": \" << OpenCLUtil::errorString(err) << std::endl;\n";
    o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to execute kernel.\" );\n";

  o.write( "return NULL;" );
  o.endFunc();
}

void petabricks::UserRule::generateOpenCLCopyOutCode(std::string& codename, CodeGenerator& o, RegionPtr region){
  SRCPOSSCOPE();
  std::string name = region->matrix()->name();
  std::string storage = "storage_" + name;
  std::string dim = jalib::XToString(region->dimensions());
  std::vector<std::string> args;
  args.push_back("std::vector<IndexT*>& begins");
  args.push_back("std::vector<IndexT*>& ends");
  args.push_back("int nodeID");
  o.beginFunc("petabricks::DynamicTaskPtr", codename+"_copyout_"+region->name(), args/*packedargs*/);
  o.write("MatrixStorageInfoPtr "+storage+" = "+name+".storageInfo();");
  o.write("IndexT sizes["+dim+"];");
  o.write("memcpy(sizes, "+storage+"->sizes(), sizeof(sizes));");
  o.write("MatrixStoragePtr outstorage = "+storage+"->getGpuOutputStoragePtr(nodeID);");
  o.write("MatrixRegion<"+dim+", "+STRINGIFY(MATRIX_ELEMENT_T)+"> normalized(outstorage, outstorage->data(), sizes);"); 
  //o.write("outstorage->print();"); 
  //o.write("std::cout << sizes[0] << \" \" << sizes[1] << std::endl;");/
  o.write("normalized.copyTo("+name+", begins, ends);");  
#ifdef GPU_TRACE
  //o.write("MatrixIO().write(normalized);");   
  //o.write("MatrixIO().write("+name+");");   
#endif
  o.write( "return NULL;" );
  o.endFunc();
}

void petabricks::UserRule::generateOpenCLKernel( Transform& trans, CLCodeGenerator& clo, IterationDefinition& iterdef, bool local)
{
  SRCPOSSCOPE();
  // This is only null if code generation failed (that is, the rule is
  // unsupported.)
  if( !isOpenClRule() )
    return;

  //TRACE( "10" );

  std::vector<std::string> from_matrices, to_matrices;
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    to_matrices.push_back( (*i)->name( ) );
  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
    from_matrices.push_back( (*i)->name( ) );

  clo.beginKernel(_to, _from, iterdef.dimensions(), trans, local);

  //TRACE( "20" );

  // Get indices.
  for( int i = 0; i < iterdef.dimensions( ); ++i )
    clo.os( ) << "int " << _getOffsetVarStr( _id, i, NULL ) << " = get_global_id( " << i << " );\n";

  // Define rule index variables (from _definitions).
  for( FormulaList::iterator it = _definitions.begin( ); it != _definitions.end( ); ++it )
    clo.os( ) << STRINGIFY(MATRIX_INDEX_T) " " << (*it)->lhs()->toString() << " = " << (*it)->rhs()->toString() << ";\n";

  // Define Sizes
  //trans.extractOpenClSizeDefines(clo, iterdef.dimensions());

  // Use local memory
  if(local)
    generateLocalBuffers(clo);

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

  //TRACE( "30" );

  // Generate indices into input and output arrays.
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
  {
    // Build & normalize formula for index.
    int minCoordSize = (*i)->minCoord().size();
    FormulaPtr idx_formula = new FormulaAdd((*i)->minCoord().at(minCoordSize - 1), FormulaInteger::zero());
    for( int j = minCoordSize - 2; j >= 0; --j )
    {
      std::stringstream sizevar;
      sizevar << "dim_" << (*i)->name( ) << "_d" << j;
      idx_formula = new FormulaAdd( (*i)->minCoord( ).at( j ),
            new FormulaMultiply( new FormulaVariable( sizevar.str( ) ), idx_formula ) );
    }

    clo.os( ) << "int idx_" << (*i)->name( ) << " = ";
    idx_formula->print( clo.os( ) );
    clo.os( ) << ";\n";
  }
  //TRACE( "40" );
  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
  {
    // Build & normalize formula for index.

    FormulaPtr idx_formula;
    switch((*i)->getRegionType()) {
      case Region::REGION_CELL:
      case Region::REGION_BOX:
        {
        idx_formula = FormulaInteger::zero( );
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
        }
        break;
      case Region::REGION_ROW:
        idx_formula = new FormulaMultiply( new FormulaVariable( "dim_" + (*i)->name( ) + "_d0" ), (*i)->minCoord().at(1) );
        break;
      case Region::REGION_COL:
        idx_formula = (*i)->minCoord().at(0);
        break;
      case Region::REGION_ALL:
        idx_formula = FormulaInteger::zero( );
        break;
      default:
        UNIMPLEMENTED();
/*=======
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
>>>>>>> distributed*/
    }
    clo.os( ) << "int idx_" << (*i)->name( ) << " = ";
    idx_formula->print( clo.os( ) );
    clo.os( ) << ";\n";
  }
  //TRACE( "50" );

  // Load inputs to rule.
  /*for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
  {
    clo.os( ) << "float" << " " << (*i)->name( ) << " = _region_" << (*i)->name( ) << "[idx_" <<
(*i)->name( ) << "];\n";
  }*/

  //TRACE( "60" );

  // Quick hack -- generate a macro that will store the output from the rule.
  {
    RegionList::const_iterator i = _to.begin( );
    clo.os( ) << "#define RETURN(x) _region_" << (*i)->name( ) << "[idx_" << (*i)->name( ) << "] = x; return\n";
  }

  // Support for multiple-output rules
  /*for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
  {
    clo.os() << "#define " << (*i)->name( ) << " _region_" << (*i)->name( ) << "[idx_" << (*i)->name( ) << "]\n";
  }*/

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
  if(local)
    clo.write( _bodyirLocalMem->toString( ) );
  else
    clo.write( _bodyir[RuleFlavor::OPENCL]->toString( ) );

  //TRACE( "70" );

  //clo.os( ) << "OUT[idx_OUT] = IN[idx_IN];\n";

  // Close conditional and kernel.
  if(iterdef.dimensions()>0)
    clo.os( ) << "}\n";
  clo.endKernel( );
}

void petabricks::UserRule::generateLocalBuffers(CLCodeGenerator& clo) {
  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());

  for( int i = 0; i < iterdef.dimensions( ); ++i ) {
    std::string var;
    if(i==0) var = "x";
    else     var = "y";
    clo.os( ) << "int " << var << "_local = get_local_id( " << i << " );\n";
  }

  clo.os() << "int _x_ = " << _getOffsetVarStr( _id, 0, NULL ) << ";\n";
  if(iterdef.dimensions() == 2) 
    clo.os() << "int _y_ = " << _getOffsetVarStr( _id, 1, NULL ) << ";\n";

  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
  {
    if((*i)->isBuffer()) {
      std::string matrix = (*i)->matrix()->name();
      if(_minCoordOffsets.find(matrix) != _minCoordOffsets.end()){
        //FormulaList min = _minCoordOffsets[matrix];
        //FormulaList max = _minCoordOffsets[matrix];
        for(int d = 0; d < iterdef.dimensions( ); d++) {
          clo.os( ) << "int " << matrix << jalib::XToString(d) << "_minoffset = -(" << _minCoordOffsets[matrix][d] << ");\n";
          clo.os( ) << "int " << matrix << jalib::XToString(d) << "_maxoffset = " << _maxCoordOffsets[matrix][d] << " - 1;\n";
        }

         clo.os() << "__local " << STRINGIFY( MATRIX_ELEMENT_T ) << " buff_" << matrix;
        for(int d = iterdef.dimensions( ) - 1; d >= 0; d--) {
          int block_size;
          if(iterdef.dimensions()==0)
            block_size = MAX_BLOCK_SIZE * MAX_BLOCK_SIZE;
          else
            block_size = MAX_BLOCK_SIZE;
          FormulaPtr formula = new FormulaSubtract(_maxCoordOffsets[matrix][d], _minCoordOffsets[matrix][d]);
          formula = new FormulaAdd(new FormulaLiteral<int>(block_size - 1),formula);
          clo.os() << "[" << MAXIMA.normalize(formula) << "]";
        }
        clo.os() << ";\n";

        /*clo.os() << "__local " << STRINGIFY( MATRIX_ELEMENT_T ) << " buff_" << matrix;
        for(int d = iterdef.dimensions( ) - 1; d >= 0; d--) {
          FormulaPtr formula = new FormulaSubtract(_maxCoordOffsets[matrix][d], _minCoordOffsets[matrix][d]);
          formula = new FormulaAdd(new FormulaVariable("block_size"),formula);
          formula = formula->minusOne();
          formula = MAXIMA.normalize(formula);
          clo.os() << "[" << formula << "]";
        }
        clo.os() << ";\n";*/

        if(iterdef.dimensions( )==1) {
          clo.os() << "for(int i = -" << matrix << "0_minoffset; i < 0; i += block_size) {\n";
          clo.os() << "  if(_x_ + i >= 0)\n";
          clo.os() << "    buff_" << matrix << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name() << "[_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "for(int i = " << matrix << "0_maxoffset; i > 0; i -= block_size) {\n";
          clo.os() << "  if(_x_ + i < dim_" << (*i)->name() << "_d0)\n";
          clo.os() << "    buff_" << matrix << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name() << "[_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "buff_" << matrix << "[x_local + " << matrix << "0_minoffset] = "
                              << "_region_" << (*i)->name() << "[_x_];\n";
        }
        else {
          // Middle region
          //clo.os() << "int j = 0;\n";
          clo.os() << "for(int i = -" << matrix << "0_minoffset; i < 0; i += block_size) {\n";
          clo.os() << "  if(_x_ + i >= 0)\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset]"
                                  << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "for(int i = " << matrix << "0_maxoffset; i > 0; i -= block_size) {\n";
          clo.os() << "  if(_x_ + i < dim_" << (*i)->name() << "_d0)\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset]"
                                  << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset]"
                                  << "[x_local + " << matrix << "0_minoffset] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_];\n";

          // Top region
          clo.os() << "for(int j = -" << matrix << "1_minoffset; j < 0; j += block_size) {\n";
          clo.os() << "if(_y_ + j >= 0) {\n";
          clo.os() << "for(int i = -" << matrix << "0_minoffset; i < 0; i += block_size) {\n";
          clo.os() << "  if(_x_ + i >= 0)\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset + j]"
                                  << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_ + j) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "for(int i = " << matrix << "0_maxoffset; i > 0; i -= block_size) {\n";
          clo.os() << "  if(_x_ + i < dim_" << (*i)->name() << "_d0)\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset + j]"
                                  << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_ + j) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset + j]"
                                  << "[x_local + " << matrix << "0_minoffset] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_ + j) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_];\n";
          clo.os() << "}\n";
          clo.os() << "}\n";

          // Bottom region
          clo.os() << "for(int j = " << matrix << "1_maxoffset; j > 0; j -= block_size) {\n";
          clo.os() << "if(_y_ + j < dim_" << (*i)->name() << "_d1) {\n";
          clo.os() << "for(int i = -" << matrix << "0_minoffset; i < 0; i += block_size) {\n";
          clo.os() << "  if(_x_ + i >= 0)\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset + j]"
                                  << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_ + j) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "for(int i = " << matrix << "0_maxoffset; i > 0; i -= block_size) {\n";
          clo.os() << "  if(_x_ + i < dim_" << (*i)->name() << "_d0)\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset + j]"
                                  << "[x_local + " << matrix << "0_minoffset + i] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_ + j) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_ + i];\n";
          clo.os() << "}\n";
          clo.os() << "    buff_" << matrix << "[y_local + " << matrix << "1_minoffset + j]"
                                  << "[x_local + " << matrix << "0_minoffset] = "
                                  << "_region_" << (*i)->name()
                                  << "[(_y_ + j) * dim_" << (*i)->name() << "_d0 + " 
                                  << "_x_];\n";
          clo.os() << "}\n";
          clo.os() << "}\n";

        } // endif 2D

      } // endif gernerate buffer
    }
  }
  clo.os() << "barrier(CLK_LOCAL_MEM_FENCE);\n";
}

#endif

void petabricks::UserRule::generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, RuleFlavor flavor){
  o.comment("MARKER 4");
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
                                            RuleFlavor flavor,
                                            std::vector<RegionNodeGroup>&,
                                            int, int){
  SRCPOSSCOPE();
  o.comment("from UserRule::generateCallCode():");
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
    MaximaWrapper::instance().assume(new FormulaLT(getOffsetVar(i), _applicableRegion->maxCoord()[i]));
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
      for(ChoiceDepGraphNodeSet::iterator a=pNode.begin(); a!=pNode.end(); ++a) {
        for(ChoiceDepGraphNodeSet::iterator b=dNode.begin(); b!=dNode.end(); ++b) {
          if( (*a)->dependencyPossible(*b, d->second->direction()) ) {
            (*a)->addDependency(*b, this, d->second->direction());
          }
        }
      }
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


void petabricks::UserRule::removeDimensionFromRegionList(RegionList& list, 
                                                     const MatrixDefPtr matrix, 
                                                     const size_t dimension) {
  for(RegionList::iterator i=list.begin(), e=list.end(); i!=e; ++i) {
    RegionPtr region = *i;
    
    const MatrixDefPtr& fromMatrix = region->matrix();
    
    if(fromMatrix != matrix) {
      continue;
    }
    
    region->removeDimension(dimension);
  }
}


void petabricks::UserRule::removeDimensionFromMatrixDependencyMap(MatrixDependencyMap& map,
                                                      const MatrixDefPtr matrix,
                                                      const size_t dimension) {
  MatrixDependencyMap::iterator dependencyIterator = map.find(matrix);
  if(dependencyIterator == map.end()) {
    //No dependencies to remove
    return;
  }
  
  MatrixDependencyPtr dependency = dependencyIterator->second;
  
  dependency->removeDimension(dimension);
}


void petabricks::UserRule::removeDimensionFromDefinitions(const size_t dimension) {
  FormulaPtr offsetVar = getOffsetVar(dimension);
  
  for(FormulaList::iterator i=_definitions.begin(), e=_definitions.end();
      i != e;
      ++i) {
    FormulaPtr definition = *i;
    FormulaPtr definingVar = definition->rhs();
    if(definingVar->toString() != offsetVar->toString()) {
      //It's not the variable we are looking for
      continue;
    }
    
    //It's the variable we are looking for
    //Let's erase it!
    _definitions.erase(i);
    return;
  }
}


void petabricks::UserRule::removeDimensionFromMatrix(const MatrixDefPtr matrix,
                                                      const size_t dimension) {
  removeDimensionFromRegionList(_to, matrix, dimension);
  removeDimensionFromRegionList(_from, matrix, dimension);
  
  removeDimensionFromDefinitions(dimension);
}
  

void petabricks::UserRule::trimDependency(DependencyDirection& dep,
                                          const ChoiceDepGraphNode& from,
                                          const ChoiceDepGraphNode& to)
{
  if(dep.isMultioutput() && _to.size() <= 1) {
    dep.removeMultioutput(); 
  }
}

void petabricks::DataDependencyVectorMap::print(std::ostream& o) const {
  o << "DataDependencyVectorMap: ";
  for(DataDependencyVectorMap::const_iterator i=this->begin(), e=this->end(); 
      i!=e; 
      ++i) {
    MatrixDefPtr matrixDef = i->first;
    const CoordinateFormula& dependencyVector = i->second;
    o << "\n  MatrixDef: " << matrixDef;
    o << "\t\tDependency vector: " << dependencyVector;
  }
}

namespace {
  void fixVersionedRegionsTypeInList(petabricks::RegionList list) {
    for(petabricks::RegionList::iterator i=list.begin(), e=list.end(); i!=e; ++i) {
      petabricks::RegionPtr region = *i;
    
      region->fixTypeIfVersioned();
    }
  }
}

void petabricks::UserRule::fixVersionedRegionsType() {
  fixVersionedRegionsTypeInList(_to);
  fixVersionedRegionsTypeInList(_from);
}

petabricks::RegionList petabricks::UserRule::getSelfDependentRegions() {
  RegionList list = RegionList();
    
  for(RegionList::iterator in=_from.begin(), in_end=_from.end(); 
      in!=in_end;
      ++in) {
    
    for (RegionList::iterator out=_to.begin(), out_end=_to.end();
         out != out_end;
         ++out)
         {
      if((*in)->matrix()->name() == (*out)->matrix()->name()) {
        list.push_back(*in);
      }
    }
  }
  return list;
}

petabricks::RegionList petabricks::UserRule::getNonSelfDependentRegions() {
  RegionList list = RegionList();
    
  //Add regions from _from, not in _to
  for(RegionList::iterator in=_from.begin(), in_end=_from.end(); 
      in!=in_end;
      ++in) {
    
    for (RegionList::iterator out=_to.begin(), out_end=_to.end();
         out != out_end;
         ++out)
         {
      if((*in)->matrix()->name() != (*out)->matrix()->name()) {
        list.push_back(*in);
      }
    }
  }
    
  //Add regions from _to, not in _from
  for(RegionList::iterator out=_to.begin(), out_end=_to.end(); 
      out!=out_end;
      ++out) {
    
    for (RegionList::iterator in=_from.begin(), in_end=_from.end();
         in != in_end;
         ++in)
         {
      if((*in)->matrix()->name() != (*out)->matrix()->name()) {
        list.push_back(*in);
      }
    }
  }
  
  return list;
}
