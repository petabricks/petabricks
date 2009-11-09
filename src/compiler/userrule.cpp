/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#include "userrule.h"

#include "maximawrapper.h"
#include "rircompilerpass.h"
#include "staticscheduler.h"
#include "transform.h"

#include "common/jconvert.h"

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

  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    SimpleRegionPtr ar = (*i)->getApplicableRegion(trans, *this, _definitions, true);
    if(_applicableRegion)
      _applicableRegion = _applicableRegion->intersect(ar);
    else
      _applicableRegion = ar;
  }
  for(RegionList::iterator i=_from.begin(); i!=_from.end(); ++i){
    SimpleRegionPtr ar = (*i)->getApplicableRegion(trans, *this, _definitions, false);
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
    (*i)->collectDependencies(trans, *this,_depends);
  }
  for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
    (*i)->collectDependencies(trans, *this,_provides);
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
    for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
      if(i->shouldPass())
        o.addMember("const IndexT", i->name());
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
    RIRBlockCopyRef bodytmp = _bodyirDynamic;
    {
      LiftVardeclPass p3(trans,*this, o);
      bodytmp->accept(p3);
    }
    { 
      DynamicBodyPrintPass dbpp(o);
      bodytmp->accept(dbpp);
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
  for(ConfigItems::const_iterator i=trans.config().begin(); i!=trans.config().end(); ++i){
    if(i->shouldPass())
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
  o.write(_bodyirStatic->toString());
  o.undefineAll();
  o.endFunc();
}

void petabricks::UserRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o, RuleFlavor flavor){
  IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
  std::vector<std::string> taskargs = iterdef.packedargs();
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::vector<std::string> packedargnames = iterdef.packedargnames();

  taskargs.insert(taskargs.begin(), "const jalib::JRef<"+trans.instClassName()+"> transform");

  switch( flavor )
    {
    case E_RF_STATIC:
      o.beginFunc("petabricks::DynamicTaskPtr", trampcodename(trans)+TX_STATIC_POSTFIX, packedargs);
      break;
    case E_RF_DYNAMIC:
      o.beginFunc("petabricks::DynamicTaskPtr", trampcodename(trans)+TX_DYNAMIC_POSTFIX, packedargs);
      break;
    #ifdef HAVE_OPENCL
    case E_RF_OPENCL:
      o.beginFunc("petabricks::DynamicTaskPtr", trampcodename(trans)+TX_OPENCL_POSTFIX, packedargs);
      break;
    #endif
    default:
      UNIMPLEMENTED( );
    }

  if((E_RF_DYNAMIC == flavor) && !isRecursive() && !isSingleElement()){
    //shortcut
    o.comment("rule is a leaf, no sense in dynamically scheduling it");
    o.write("return");
    o.call(trampcodename(trans)+TX_STATIC_POSTFIX, packedargnames);
  }
  #ifdef HAVE_OPENCL
  else if( E_RF_OPENCL == flavor )
    {
      // Generate CL program
      CLCodeGenerator clcodegen;
      generateOpenCLKernel( trans, clcodegen, iterdef );

      o.os( ) << "/* -- Testing purposes only, to make this easy to read --\n";
      clcodegen.outputStringTo( o.os( ) );
      o.os( ) << "\n*/\n";

      o.os( ) << "const char* clsrc = ";
      clcodegen.outputEscapedStringTo( o.os( ) );
      o.os( ) << ";\nsize_t clsrclen = strlen( clsrc );\n";

      // Build program and create kernel
      o.os( ) << "cl_program clprog = clCreateProgramWithSource( OpenCLUtil::getContext( ), 1, &clsrc, &clsrclen, NULL );\n";
      o.os( ) << "clBuildProgram( clprog, 0, NULL, NULL, NULL, NULL );\n";
      o.os( ) << "cl_kernel clkern = clCreateKernel( clprog, \"kernel_main\", NULL );\n";

      // Trampoline will do copy-in/copy-out and invoke kernel
      generateTrampCellCodeSimple( trans, o, E_RF_OPENCL );
    }
  #endif
  else {
    if(E_RF_STATIC != flavor) o.write("DynamicTaskPtr _spawner = new NullDynamicTask();");

    iterdef.unpackargs(o);
    
    if(isSingleElement()){
      trans.markSplitSizeUse(o);
      o.beginIf("petabricks::split_condition<"+jalib::XToString(dimensions())+">("SPLIT_CHUNK_SIZE","COORD_BEGIN_STR","COORD_END_STR")");
      iterdef.genSplitCode(o, trans, *this, flavor==E_RF_STATIC);
      // return written in get split code
      o.elseIf();
    }

    iterdef.genLoopBegin(o);
    if( ( E_RF_DYNAMIC == flavor ) && !isRecursive( ) )
      generateTrampCellCodeSimple( trans, o, E_RF_STATIC );
    else
      generateTrampCellCodeSimple( trans, o, flavor );
    iterdef.genLoopEnd(o);
    
    if(E_RF_STATIC != flavor) o.write("return _spawner;");
    else o.write("return NULL;");
    
    if(isSingleElement())
      o.endIf();
  }
  o.endFunc();
}

#ifdef HAVE_OPENCL

void petabricks::UserRule::generateOpenCLKernel( Transform& trans, CLCodeGenerator& clo, IterationDefinition& iterdef )
{
  std::vector<std::string> from_matrices, to_matrices;
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    to_matrices.push_back( (*i)->matrix( )->name( ) );
  for( RegionList::const_iterator i = _from.begin( ); i != _from.end( ); ++i )
    from_matrices.push_back( (*i)->matrix( )->name( ) );

  clo.beginKernel( to_matrices, from_matrices, iterdef.dimensions( ) );

  // Get indices.
  for( int i = 0; i < iterdef.dimensions( ); ++i )
    clo.os( ) << "unsigned int " << _getOffsetVarStr( _id, i, NULL ) << " = get_global_id( " << i << " );\n";

  // Conditional to ensure we are about to work on a valid part of the buffer.
  clo.os( ) << "if( ";
  for( int i = 0; i < iterdef.dimensions( ); ++i )
    {
      clo.os( ) << "pos_d" << i << " < dim_d" << i << " ";
      if( i != ( iterdef.dimensions( ) - 1 ) )
	clo.os( ) << "&& ";
    }
  clo.os( ) << ") {\n";

  // Generate indices into input and output arrays.
  for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i )
    {
      clo.os( ) << "unsigned int idx_" << (*i)->matrix( )->name( ) << " = ";
      //	      << (*i)->generateAccessorCode( )
      //	      << (*i)->minCoord( )
      (*i)->minCoord( ).print( clo.os( ) );
      clo.os( ) << ";\n";
    }

  // Generate OpenCL implementation of rule logic.
  /** \todo */

  // Close conditional and kernel.
  clo.os( ) << "}\n";
  clo.endKernel( );
}

#endif

void petabricks::UserRule::generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, RuleFlavor flavor){

#ifdef HAVE_OPENCL
  // temporary
  if( E_RF_OPENCL == flavor )
  {
      o.write( "return NULL;" );
      return;
  }
#endif

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

  for(int i=0; i<dimensions(); ++i)
    args.push_back(getOffsetVar(i)->toString());

  if(E_RF_STATIC != flavor){
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

void petabricks::UserRule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  o.callSpatial(trampcodename(trans)+TX_STATIC_POSTFIX, region);
}

void petabricks::UserRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  o.mkSpatialTask(name, trans.instClassName(), trampcodename(trans)+TX_DYNAMIC_POSTFIX, region);
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

petabricks::DependencyDirection petabricks::UserRule::getSelfDependency() const {
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
  return trans.name()+"_apply_rule" + jalib::XToString(_id-trans.ruleIdOffset());
}
