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
#include "gpurule.h"

#ifdef HAVE_OPENCL

namespace petabricks
{

void
GpuRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o)
{
  if( !_rule->isOpenClRule() )
    return;

  CLCodeGenerator clcodegen(o.cgPtr());
  IterationDefinition iterdef(*_rule, _rule->getSelfDependency(), _rule->isSingleCall());
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::vector<std::string> packedargnames = iterdef.packedargnames();

  o.os() << "// GPURULE TRAMPOLINE CODE\n";

  // Create variables to hold handles to program, kernel
  o.hos() << "static cl_program clprog_" << _rule->id() << ";\n";
  o.hos() << "static cl_kernel clkern_" << _rule->id() << ";\n\n";
  o.os( ) << "cl_program " << trans.name() << "_instance::clprog_" << _rule->id()
	  << " = 0;\n";
  o.os( ) << "cl_kernel " << trans.name() << "_instance::clkern_" << _rule->id()
	  << " = 0;\n";

  // Create init function call
  o.beginFunc("int", codename()+"_init", std::vector<std::string>(),true);

  _rule->generateOpenCLKernel( trans, clcodegen, iterdef );

  o.os( ) << "cl_int err;";

  //o.os( ) << "/* -- Testing purposes only, to make this easy to read --\n\n";
  //clcodegen.outputStringTo( o.os( ) );
  //o.os( ) << "\n*/\n";

  o.os( ) << "const char* clsrc = ";
  clcodegen.outputEscapedStringTo( o.os( ) );
  o.os( ) << ";\n";

  o.comment( "Source for kernel." );
  o.os( ) << "cl_context ctx = OpenCLUtil::getContext( );\n\n";

  o.comment( "Build program." );
  o.os( ) << "size_t programlength = strlen( clsrc );\n";
  //TODO: fail to create program
  o.os( ) << "clprog_" << _rule->id() << " = clCreateProgramWithSource( ctx, 1, (const char **)&clsrc, NULL, &err );\n";
  o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create program.\" );\n\n";
  //o.os( ) << "err = OpenCLUtil::buildProgram( clprog_" << _rule->id() << " );\n";
  //ciErrNum = clBuildProgram(cpProgram, 0, NULL, "-cl-fast-relaxed-math", NULL, NULL);
  o.os( ) << "err = clBuildProgram( clprog_" << _rule->id() << ", 0, NULL, NULL, NULL, NULL);\n";
  o.os( ) << "std::cout << \"clBuildProgram err #\" << err << \": \" << OpenCLUtil::errorString( err ) << std::endl;\n";
  o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to build program.\" );\n\n";

  o.comment( "Create kernel." );
  o.os( ) << "clkern_" << _rule->id() << "= clCreateKernel( clprog_" << _rule->id() << ", \"kernel_main\", &err );\n";
  o.os( ) << "#if OPENCL_TRACE\nstd::cout << \"clCreateKernel err #\" << err << \": \" << OpenCLUtil::errorString( err ) << std::endl;\n#endif\n";
  o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create kernel.\" );\n\n";

  o.os( ) << "return 0;";
  o.endFunc();

  // Create actual function call
  o.beginFunc("petabricks::DynamicTaskPtr", codename(), packedargs);
  o.write("return ");
  o.call(_rule->trampcodename(trans)+TX_OPENCL_POSTFIX, packedargnames);
  o.endFunc();

  // Invoke init once before main.
  o.os() << "static int ignored_" 
         << trans.name() 
         << '_' 
         << codename() 
         << " = " 
         << trans.name() 
         << "_instance::" 
         << codename() 
         << "_init();\n\n";  
}

void
GpuRule::generateCallCodeSimple(Transform& /*trans*/, CodeGenerator& o, const SimpleRegionPtr& region)
{
  o.comment( "GENERATECALLCODESIMPLE" );
  o.callSpatial(codename(), region);
}

void
GpuRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region)
{
  o.comment( "GENERATECALLTASKCODE" );
  o.mkSpatialTask(name, trans.instClassName(), codename(), region);
}

  /*
void petabricks::UserRule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  o.callSpatial(trampcodename(trans)+TX_STATIC_POSTFIX, region);
}

void petabricks::UserRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
  o.mkSpatialTask(name, trans.instClassName(), trampcodename(trans)+TX_DYNAMIC_POSTFIX, region);
}
  */

bool
GpuRule::canProvide(const MatrixDefPtr& m) const
{
  return _rule->canProvide(m);
}

void
GpuRule::getApplicableRegionDescriptors(RuleDescriptorList& rdl, const MatrixDefPtr& md, int i, const RulePtr& rule)
{
  _rule->getApplicableRegionDescriptors(rdl, md, i, rule);
}

bool
GpuRule::isSingleElement() const
{
  return _rule->isSingleElement();
}

int
GpuRule::dimensions() const
{
  return _rule->dimensions();
}

FormulaPtr
GpuRule::getSizeOfRuleIn(int d)
{
  return _rule->getSizeOfRuleIn(d);
}

std::string
GpuRule::codename() const
{
  return "gpuRule"+jalib::XToString(_id);
}

void
GpuRule::collectDependencies(StaticScheduler& scheduler)
{
  return _rule->collectDependencies(scheduler);
}

  /*
void
GpuRule::genWhereSwitch(Transform& trans, CodeGenerator& o)
{
  return _rule->genWhereSwitch(trans,o);
}
  */

DependencyDirection
GpuRule::getSelfDependency() const
{
  return _rule->getSelfDependency();
}

petabricks::RuleFlags::PriorityT petabricks::GpuRule::priority() const { 
  return _rule->priority();
}
bool petabricks::GpuRule::isRecursive() const { 
  return _rule->isRecursive();
}
bool petabricks::GpuRule::hasWhereClause() const { 
  return _rule->hasWhereClause();
}
petabricks::FormulaPtr petabricks::GpuRule::getWhereClause() const { 
  return _rule->getWhereClause();
}

}//namespace

#endif
