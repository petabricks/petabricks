#include "gpurule.h"

#ifdef HAVE_OPENCL

namespace petabricks
{

void
GpuRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o)
{
  CLCodeGenerator clcodegen;
  IterationDefinition iterdef(*_rule, _rule->getSelfDependency(), _rule->isSingleCall());
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::vector<std::string> packedargnames = iterdef.packedargnames();

  o.os() << "// GPURULE TRAMPOLINE CODE\n";

  // Create variables to hold handles to program, kernel
  o.hos() << "static cl_kernel clkern_" << _rule->id() << ";\n\n";
  o.os( ) << "cl_program " << trans.name() << "_instance::clprog_" << codename()
	  << " = 0;\n";
  o.os( ) << "cl_kernel " << trans.name() << "_instance::clkern_" << codename()
	  << " = 0;\n";

  // Create init function call
  o.beginFunc("int", codename()+"_init", std::vector<std::string>(),true);

  _rule->generateOpenCLKernel( trans, clcodegen, iterdef );

  o.os( ) << "cl_int err;";

  o.os( ) << "/* -- Testing purposes only, to make this easy to read --\n\n";
  clcodegen.outputStringTo( o.os( ) );
  o.os( ) << "\n*/\n";

  o.os( ) << "const char* clsrc = ";
  clcodegen.outputEscapedStringTo( o.os( ) );
  o.os( ) << ";\n";

  o.comment( "Source for kernel." );
  o.os( ) << "cl_context ctx = OpenCLUtil::getContext( );\n\n";

  o.comment( "Build program." );
  o.os( ) << "size_t programlength = strlen( clsrc );\n";
  o.os( ) << "clprog_" << codename() << " = clCreateProgramWithSource( ctx, 1, &clsrc, NULL, &err );\n";
  o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create program.\" );\n\n";
  o.os( ) << "err = OpenCLUtil::buildProgram( clprog_" << codename() << " );\n";
  o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to build program.\" );\n\n";

  o.comment( "Create kernel." );
  o.os( ) << "clkern_" << codename() << "= clCreateKernel( clprog_" << codename() << ", \"kernel_main\", &err );\n";
  o.os( ) << "std::cout << \"clCreateKernel err #\" << err << \": \" << OpenCLUtil::errorString( err ) << std::endl;\n";
  o.os( ) << "JASSERT( CL_SUCCESS == err ).Text( \"Failed to create kernel.\" );\n\n";

  o.os( ) << "return 0;";
  o.endFunc();

  // Create actual function call
  o.beginFunc("petabricks::DynamicTaskPtr", codename(), packedargs);
  o.write("return ");
  o.call(_rule->trampcodename(trans)+TX_OPENCL_POSTFIX, packedargnames);
  o.endFunc();

  // Invoke init once before main.
  o.os() << "static int ignored_" << codename() << " = " << trans.name() << "_instance::" << codename() << "_init();\n\n";  
}

void
GpuRule::generateCallCodeSimple(Transform& /*trans*/, CodeGenerator& o, const SimpleRegionPtr& region)
{
  o.callSpatial(codename(), region);
}

void
GpuRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region)
{
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

};

#endif
