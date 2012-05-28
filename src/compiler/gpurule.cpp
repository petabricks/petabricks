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

//#define GPU_TRACE

namespace petabricks
{
std::set<int> GpuRule::_done;
void GpuRule::generateDeclCode(Transform& trans, CodeGenerator& o, RuleFlavor rf) {
  if(rf != RuleFlavor::SEQUENTIAL || isDisabled() || _done.find(_rule->id()) != _done.end())
    return;
    
  _done.insert(_rule->id());

  generateKernel(trans, o, false);
  if(_rule->canUseLocalMemory()) {
    generateKernel(trans, o, true);
    //o.createTunable(true, "system.flag.localmem", "rule_" + _rule->id() + "_localmem", 0, 0, 2);
    //o.createTunable(true, "system.size.blocksize", "rule_" + _rule->id() + "_blocksize", 4, 0, 5);
  }
  //else {
    //o.define("rule_" + _rule->id() + "_localmem", "0");
  //}
}

void GpuRule::generateKernel(Transform& trans, CodeGenerator& o, bool local) {
  std::string SUFFIX;
  if(local)
    SUFFIX = "_local";
  else
    SUFFIX = "_nolocal";

  CLCodeGenerator clcodegen(o.cgPtr());
  IterationDefinition iterdef(*_rule, _rule->getSelfDependency(), _rule->isSingleCall());
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::vector<std::string> packedargnames = iterdef.packedargnames();
  o.os() << "// GPURULE DECL CODE " << _rule->id() << " " << this << "\n";

  // Create variables to hold handles to program, kernel
  o.os( ) << "cl_program " <<  "clprog_" << _rule->id() << SUFFIX
	  << " = 0;\n";
  o.os( ) << "cl_kernel " << "clkern_" << _rule->id() << SUFFIX
	  << " = 0;\n";

  // Create init function call
  o.beginFunc("void", codename()+"_init"+SUFFIX, std::vector<std::string>(),false);
  trans.addInitCall(codename()+"_init"+SUFFIX);

  _rule->generateOpenCLKernel( trans, clcodegen, iterdef, local);

  //o.os( ) << "/* -- Testing purposes only, to make this easy to read --\n\n";
  //clcodegen.outputStringTo( o.os( ) );
  //o.os( ) << "\n*/\n";

  o.os( ) << "const char* clsrc = ";
  clcodegen.outputEscapedStringTo( o.os( ) );
  o.os( ) << ";\n";

  o.os() << "bool rv = OpenCLUtil::buildKernel(clprog_" << _rule->id() << SUFFIX << ", clkern_" << _rule->id() << SUFFIX << ", clsrc);\n";
  o.os() << "JASSERT(rv);\n";

  o.endFunc();

  // Get kernel
  o.beginFunc("cl_kernel", "get_kernel_" + jalib::XToString(_rule->id()) + SUFFIX);
#ifdef DEBUG
  o.write("JASSERT(clkern_" + jalib::XToString(_rule->id()) + SUFFIX + " != 0);");
#endif
  o.write("return clkern_" + jalib::XToString(_rule->id()) + SUFFIX + ";");
  o.endFunc();

  // Get program
  o.beginFunc("cl_program", "get_program_" + jalib::XToString(_rule->id()) + SUFFIX);
#ifdef DEBUG
  o.write("JASSERT(clprog_" + jalib::XToString(_rule->id()) + SUFFIX + " != 0);");
#endif
  o.write("return clprog_" + jalib::XToString(_rule->id()) + SUFFIX + ";");
  o.endFunc();
  
}

void GpuRule::generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor)
{
  if(isDisabled())
    return;
  o.os() << "// GPURULE TRAMP CODE " << _rule->id() << "\n";
  switch(flavor) {
  case RuleFlavor::SEQUENTIAL:
    _rule->generateTrampCode(trans, o, RuleFlavor::SEQUENTIAL_OPENCL);
    break;
  case RuleFlavor::WORKSTEALING:
    _rule->generateTrampCode(trans, o, RuleFlavor::WORKSTEALING_OPENCL);
    break;
  case RuleFlavor::DISTRIBUTED:
    _rule->generateTrampCode(trans, o, RuleFlavor::DISTRIBUTED_OPENCL);
    break;
  default:
    UNIMPLEMENTED();
  }
}

void GpuRule::generateCallCode(const std::string& name,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor,
                        std::vector<RegionNodeGroup>& regionNodesGroups,
                        int nodeID,
                        int gpuCopyOut)
{
  o.comment("gpu generateCallCode");
  switch(flavor) {
  case RuleFlavor::SEQUENTIAL:
    o.callSpatial(_rule->trampcodename(trans)+TX_OPENCL_POSTFIX, region);
    break;
  case RuleFlavor::WORKSTEALING:
    o.mkCreateGpuSpatialMethodCallTask(name, trans.instClassName() + "_workstealing", _rule->trampcodename(trans)+TX_OPENCL_POSTFIX+"_createtasks", region, regionNodesGroups, nodeID, gpuCopyOut);
    break;
  case RuleFlavor::DISTRIBUTED:
    o.comment("gpurule::distributed");
    break;
  default:
    UNIMPLEMENTED();
  }
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

