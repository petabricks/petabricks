#include "gpurule.h"

#ifdef HAVE_OPENCL

namespace petabricks
{

void
GpuRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o)
{
  o.os() << "// GPURULE TRAMPOLINE CODE\n";

  IterationDefinition iterdef(*_rule, _rule->getSelfDependency(), _rule->isSingleCall());
  std::vector<std::string> packedargs = iterdef.packedargs();
  std::vector<std::string> packedargnames = iterdef.packedargnames();

  o.beginFunc("petabricks::DynamicTaskPtr", codename(), packedargs);
  o.write("return ");
  o.call(_rule->trampcodename(trans)+TX_OPENCL_POSTFIX, packedargnames);
  o.endFunc();
  
}

void
GpuRule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region)
{
}

void
GpuRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region)
{
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
