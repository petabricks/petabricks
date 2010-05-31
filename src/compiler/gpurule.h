#ifndef PETABRICKSGPURULE_H
#define PETABRICKSGPURULE_H

#include "pbc.h"

#ifdef HAVE_OPENCL

#include "syntheticrule.h"
#include "userrule.h"
#include "matrixdependency.h"
#include "clcodegenerator.h"
#include "iterationorders.h"
#include "maximawrapper.h"
#include "transform.h"

namespace petabricks
{

class GpuRule : public SyntheticRule {
 public:

    GpuRule( UserRule* rule )
      : _rule( rule )
    {
      _bodyirOpenCL = _rule->getBody( );
    }

    // Overridden functions

  void generateTrampCodeSimple(Transform& trans, CodeGenerator& o);

  void generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);
  void generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region);

  bool isSingleElement() const;

  int dimensions() const;
  FormulaPtr getSizeOfRuleIn(int d);

  std::string codename() const;

  void collectDependencies(StaticScheduler& scheduler);

  void getApplicableRegionDescriptors(RuleDescriptorList& output,
                                      const MatrixDefPtr& matrix, int dimension, const RulePtr&);

  bool canProvide(const MatrixDefPtr& m) const;

  //void genWhereSwitch(Transform& trans, CodeGenerator& o);

  DependencyDirection getSelfDependency() const;

  // New helper functions

  void generateOpenCLKernel( Transform& trans, CLCodeGenerator& clo, IterationDefinition& iterdef );

  // Helper code copied from UserRule
  std::string trampcodename(Transform& trans) const;
  
  
  
  
  RuleFlags::PriorityT priority() const;
  bool isRecursive() const;
  bool hasWhereClause() const;
  FormulaPtr getWhereClause() const;
 private:
  UserRule* _rule;
  RIRBlockCopyRef _bodyirOpenCL;
};

}

#endif
#endif
