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
#ifndef PETABRICKSGPURULE_H
#define PETABRICKSGPURULE_H

#include "pbc.h"

#include "syntheticrule.h"
#include "userrule.h"
#include "matrixdependency.h"
#include "clcodegenerator.h"
#include "iterationorders.h"
#include "maximawrapper.h"
#include "transform.h"

#include <set>

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

  void generateDeclCode(Transform& trans, CodeGenerator& o, RuleFlavor rf);
  void generateKernel(Transform& trans, CodeGenerator& o, bool local);

  void generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor);

  void generateCallCode(const std::string& nodename,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor,
                        std::vector<RegionNodeGroup>& regionNodesGroups,
                        int nodeID,
                        int gpuCopyOut);
  //TODO: remove this
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

  bool isEnabledGpuRule() { return !isDisabled(); }
  int getAssociatedId() { return _rule->id(); }
  
  static std::set<int> _done;
 private:
  UserRule* _rule;
  RIRBlockCopyRef _bodyirOpenCL;
};

}

#endif
