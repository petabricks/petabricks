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
#ifndef PETABRICKSITERATIONORDERS_H
#define PETABRICKSITERATIONORDERS_H

#include "formula.h"
#include "matrixdependency.h"
#include "pbc.h"

#include <string>
#include <vector>

namespace petabricks {
class CodeGenerator;
class RuleInterface;
class Transform;
class SplitRegion;

class SplitRegionList : public std::vector<SplitRegion> , public jalib::JRefCounted, public jalib::SrcPosTaggable, public jalib::JPrintable {
public:
  void print(std::ostream& o) const;
};


class IterationDefinition {
public:
  IterationDefinition(RuleInterface& rule, const DependencyDirection& order, bool isSingleCall);

  void genLoopBegin(CodeGenerator& o);
  void genLoopEnd(CodeGenerator& o);

  DependencyDirection& order() { return _order;}
  CoordinateFormula&  begin() { return _begin;}
  CoordinateFormula&  end  () { return _end;}
  CoordinateFormula&  step () { return _step;}
  const DependencyDirection& order() const { return _order;}
  const CoordinateFormula&  begin() const { return _begin;}
  const CoordinateFormula&  end  () const { return _end;}
  const CoordinateFormula&  step () const { return _step;}

  bool isSingleCall() const { return _step.empty(); }

  std::vector<std::string> args() const;
  std::vector<std::string> argnames() const;

  int dimensions() const { return (int) _var.size(); }

  std::vector<std::string> packedargs() const;
  std::vector<std::string> packedargnames() const;
  void unpackargs(CodeGenerator& o) const;
  

  void genSplitCode(CodeGenerator& o, Transform& trans, RuleInterface& rule, RuleFlavor rf, unsigned int blockNumber) const;

protected:
  void fillSplitRegionList(SplitRegionList& regions, SplitRegion& seed, unsigned int blockNumber) const;
  bool canDependOn(const SplitRegion& a, const SplitRegion& b) const;
private:
  DependencyDirection _order;
  CoordinateFormula   _var;
  CoordinateFormula   _begin;
  CoordinateFormula   _end;
  CoordinateFormula   _step;
};

}

#endif
