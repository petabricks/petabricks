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
#ifndef PETABRICKSITERATIONORDERS_H
#define PETABRICKSITERATIONORDERS_H

#include "formula.h"
#include "matrixdependency.h"

#include <string>
#include <vector>

namespace petabricks {
class CodeGenerator;
class RuleInterface;
class Transform;
class SplitRegion;
typedef std::vector<SplitRegion> SplitRegionList;

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
  

  void genSplitCode(CodeGenerator& o, Transform& trans, RuleInterface& rule, bool isStatic) const;

protected:
  void fillSplitRegionList(SplitRegionList& regions, SplitRegion& seed) const;
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
