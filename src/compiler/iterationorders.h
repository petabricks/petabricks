/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
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
