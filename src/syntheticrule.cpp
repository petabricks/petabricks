/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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

#include "syntheticrule.h"
  
void petabricks::SyntheticRule::initialize(Transform&){}
void petabricks::SyntheticRule::compileRuleBody(Transform& tx, RIRScope& s){}

petabricks::RuleFlags::PriorityT petabricks::SyntheticRule::priority() const { 
  return RuleFlags::PRIORITY_DEFAULT; 
}
bool petabricks::SyntheticRule::isRecursive() const { 
  return true;
}
bool petabricks::SyntheticRule::hasWhereClause() const { 
  return false; 
}

bool petabricks::SyntheticRule::canProvide(const MatrixDefPtr& m) const { 
  UNIMPLEMENTED(); 
}
bool petabricks::SyntheticRule::isSingleElement() const { 
  UNIMPLEMENTED(); 
}

void petabricks::SyntheticRule::collectDependencies(StaticScheduler& scheduler) { 
  UNIMPLEMENTED(); 
}
void petabricks::SyntheticRule::getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension) { 
  UNIMPLEMENTED(); 
}

void petabricks::SyntheticRule::generateCallCodeSimple(Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region){
}
void petabricks::SyntheticRule::generateCallTaskCode(const std::string& name, Transform& trans, CodeGenerator& o, const SimpleRegionPtr& region) {
}
void petabricks::SyntheticRule::generateDeclCodeSimple(Transform& trans, CodeGenerator& o) {
}
void petabricks::SyntheticRule::generateTrampCodeSimple(Transform& trans, CodeGenerator& o) {
}

void petabricks::SyntheticRule::markRecursive() { 
  UNIMPLEMENTED();
}
const petabricks::FormulaPtr& petabricks::SyntheticRule::recursiveHint() const { 
  static FormulaPtr t = new FormulaVariable(INPUT_SIZE_STR);  
  return t;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////





