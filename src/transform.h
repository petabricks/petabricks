/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
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
#ifndef HECURATRANSFORM_H
#define HECURATRANSFORM_H

#include "jrefcounted.h"
#include "jprintable.h"
#include "matrixdef.h"
#include "choicegrid.h"
#include "rule.h"
#include "learner.h"
#include "performancetester.h"

#include <vector>
#include <set>

namespace hecura {

class Transform;
typedef jalib::JRef<Transform> TransformPtr;
class TransformList : public std::vector<TransformPtr> , public jalib::JRefCounted {};
typedef jalib::JRef<TransformList> TransformListPtr;
typedef std::set<std::string> ConstantSet;

/**
 * a transformation algorithm
 */
class Transform : public jalib::JRefCounted, public jalib::JPrintable {
public:
  ///
  /// Constructor
  Transform(const char* name) : _name(name),_isMain(false),_tuneId(0) {}
  
  //called durring parsing:
  void addFrom(const MatrixDefList&);
  void addThrough(const MatrixDefList&);
  void addTo(const MatrixDefList&);
  void setRules(const RuleList&);
  
  ///
  /// Initialize after parsing
  void initialize();

  void print(std::ostream& o) const;

  const std::string& name() const { return _name; }
  
  MatrixDefPtr lookupMatrix(const std::string& name) const{
    MatrixDefMap::const_iterator i = _matrices.find(name);
    JASSERT(i != _matrices.end())(name).Text("Unknown input/output matrix");
    return i->second;
  }

  void generateCodeSimple(CodeGenerator& o);

  void generateMainCode(CodeGenerator& o);

  void fillBaseCases(const MatrixDefPtr& matrix);
  
  const FreeVars& constants() const { return _constants; }
  FreeVars& constants() { return _constants; }

  void extractSizeDefines(CodeGenerator& o);

  void markMain() { _isMain=true; }

  Learner& learner() { return _learner; }
  PerformanceTester& tester() { return _tester; }

  void addTestCase(const TestCasePtr& p) {tester().addTestCase(p);}

  std::vector<std::string> maximalArgList() const;

  std::string createTunerPrefix(){
    return _name + "_" + jalib::XToString(_tuneId++) + "_";
  }

  int ruleIdOffset() const { return _rules.front()->id()-1; }


  std::string taskname() const { return _name+"_fin"; }
private:
  std::string   _name;
  MatrixDefList _from;
  MatrixDefList _through;
  MatrixDefList _to;
  MatrixDefMap  _matrices;
  RuleList      _rules;
  ChoiceGridMap _baseCases;
  FreeVars      _constants;
  bool          _isMain;
  Learner       _learner;
  PerformanceTester _tester;
  int _tuneId;
};

}

#endif
