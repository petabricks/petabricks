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
#ifndef PETABRICKSREGION_H
#define PETABRICKSREGION_H

#include "jrefcounted.h"
#include "jprintable.h"
#include "formula.h"
#include "matrixdef.h"

#include <vector>

namespace petabricks {
class RuleInterface;
class UserRule;
class CodeGenerator;
class Transform;
class SimpleRegion;
class Region;
class MatrixDependencyMap;
typedef jalib::JRef<Region> RegionPtr;
typedef jalib::JRef<SimpleRegion> SimpleRegionPtr;
typedef std::vector<int> IterationOrderList;
class RegionList : public std::vector<RegionPtr> , public jalib::JRefCounted {
public:
  void makeRelativeTo(const FormulaList& defs);
};

class SimpleRegion : public jalib::JRefCounted, public jalib::JPrintable {
public: 
  SimpleRegion(){}
  SimpleRegion(const CoordinateFormula& min, const CoordinateFormula& max)
    :_minCoord(min), _maxCoord(max)
  {}

  SimpleRegion(const MatrixDef& matrix){
    for(size_t i=0; i<matrix.numDimensions(); ++i){
      _minCoord.push_back(FormulaInteger::zero());
      _maxCoord.push_back(matrix.getSizeOfDimension(i));
    }
  }

  void print(std::ostream& o) const;

  SimpleRegionPtr intersect(const SimpleRegion& that) const;
  SimpleRegionPtr regionUnion(const SimpleRegion& that) const;

  bool hasIntersect(const SimpleRegion& that) const;

  size_t dimensions() const { return _minCoord.size(); }

  const CoordinateFormula& minCoord() const { return _minCoord; }
  const CoordinateFormula& maxCoord() const { return _maxCoord; }
  CoordinateFormula& minCoord() { return _minCoord; }
  CoordinateFormula& maxCoord() { return _maxCoord; }

  void addDimension(const FormulaPtr& min, const FormulaPtr& max){
    _minCoord.push_back(min);
    _maxCoord.push_back(max);
  }

  void offsetMaxBy(const FormulaPtr& val){
    for(CoordinateFormula::iterator i=_maxCoord.begin(); i!=_maxCoord.end(); ++i)
      *i = new FormulaAdd(*i, val);
    _maxCoord.normalize();
  }

protected:
  CoordinateFormula _minCoord;
  CoordinateFormula _maxCoord;
};

class Region : public SimpleRegion {
public:
  enum RegionType {
    REGION_INVALID,
    REGION_CELL,
    REGION_ROW,
    REGION_COL,
    REGION_BOX,
    REGION_SLICE,
    REGION_ALL
  };

  Region(const char* fromMatrix, const FormulaList& version, const char* type, const FormulaList& bounds);

  void print(std::ostream& o) const;

  void setName(const char*);
  void initialize(Transform&);
  CoordinateFormula calculateCenter() const;
  
  static RegionType strToRegionType(const std::string& str);

  void makeRelativeTo(const FormulaList& defs){
    _minCoord.makeRelativeTo(defs);
    _maxCoord.makeRelativeTo(defs);
  }

  std::string genTypeStr(bool isConst) const;
  std::string generateSignatureCode(bool isConst) const;
  std::string generateAccessorCode() const;

  SimpleRegionPtr getApplicableRegion(RuleInterface& rule, const FormulaList& defs, bool isOutput);

  void collectDependencies(const RuleInterface& rule, MatrixDependencyMap& map) const;

  void addAssumptions() const;

  FormulaPtr getSizeOfRuleIn(int d) const;

  MatrixDefPtr matrix() const { return _fromMatrix; }
  
  ///
  /// Get rate of change for this region as rule center moves
  FormulaList diff(const RuleInterface&) const;

  bool isSingleElement() const { return _originalType==REGION_CELL; }
  
  const std::string& name() const { return _name; }

  bool isAll() const { return _originalType == REGION_ALL; }
private:
  std::string _name;
  std::string _fromMatrixName;
  FormulaPtr  _version;
  RegionType  _originalType;
  FormulaList _originalBounds;
  MatrixDefPtr _fromMatrix;
};

}

#endif
