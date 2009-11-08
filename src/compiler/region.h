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
#ifndef PETABRICKSREGION_H
#define PETABRICKSREGION_H

#include "formula.h"
#include "matrixdef.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"

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
  
  std::vector<std::string> argnames() const {
    std::vector<std::string> args;
    for( CoordinateFormula::const_iterator i=minCoord().begin()
         ; i!=minCoord().end()
         ; ++i)
    {
      args.push_back((*i)->toString());
    }
    for( CoordinateFormula::const_iterator i=maxCoord().begin()
         ; i!=maxCoord().end()
         ; ++i)
    {
      args.push_back((*i)->toString());
    }
    return args;
  }

  size_t size() const { return dimensions(); }
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

  SimpleRegionPtr getApplicableRegion(Transform& tx, RuleInterface& rule, const FormulaList& defs, bool isOutput);

  void collectDependencies(const Transform& tx, const RuleInterface& rule, MatrixDependencyMap& map) const;

  void addAssumptions() const;

  FormulaPtr getSizeOfRuleIn(int d) const;

  MatrixDefPtr matrix() const { return _fromMatrix; }
  
  ///
  /// Get rate of change for this region as rule center moves
  FormulaList diff(const Transform&, const RuleInterface&) const;

  bool isSingleElement() const { return _originalType==REGION_CELL && dimensions()>0; }
  
  const std::string& name() const { return _name; }

  bool isAll() const { return _originalType == REGION_ALL; }

  void assertNotInput();

  void setOptionalDefault(const FormulaPtr& f){
    _optionalDefault = f;
  }
  bool isOptional() const { return _optionalDefault; }
  const FormulaPtr& optionalDefault() { return _optionalDefault; }
private:
  std::string _name;
  std::string _fromMatrixName;
  FormulaPtr  _version;
  RegionType  _originalType;
  FormulaList _originalBounds;
  MatrixDefPtr _fromMatrix;
  FormulaPtr  _optionalDefault;
};

}

#endif
