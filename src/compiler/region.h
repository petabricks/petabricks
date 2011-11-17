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
#ifndef PETABRICKSREGION_H
#define PETABRICKSREGION_H

#include "formula.h"
#include "matrixdef.h"
#include "pbc.h"

#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/srcpos.h"

#include <vector>

namespace petabricks {
class CodeGenerator;
class MatrixDependencyMap;
class Region;
class RIRScope;
class RuleInterface;
class SimpleRegion;
class Transform;
class UserRule;
typedef jalib::JRef<Region> RegionPtr;
typedef jalib::JRef<SimpleRegion> SimpleRegionPtr;


class RegionNodeGroup : public jalib::JRefCounted {
public:
  RegionNodeGroup(const std::string& name, std::vector<int>& ids) {
    _matrixName = name;
    _nodeIDs = ids;
  }
  std::string matrixName() { return _matrixName; }
  std::vector<int>& nodeIDs() { return _nodeIDs; }
private:
  std::string _matrixName;
  std::vector<int> _nodeIDs;
};

class RegionList : public std::vector<RegionPtr> , public jalib::JRefCounted, public jalib::SrcPosTaggable, public jalib::JPrintable {
public:
  void print(std::ostream& o) const;
  void makeRelativeTo(const FormulaList& defs);
};

class SimpleRegion : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
private:
  struct RemovedDimensionsInfo {
    CoordinateFormula minCoord;
    CoordinateFormula maxCoord;
  };
  
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
  
  size_t isExistingDimension(size_t dimension) const { 
    return dimension < dimensions();
  }
  
  size_t removedDimensions() const { return _removedDimensions.minCoord.size(); }
  
  size_t totalDimensions() const {return dimensions() + removedDimensions(); }
  
  size_t isRemovedDimension(size_t dim) const {
    size_t existingDimensions = dimensions();
    
    return (existingDimensions <= dim) && (dim < totalDimensions());
  }
  
  const CoordinateFormula& minCoord() const { return _minCoord; }
  const CoordinateFormula& maxCoord() const { return _maxCoord; }
  CoordinateFormula& minCoord() { return _minCoord; }
  CoordinateFormula& maxCoord() { return _maxCoord; }

  void addDimension(const FormulaPtr& min, const FormulaPtr& max){
    _minCoord.push_back(min);
    _maxCoord.push_back(max);
  }
  
  FormulaPtr symbolicSize() const;
  
  ///Remove the given dimension from the region
  void removeDimension(const size_t dimension) {
    CoordinateFormula& minCoordVector = minCoord();
    CoordinateFormula& maxCoordVector = maxCoord();
    
    //Store the dimension in the removed dimensions data structure
    _removedDimensions.minCoord.push_back(minCoordVector[dimension]);
    _removedDimensions.maxCoord.push_back(maxCoordVector[dimension]);
    
    //Erase the dimension
    minCoordVector.erase(minCoordVector.begin()+dimension);
    maxCoordVector.erase(maxCoordVector.begin()+dimension);    
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

  std::string getIterationLowerBounds() const;
  std::string getIterationUpperBounds() const;
  
  size_t size() const { return dimensions(); }
protected:
  CoordinateFormula _minCoord;
  CoordinateFormula _maxCoord;
  RemovedDimensionsInfo _removedDimensions;
};

class DependencyDirection;

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
  void validate();
  CoordinateFormula calculateCenter() const;
  
  static RegionType strToRegionType(const std::string& str);

  void makeRelativeTo(const FormulaList& defs){
    _minCoord.makeRelativeTo(defs);
    _maxCoord.makeRelativeTo(defs);
  }

  std::string genTypeStr(RuleFlavor rf, bool isConst) const;
  std::string generateSignatureCode(RuleFlavor rf, bool isConst) const;
  std::string generateAccessorCode(bool allowOptional=true) const;

  SimpleRegionPtr getApplicableRegion(Transform& tx, RuleInterface& rule, const FormulaList& defs, bool isOutput);

  void collectDependencies(const Transform& tx, const RuleInterface& rule, MatrixDependencyMap& map) const;

  void addAssumptions() const;

  FormulaPtr getSizeOfRuleIn(int d) const;
  FormulaPtr getSizeOfRuleInRemovedDimension(int d) const;

  MatrixDefPtr matrix() const { return _fromMatrix; }
  
  ///
  /// Get rate of change for this region as rule center moves
  FormulaList diff(const Transform&, const RuleInterface&) const;

  bool isSingleElement() const { return _originalType==REGION_CELL && dimensions()>0; }
  
  const std::string& name() const { return _name; }

  bool isVersioned() const { return _version; }
  
  bool isAll() const { return _originalType == REGION_ALL; }

  void assertNotInput();

  void setOptionalDefault(const FormulaPtr& f){
    if(f->toString()=="OPTIONAL")
      _optionalDefault = new FormulaVariable("petabricks::the_missing_val()");  
    else 
      _optionalDefault = f;
  }
  bool isOptional() const { return _optionalDefault; }
  const FormulaPtr& optionalDefault() const { return _optionalDefault; }

  FormulaList getOriginalBounds() const
  {
    return _originalBounds;
  }

  RegionType getRegionType() const
  {
    return _originalType;
  }

  void addArgToScope(RIRScope& scope) const;

  void setBuffer(bool b) { buffer = b; }
  bool isBuffer() const { return buffer; }

  void setArgName(std::string& name) { argName = name; }
  const std::string& getArgName() const { return argName; }

  void fixTypeIfVersioned();
  
private:
  void determineDependencyDirection(const size_t dimension, 
                                    const RuleInterface& rule, 
                                    DependencyDirection& direction) const;
private:
  std::string _name;
  std::string _fromMatrixName;
  FormulaPtr  _version;
  RegionType  _originalType;
  FormulaList _originalBounds;
  MatrixDefPtr _fromMatrix;
  FormulaPtr  _optionalDefault;
  bool        buffer;
  std::string argName;
};

}

#endif
