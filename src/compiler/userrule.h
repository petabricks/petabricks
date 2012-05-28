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
#ifndef PETABRICKSUSERRULE_H
#define PETABRICKSUSERRULE_H

#define STRINGIFY_INNER(x) #x
#define STRINGIFY(x) STRINGIFY_INNER(x)

#include "pbc.h"

#include "clcodegenerator.h"
#include "codegenerator.h"
#include "configitem.h"
#include "iterationorders.h"
#include "matrixdef.h"
#include "matrixdependency.h"
#include "rule.h"
#include "ruleir.h"

#include "common/jconvert.h"

#include <vector>

namespace petabricks {

/**
 * Represent a transform rule defined by the user
 */
class UserRule : public RuleInterface{
public:
  ///
  /// Constructor -- return style rule
  UserRule(const RegionPtr& to, const RegionList& from, const MatrixDefList& through, const FormulaList& where);

  ///
  /// Constructor -- to style rule
  UserRule(const RegionList& to, const RegionList& from, const MatrixDefList& through, const FormulaList& where);
  
  ///
  /// Initialize this rule after parsing
  void initialize(Transform&);
  

  ///
  /// Expand any duplicates by generating synthetic rules
  void performExpansion(Transform&);

  ///
  /// Set this->_body
  void setBody(const char* str, const jalib::SrcPos& p);

  ///
  /// Set priority flag
  void setPriority(RuleFlags::PriorityT v)  { _flags.priority = v; }
  
  ///
  /// Set rotation flag
  void addRotations(RuleFlags::RotationT v) { _flags.rotations |= v; }

  ///
  /// Set rule label
  void setLabel(const char *label) {
    _label = label;
  }

  ///
  /// Get rule label
  std::string getLabel() const {
    return _label;
  }

  ///
  /// Print this rule to a given stl stream
  /// implements JPrintable::print
  void print(std::ostream& o) const;
  
  ///
  /// Add RuleDescriptors to output corresponding to the extrema of the applicable region in dimension
  void getApplicableRegionDescriptors(RuleDescriptorList& output, const MatrixDefPtr& matrix, int dimension, const RulePtr& rule);

  
  void generateDeclCode(Transform& trans, CodeGenerator& o, RuleFlavor rf);
  void generateDeclCodeSequential(Transform& trans, CodeGenerator& o);
  void generateDeclCodeOpenCl(Transform& trans, CodeGenerator& o);

  void generateTrampCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor);
  
  void generateTrampCellCodeSimple(Transform& trans, CodeGenerator& o, RuleFlavor flavor);

  void generateUseOnCpu(CodeGenerator& o);
  void generateModifyOnCpu(CodeGenerator& o);
  void generateMultiOpenCLTrampCodes(Transform& trans, CodeGenerator& o, RuleFlavor flavor);
  void generateOpenCLCallCode(Transform& trans, CodeGenerator& o, RuleFlavor flavor);
  void generateOpenCLPrepareCode(Transform& trans, std::string& codename, CodeGenerator& o);
  void generateOpenCLCopyInCode(std::string& codename, std::vector<std::string>& packedargs, CodeGenerator& o, RegionPtr region);
  void generateOpenCLRunCode(Transform& trans, CodeGenerator& o);
  void generateOpenCLCopyOutCode(std::string& codename, CodeGenerator& o, RegionPtr region);
  ///
  /// Generate an OpenCL program implementing this rule
  void generateOpenCLKernel( Transform& trans, CLCodeGenerator& clo, IterationDefinition& iterdef, bool local=false);
  std::string getLastRowOnGpuGuide(RegionPtr region, int dim_int);
  std::string getLastRowOnGpuOffset(RegionPtr region, int dim_int);
  void collectGpuLocalMemoryData();
  RegionPtr findMatrix(std::string var) {
    for( RegionList::const_iterator i = _to.begin(); i != _to.end(); ++i )
      if( var == (*i)->name() )
	return (*i);
    for( RegionList::const_iterator i = _from.begin(); i != _from.end(); ++i )
      if( var == (*i)->name() )
	return (*i);
    return NULL;
  }
  bool canUseLocalMemory() {
    return _local.size() > 0;
  }
  void generateLocalBuffers(CLCodeGenerator& clo);

  ///
  /// Generate seqential code to invoke this rule
  void generateCallCode(const std::string& nodename,
                        Transform& trans,
                        CodeGenerator& o,
                        const SimpleRegionPtr& region,
                        RuleFlavor flavor,
			bool wrap,
                        std::vector<RegionNodeGroup>& regionNodesGroups,
                        int nodeID,
                        int gpuCopyOut); 

  ///
  /// Return function the name of this rule in the code
  std::string implcodename(Transform& trans) const;
  std::string trampcodename(Transform& trans) const;

  bool isReturnStyle() const { return _flags.isReturnStyle; }

  int dimensions() const;

  void addAssumptions() const;

  void collectDependencies(StaticScheduler& scheduler);

  void markRecursive() { 
    markRecursive(NULL);
  }
  void markRecursive(const FormulaPtr& rh) { 
    if(!_flags.isRecursive){
      _flags.isRecursive = true; 
      _recursiveHint = rh;
    }
  }

  bool isRecursive() const { return _flags.isRecursive; }

  bool isOpenClRule() const {
#ifdef HAVE_OPENCL
    return _gpuRule;
#else
    return false;
#endif
  }

  /* bool hasStencil(MatrixDefPtr matrix, size_t dim) { */
  /*   if(matrix->numDimensions() != dim) */
  /*     return false; */
  /*   return _maxCoordOffsets.find(matrix->name()) != _maxCoordOffsets.end() && MAXIMA.compare(_maxCoordOffsets[matrix->name()][dim - 1], ">", FormulaInteger::one()); */
  /* } */

  /// 0 - need to entire matrix
  /// 1 - one to one
  /// 2 - multiple to one
  int stencilType(RegionPtr region, bool min) {
    std::cout << "#### stencil = " << region->name() << " " << region->matrix()->name() << std::endl;
    MatrixDefPtr matrix = region->matrix();
    IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
    size_t dim = iterdef.dimensions();
    if(dim == 0 || region->getRegionType() == Region::REGION_ALL || matrix->numDimensions() != dim) {
      return 0;
    }
    if(_maxCoordOffsets.find(matrix->name()) != _maxCoordOffsets.end()) {
      std::cout << "mincoordoffset = " << _minCoordOffsets[matrix->name()][dim - 1] << std::endl;
      std::cout << "name = " << matrix->name() << std::endl;
      std::cout << "offset = " << _minCoordOffsets[matrix->name()][dim - 1] << std::endl;
      if(min) {
	if(MAXIMA.comparePessimistically(_minCoordOffsets[matrix->name()][dim - 1], "<", FormulaInteger::zero())) {
	  return 2;
	}
	else if(MAXIMA.comparePessimistically(_minCoordOffsets[matrix->name()][dim - 1], "=", FormulaInteger::zero())) {
	  return 1;
	}
      }
      else {
	std::cout << "#### max = " <<  _maxCoordOffsets[matrix->name()][dim - 1] << std::endl;
	if(MAXIMA.comparePessimistically(_maxCoordOffsets[matrix->name()][dim - 1], ">", FormulaInteger::one())) {
	std::cout << "### stencil = 2" << std::endl;
	  return 2;
	}
	else if(MAXIMA.comparePessimistically(_maxCoordOffsets[matrix->name()][dim - 1], "=", FormulaInteger::one())) {
	std::cout << "### stencil = 1" << std::endl;
	  return 1;
	}

      }
    }
    //std::cout << "matrix not found" << std::endl;
    std::cout << "### stencil = 0" << std::endl;
    return 0;
  }

  /* bool isDivisible(SimpleRegionPtr region) {   */
  /*   //TODO: what about COL and ROW? */
  /*   if(region->removedDimensions() > 0) return false; */
  /*   IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall()); */
  /*   for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){ */
  /*     if((*i)->getRegionType() == Region::REGION_ALL || (*i)->getRegionType() == Region::REGION_COL) { */
  /* 	return false; */
  /*     } */
  /*   } */
  /*   return true; */
  /* } */

  bool isDivisible() {  
    IterationDefinition iterdef(*this, getSelfDependency(), isSingleCall());
    for(RegionList::iterator i=_to.begin(); i!=_to.end(); ++i){
      if((*i)->getRegionType() == Region::REGION_ALL || (*i)->getRegionType() == Region::REGION_COL || (*i)->removedDimensions() > 0) {
	return false;
      }
    }
    return true;
  }

  RuleFlags::PriorityT priority() const { return _flags.priority; }
  const FormulaList& conditions() const { return _conditions; }

  bool canProvide(const MatrixDefPtr& m) const {
    return _provides.find(m) != _provides.end();
  }

  const FormulaPtr& recursiveHint() const { return _recursiveHint; }

  FormulaPtr getSizeOfRuleIn(int d){
    for(size_t i=0; i<_to.size(); ++i){
      if(_to[i]->isExistingDimension(d)){
        return _to[i]->getSizeOfRuleIn(d);
      }
    }
    for(size_t i=0; i<_to.size(); ++i){
      if(_to[i]->isRemovedDimension(d)){
        return _to[i]->getSizeOfRuleInRemovedDimension(d);
      }
    }
    JASSERT(false)(d)(_id);
    return 0;
  }

  size_t getMaxOutputDimension(){
    size_t max = 0;
    for(size_t i = 0; i < _to.size(); ++i) {
      size_t d = _to[i]->dimensions();
      if(d > max)
        max = d;
    }
    return max;
  }

  bool isSingleElement() const {
    if(_to.size()!=1) return false;
    return _to[0]->isSingleElement();
  }

  void compileRuleBody(Transform& tx, RIRScope& s);

  bool isSingleCall() const {
    for(size_t i=0; i<_to.size(); ++i)
      if(!_to[i]->isAll())
        return false;
    return true;
  }

  RegionPtr regionRep() {
    RegionPtr rep = _to.front();
    for( RegionList::const_iterator i = _to.begin( ); i != _to.end( ); ++i ) {
      if((*i)->getRegionType() < rep->getRegionType())
	rep = *i;
    }
    return rep;
  }

  ///
  /// add a variable to duplicate the rulebody max-min times with
  void addDuplicateVar(const std::string& name, int min, int max){
    _duplicateVars.push_back(ConfigItem(ConfigItem::FLAG_TEMPLATEVAR, name, min, min, max));
  }

  size_t duplicateCount() const;
  ///returns old duplicate number
  size_t setDuplicateNumber(size_t c);
  size_t getDuplicateNumber();

  bool hasWhereClause() const { return _conditions.size()>0; }

  FormulaPtr getWhereClause() const { return getWhereClause(0); }
  FormulaPtr getWhereClause(int start) const {
    if(_conditions.size()==0) return NULL;
    if((int)_conditions.size()-1==start) return _conditions[start];
    return new FormulaAnd(_conditions[start], getWhereClause(start+1));
  }
  
  DependencyDirection getSelfDependency() const;

  RegionList getFromRegions( ) const
  {
    return _from;
  }

  RegionSet getFromRegionsOnCpu( ) const
  {
    return _loads;
  }
  RegionList getToRegions( ) const
  {
    return _to;
  }

  RIRBlockCopyRef getBody( ) const
  {
    return _bodyir[RuleFlavor::SEQUENTIAL];
  }

  void buildApplicableRegion(Transform& trans,
                             SimpleRegionPtr& ar, 
                             bool allowOptional);
                                     
  virtual void removeDimensionFromMatrix(const MatrixDefPtr matrix, 
                                          const size_t dimension);
  
  virtual void fixVersionedRegionsType();
  
  virtual RegionList getSelfDependentRegions();
  
  virtual RegionList getNonSelfDependentRegions();

  void buildFromBoundingBox();
  
  void trimDependency(DependencyDirection& dep,
                      const ChoiceDepGraphNode& from,
                      const ChoiceDepGraphNode& to);

  std::map<std::string, FormulaList>& minCoordOffsets() {
    return _minCoordOffsets;
  }
  
private:
  void computeDataDependencyVector();
  CoordinateFormula computeDDVAsDifference(const RegionPtr inputRegion,
                                           const RegionPtr outputRegion
                                          ) const;
  void computeDDVForGivenOutput(const RegionPtr outputRegion);
  
  void removeDimensionFromRegionList(RegionList& list,
                                     const MatrixDefPtr matrix, 
                                     const size_t dimension);
  
  void removeDimensionFromMatrixDependencyMap(MatrixDependencyMap& map,
                                              const MatrixDefPtr matrix,
                                              const size_t dimension);                                
                                              
  void removeDimensionFromDefinitions(const size_t dimension);

  ///
  /// Set gpu buffer flag to matrices
  void prepareBuffers();

  bool passBuildGpuProgram(Transform& trans);

  std::set<std::string> _local;
  std::map<std::string, FormulaList> _minCoordOffsets;
  std::map<std::string, FormulaList> _maxCoordOffsets;
  RegionSet _loads;
  RegionSet _stores;

  RuleFlags _flags;
  RegionList _from;
  RegionList _to;
  MatrixDefList _through;
  FormulaList _conditions;
  FormulaList _definitions;
  std::string _bodysrc;
  jalib::SrcPosTaggable _bodysrcPos;
  RIRBlockCopyRef _bodyir[RuleFlavor::_COUNT];
  RIRBlockCopyRef _bodyirLocalMem;
  MatrixDependencyMap _depends;
  MatrixDependencyMap _provides;
  FormulaPtr _recursiveHint;
  std::string _label;
  ConfigItems _duplicateVars;
  RulePtr _gpuRule;

 
  typedef std::map<MatrixDefPtr, SimpleRegionPtr> MatrixToRegionMap;
  MatrixToRegionMap _fromBoundingBox;
};

}
#endif
