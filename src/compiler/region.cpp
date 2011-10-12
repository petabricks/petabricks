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
#include "region.h"

#include "matrixdependency.h"
#include "maximawrapper.h"
#include "rule.h"
#include "transform.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

static petabricks::Region::RegionType _strToRegionType(const std::string& str){
  if(str=="cell")   return petabricks::Region::REGION_CELL;
  if(str=="row")    return petabricks::Region::REGION_ROW;
  if(str=="col")    return petabricks::Region::REGION_COL;
  if(str=="column") return petabricks::Region::REGION_COL;
  if(str=="region") return petabricks::Region::REGION_BOX;
  if(str=="all")    return petabricks::Region::REGION_ALL;
  JASSERT(false)(str).Text("Unknown region type");
  return petabricks::Region::REGION_INVALID;
}

petabricks::Region::RegionType petabricks::Region::strToRegionType(const std::string& str){
  return _strToRegionType(str);
}

petabricks::Region::Region(const char* fromMatrix, const FormulaList& version, const char* type, const FormulaList& bounds) 
  : _name(RETURN_VAL_STR)
  , _fromMatrixName(fromMatrix)
  , _originalType(strToRegionType(type))
  , _originalBounds(bounds)
{
  if(version.size()>0){
    JASSERT(version.size()==1)(version).Text("only one version allowed in matrix accessors");
    _version = version[0];
  }
}

void petabricks::Region::print(std::ostream& o) const {
  o << _fromMatrixName << ".region(" ;
  printStlList(o, _minCoord.begin(), _minCoord.end(), ", ");
  o << ", ";
  printStlList(o, _maxCoord.begin(), _maxCoord.end(), ", ");
  o << ") " << _name;
}

void petabricks::Region::setName(const char* name){ 
  _name=name; 
  if(_name=="") _name=RETURN_VAL_STR;
}

void petabricks::Region::initialize(Transform& trans) {
  _originalBounds.normalize();
   _fromMatrix = trans.lookupMatrix( _fromMatrixName );
  JASSERT(_fromMatrix);

  JASSERT(!isOptional() || _originalType == REGION_CELL)(_name)(_fromMatrix)
    .Text("optional rule inputs currently only supported for .cell() inputs");
  
  //convert given coordinates to a region
  switch(_originalType){
  case REGION_CELL: {
      //min = max = given
      for(size_t i=0; i<_originalBounds.size(); ++i){
        _minCoord.push_back(_originalBounds[i]);
        /* NB: _maxCoord[i]=_originalBounds[i]+1 because the lower bound is 
         * included while the upper bound is excluded */
        _maxCoord.push_back(MaximaWrapper::instance().normalize(
          new FormulaAdd(_originalBounds[i], FormulaInteger::one())));
      }
      JASSERT(_fromMatrix->numDimensions()==_minCoord.size()+_version.operator bool())
        (*this)(_fromMatrix->numDimensions())(_minCoord.size())
        .Text("Wrong number of args given for cell");
    }
    break;
  case REGION_COL: {
      //min=(given[0], 0)
      //max=(given[0], height-1)
      JASSERT(_originalBounds.size()==1)(_originalBounds.size())
        .Text("only one arg expected for 'col()'");
      JASSERT(_fromMatrix->numDimensions()==2)(*this)(_fromMatrix->numDimensions())
        .Text("col() only supported on a 2D matrix");
      _minCoord.push_back(_originalBounds.front());
      _minCoord.push_back(FormulaInteger::zero());
//       _maxCoord.push_back(_originalBounds.front());
      _maxCoord.push_back(new FormulaAdd(_originalBounds.front(), FormulaInteger::one()));
      _maxCoord.push_back(_fromMatrix->height());
      //JTRACE("made column")(_fromMatrix)(_minCoord)(_maxCoord);
    }
    break;
  case REGION_ROW: {
      //min=(0,        given[0])
      //max=(height-1, given[0])
      JASSERT(_originalBounds.size()==1)(_originalBounds.size())
        .Text("only one arg expected for 'row()'");
      JASSERT(_fromMatrix->numDimensions()==2)(*this)(_fromMatrix->numDimensions())
        .Text("row() only supported on a 2D matrix");
      _minCoord.push_back(FormulaInteger::zero());
      _minCoord.push_back(_originalBounds.front());
      _maxCoord.push_back(_fromMatrix->width());
//       _maxCoord.push_back(_originalBounds.front());
      _maxCoord.push_back(new FormulaAdd(_originalBounds.front(), FormulaInteger::one()));
      //JTRACE("made row")(_fromMatrix)(_minCoord)(_maxCoord);
    }
    break;
  case REGION_BOX: {
      //min = (given[0], given[1])
      //max = (given[2], given[3])
      size_t s = _originalBounds.size();
      JASSERT(s%2==0)(s).Text("expected even number of args for 'region()'");
      JASSERT(_fromMatrix->numDimensions()==s/2+_version.operator bool())(*this)(_fromMatrix->numDimensions())(s)
        .Text("Wrong number of args given for region");
      for(size_t i=0; i<s/2; ++i)
        _minCoord.push_back(_originalBounds[i]);
      for(size_t i=s/2; i<s; ++i)
        _maxCoord.push_back(_originalBounds[i]);
    }
    break;
  case REGION_ALL: {
      JASSERT(_originalBounds.size()==0)(_originalBounds.size())
        .Text("no args expected for all()");
      int d=_fromMatrix->numDimensions();
      if(_version) --d;
      if(d==0){
        _originalType=REGION_CELL;
      }else{
        for(int i=0; i<d; ++i){
          _minCoord.push_back(FormulaInteger::zero());
          _maxCoord.push_back(_fromMatrix->getSizeOfDimension(i));
        }
      }
    }
    break;
  default:
    JASSERT(false).Text("Unreachable");
    break;
  }

  if(_version){
    _minCoord.push_back(_version);
    _maxCoord.push_back(MaximaWrapper::instance().normalize(new FormulaAdd(_version, FormulaInteger::one())));
  }
}

void petabricks::Region::validate() {
  addAssumptions();
  for(size_t i=0; i<dimensions(); ++i){
    int isBeginLeqEnd = MAXIMA.tryCompare(_minCoord[i], "<=", _maxCoord[i]); 
    JASSERT(isBeginLeqEnd==MaximaWrapper::YES)(_minCoord[i])(_maxCoord[i])
      .Text("Invalid .region().  Syntax is .region(beginCoord, endCoord), for example region(x, y, x+4, y+4).  Begin is inclusive end is exclusive.");
  }
}

petabricks::CoordinateFormula petabricks::Region::calculateCenter() const {
//   CoordinateFormula tmp;
//   JASSERT(_minCoord.size()==_maxCoord.size())(_minCoord.size())(_maxCoord.size());
//   for(size_t i=0; i<_minCoord.size(); ++i){
//     tmp.push_back( new FormulaDivide(new FormulaAdd(_minCoord[i],_maxCoord[i]),
//                                      new FormulaInteger(2)) );
//   }
//   tmp.normalize();
  return _minCoord;
}

void petabricks::RegionList::makeRelativeTo(const FormulaList& defs){
  for(iterator i=begin(); i!=end(); ++i){
    (*i)->makeRelativeTo(defs);
  }
}

void petabricks::RegionList::print(std::ostream& o) const {
  o << "RegionList:";
  for(RegionList::const_iterator i=this->begin(), e=this->end(); i!=e; ++i) {
    RegionPtr region= *i;
    o << "\n* " << region;
  }
}

void petabricks::SimpleRegion::print(std::ostream& o) const {
  o << _minCoord << ", " << _maxCoord;
}

std::string petabricks::SimpleRegion::getIterationLowerBounds() const {
  std::string s = minCoord().toString();
  
  if(removedDimensions() == 0) {
    return s;
  }
  
  return s + ", " + _removedDimensions.minCoord.toString();
}

std::string petabricks::SimpleRegion::getIterationUpperBounds() const {
  std::string s = maxCoord().toString();
  
  if(removedDimensions() == 0) {
    return s;
  }
  
  return s + ", " + _removedDimensions.maxCoord.toString();
}

  
petabricks::SimpleRegionPtr petabricks::Region::getApplicableRegion(Transform& tx, RuleInterface& rule, const FormulaList&, bool isOutput){
  CoordinateFormula min;
  CoordinateFormula max;
    
  FormulaList offsets = diff(tx,rule);
  FormulaList minDefs /*= _defs*/;
  FormulaList maxDefs /*= _defs*/;
  for(size_t i=0; i<_minCoord.size(); ++i){
    minDefs.push_back(new FormulaEQ(FormulaInteger::zero(), _minCoord[i]));
    maxDefs.push_back(new FormulaEQ(_fromMatrix->getSizeOfDimension(i), _maxCoord[i]));
    if(MAXIMA.tryCompare(offsets[i],"<",FormulaInteger::zero())==MaximaWrapper::YES){
      //special case... coordinate grows inversely proportional to x
      std::swap(minDefs[i], maxDefs[i]);
      offsets[i]=offsets[i]->negative();
    }
  }

  //first do min
  {
    FormulaList& defs = minDefs;
    FreeVarsPtr fv = defs.getFreeVariables();
    for(size_t i=0; i<_minCoord.size(); ++i){
      std::string var = rule.getOffsetVar(i)->toString();
      if(fv->contains(var)){
        FormulaPtr f =  rule.trimImpossible(MaximaWrapper::instance().maxSolve(defs, var))->rhs();
        f=MaximaWrapper::instance().staticCeiling(f);
        min.push_back(f);
      }else if(_minCoord[i]->getFreeVariables()->empty())
        min.push_back(_minCoord[i]);
      else
        min.push_back(FormulaInteger::zero());
    }
  }

  //then do max
  {
    FormulaList& defs = maxDefs;
    FreeVarsPtr fv = defs.getFreeVariables();
    for(size_t i=0; i<_maxCoord.size(); ++i){
      std::string var = rule.getOffsetVar(i)->toString();
      if(fv->contains(var)){
        FormulaPtr f = rule.trimImpossible(MaximaWrapper::instance().minSolve(defs, var))->rhs();
        if(offsets[i]->toString()=="0"){
          JWARNING(offsets[i]->toString()!="0");
          f=new FormulaAdd(f, FormulaInteger::one());
        }else{
          f=new FormulaAdd(f, new FormulaDivide(FormulaInteger::one(), offsets[i]));
        }
        if(isOutput)
          f=MaximaWrapper::instance().normalize(f);
        else
          f=MaximaWrapper::instance().staticCeiling(f);
        max.push_back(f);
      }else if(_maxCoord[i]->getFreeVariables()->empty()){
        FormulaPtr f;
        if(isOutput){
          f = _maxCoord[i];
          f = new FormulaAdd(f, FormulaInteger::one());
        }else{
          f=Formula::inf();
        }
        max.push_back(MaximaWrapper::instance().normalize(f));
      }else{
        FormulaPtr f;
        if(isOutput){
          f = _fromMatrix->getSizeOfDimension(i);
        }else{
          f=Formula::inf();
        }
        max.push_back(f);
      }
    }
  }
  JTRACE("Computed applicable region")(_fromMatrix)(_minCoord)(_maxCoord)(offsets)(minDefs)(maxDefs)(min)(max);
  return new SimpleRegion(min,max);
}

petabricks::FormulaList petabricks::Region::diff(const Transform& tx, const RuleInterface& rule) const {
  FormulaList tmp = _maxCoord;
  for(size_t i=0; i<tmp.size(); ++i){
    FreeVars fv = *tmp[i]->getFreeVariables();
    fv.eraseAll(tx.constants());
    if(fv.size()==1){
      bool found=false;
      for(size_t d=0; d<tmp.size(); ++d){
        if(fv.contains(rule.getOffsetVar(d)->toString())){
          tmp[i]=MaximaWrapper::instance().diff(tmp[i], rule.getOffsetVar(d));
          found=true;
          break;
        }
      }
      if(!found) tmp[i]=FormulaInteger::zero();
    }else{
      //TODO suppport multiple free vars in coordinates
      JWARNING(fv.size()<=1)(fv.size());
      tmp[i]=FormulaInteger::zero();
    }
  }
  JTRACE("diff maxcoord")(_fromMatrix)(_minCoord)(tmp);
  return tmp;
}

petabricks::SimpleRegionPtr petabricks::SimpleRegion::intersect(const SimpleRegion& that) const{
  if(that.dimensions() > dimensions()) return that.intersect(*this);
  CoordinateFormula min,max;
  for(size_t i=0; i<that.dimensions(); ++i){
    min.push_back( MaximaWrapper::instance().max(_minCoord[i], that._minCoord[i]) );
    max.push_back( MaximaWrapper::instance().min(_maxCoord[i], that._maxCoord[i]) );
  }
  for(size_t i=that.dimensions(); i<dimensions(); ++i){
    min.push_back(_minCoord[i]);
    max.push_back(_maxCoord[i]);
  }
  return new SimpleRegion(min, max);
}

petabricks::SimpleRegionPtr petabricks::SimpleRegion::regionUnion(const SimpleRegion& that) const{
  JASSERT(dimensions()==that.dimensions());
  CoordinateFormula min,max;
  for(size_t i=0; i<dimensions(); ++i){
    min.push_back( MaximaWrapper::instance().min(_minCoord[i], that._minCoord[i]) );
    max.push_back( MaximaWrapper::instance().max(_maxCoord[i], that._maxCoord[i]) );
  }
  return new SimpleRegion(min, max);
}

bool petabricks::SimpleRegion::hasIntersect(const SimpleRegion& that) const {
  if(toString() == that.toString()) //optimization
    return true;
  if(dimensions()!=that.dimensions()){
    JWARNING(dimensions()==that.dimensions())
      .Text("assuming no intersect");
    return false;
  }
  if(dimensions()==0) return true;
  for(size_t i=0; i<dimensions(); ++i){
    if( MaximaWrapper::instance().compare(maxCoord()[i], "<=", that.minCoord()[i]) )
      return false;
    if( MaximaWrapper::instance().compare(minCoord()[i], ">=", that.maxCoord()[i]) )
      return false;
  }
//   JTRACE("found intersect")(*this)(that);
  return true;
}

std::string petabricks::Region::genTypeStr(bool isConst) const{
  switch(_originalType){
  case REGION_CELL:
    if(isConst)
#ifndef REGIONMATRIX_TEST
      return "const ElementT";
#else
      return "const CellProxy";
#endif
    else
#ifndef REGIONMATRIX_TEST
      return "ElementT&";
#else
      return "CellProxy&";
#endif
  case REGION_COL:
  case REGION_ROW:
    return (isConst?MatrixDef::oneD().constMatrixTypeName():MatrixDef::oneD().matrixTypeName());
  case REGION_BOX:
  case REGION_ALL:
    return (isConst?_fromMatrix->constMatrixTypeName():_fromMatrix->matrixTypeName());
  case REGION_SLICE:
    return (isConst?_fromMatrix->constSliceTypeName():_fromMatrix->sliceTypeName());
  default:
    JASSERT(false).Text("Unreachable");
    return "";
  }
}

std::string petabricks::Region::generateSignatureCode(bool isConst) const{
  return genTypeStr(isConst) + " " + _name;
}

std::string petabricks::Region::generateAccessorCode(bool allowOptional) const{
  switch(_originalType){
  case REGION_CELL:
    {
      std::string s = _fromMatrix->name() + ".cell("+_minCoord.toString()+")";
      if(allowOptional && isOptional())
        return "(" + _fromMatrix->name() + ".contains("+_minCoord.toString()+")"
                   + " ? " + s + " : " + optionalDefault()->toString() + ")";
      else
        return s;
    }
  case REGION_COL:
    return _fromMatrix->name() + ".col("+_minCoord[0]->toString()+")";
  case REGION_ROW:
    return _fromMatrix->name() + ".row("+_minCoord[1]->toString()+")";
  case REGION_BOX:
    return _fromMatrix->name() + ".region("+_minCoord.toString() +", "+_maxCoord.toString()+")";
  case REGION_SLICE:
    return _fromMatrix->name() + ".slice("+jalib::XToString(_fromMatrix->numDimensions()-1)+","+jalib::XToString(_minCoord.back())+")";
  case REGION_ALL:
    return _fromMatrix->name();
  default:
    JASSERT(false).Text("Unreachable");
    return "";
  }
}

void petabricks::Region::determineDependencyDirection(const size_t dimension, const RuleInterface& rule, DependencyDirection& direction) const {
  MaximaWrapper::tryCompareResult isLeft =MaximaWrapper::instance().tryCompare(rule.getOffsetVar(dimension),  "<", _maxCoord[dimension]);
  MaximaWrapper::tryCompareResult isRight=MaximaWrapper::instance().tryCompare(rule.getOffsetVar(dimension), ">=", _minCoord[dimension]);
  
  if(isLeft!=MaximaWrapper::YES)
    direction.addDirection(dimension, DependencyDirection::D_LT);

  if(isRight!=MaximaWrapper::YES)
    direction.addDirection(dimension, DependencyDirection::D_GT);

  if(isLeft==MaximaWrapper::UNKNOWN || isRight==MaximaWrapper::UNKNOWN || isLeft==isRight){
    direction.addDirection(dimension, DependencyDirection::D_EQ);
  }else{
    if(isLeft){
      if(MaximaWrapper::instance().tryCompare(rule.getOffsetVar(dimension), "<",  _minCoord[dimension])!=MaximaWrapper::YES)
        direction.addDirection(dimension, DependencyDirection::D_EQ);
    }else{//isRight
      if(MaximaWrapper::instance().tryCompare(rule.getOffsetVar(dimension), ">=", _maxCoord[dimension])!=MaximaWrapper::YES)
        direction.addDirection(dimension, DependencyDirection::D_EQ);
    }
  }

}

void petabricks::Region::collectDependencies(const Transform& tx, const RuleInterface& rule, MatrixDependencyMap& map) const {
  DependencyDirection direction(dimensions());
  for(size_t dimension=0; dimension<dimensions(); ++dimension){
    determineDependencyDirection(dimension, rule, direction);
  }
  FormulaList offsets = diff(tx,rule);
  SimpleRegion applicable = rule.applicableRegion();
  applicable.maxCoord().subToEach(FormulaInteger::one());
  FormulaList minDefs;
  FormulaList maxDefs;
  for(size_t i=0; i<applicable.dimensions(); ++i){
    minDefs.push_back(new FormulaEQ(rule.getOffsetVar(i), applicable.minCoord()[i]));
    maxDefs.push_back(new FormulaEQ(rule.getOffsetVar(i), applicable.maxCoord()[i]));
  }
  CoordinateFormula minAbsolute = minCoord();
  CoordinateFormula maxAbsolute = maxCoord();
  for(size_t i=0; i<offsets.size(); ++i){
    if(MAXIMA.tryCompare(offsets[i],"<",FormulaInteger::zero())==MaximaWrapper::YES){
      //this dimension is inversely proportional to var
      //swap it so the next step makes it relative to the opposite formulas
      std::swap(minAbsolute[i], maxAbsolute[i]);
    }
  }
  minAbsolute.makeRelativeTo(minDefs);
  maxAbsolute.makeRelativeTo(maxDefs);
  for(size_t i=0; i<offsets.size(); ++i){
    if(MAXIMA.tryCompare(offsets[i],"<",FormulaInteger::zero())==MaximaWrapper::YES){
      //this dimension is inversely proportional to var
      //now swap its back to its original place
      std::swap(minAbsolute[i], maxAbsolute[i]);
    }
  }
  SimpleRegionPtr region = new SimpleRegion(minAbsolute, maxAbsolute);
      
  //Merge with existing entry
  MatrixDependencyPtr dep = new MatrixDependency(direction, region);
  MatrixDependencyPtr& element = map[_fromMatrix];
  if(!element) element = dep;
  else         element->mergeWith(dep);
}

void petabricks::Region::addAssumptions() const{
  for(size_t i=0; i<dimensions(); ++i){
    FormulaPtr end = _fromMatrix->getSizeOfDimension(i);
    MAXIMA.assume(new FormulaLE(FormulaInteger::zero(), _minCoord[i]));
    MAXIMA.assume(new FormulaLE(FormulaInteger::zero(), _maxCoord[i]));
    MAXIMA.assume(new FormulaLE(_minCoord[i], end));
    MAXIMA.assume(new FormulaLE(_maxCoord[i], end));
  } 
}

void petabricks::Region::assertNotInput(){
  JASSERT(!_fromMatrix->isAllInput())(_name)(_fromMatrixName)
    .Text("Error, input region cannot appear in to() clause.");
}

petabricks::FormulaPtr petabricks::Region::getSizeOfRuleIn(int d) const{
  JASSERT((int)dimensions()>d)(dimensions())(d);
  return MaximaWrapper::instance().normalize(new FormulaSubtract(_maxCoord[d], _minCoord[d]));
}

petabricks::FormulaPtr petabricks::Region::getSizeOfRuleInRemovedDimension(int d) const {
  JASSERT(isRemovedDimension(d))(d)(dimensions())(removedDimensions());
  
  size_t removedDimensionIndex= d-dimensions();
  FormulaPtr maxCoord = _removedDimensions.maxCoord[removedDimensionIndex];
  FormulaPtr minCoord = _removedDimensions.minCoord[removedDimensionIndex];
  
  return MaximaWrapper::instance().normalize(new FormulaSubtract(maxCoord, 
                                                                 minCoord));
}

void  petabricks::Region::addArgToScope(RIRScope& scope) const {
  switch(_originalType){
  case REGION_CELL:
    scope.set(_name, RIRSymbol::SYM_ARG_ELEMENT);
    break;
  case REGION_COL:
  case REGION_ROW:
  case REGION_BOX:
  case REGION_SLICE:
  case REGION_ALL:
    scope.set(_name, RIRSymbol::SYM_ARG_REGION);
    break;
  default:
    JASSERT(false).Text("Unreachable");
  }
}

void petabricks::Region::fixTypeIfVersioned() {
  if(! _version) {
    //Not versioned. Nothing to do
    return;
  }
  
  if (_originalType == REGION_ALL && removedDimensions() == 0) {
    //The versioning is done by adding a dimension to the matrix
    //Access to a version is done by slicing
    _originalType = REGION_SLICE;
  }
}
