/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
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
#include "region.h"
#include "transform.h"
#include "maximawrapper.h"
#include "matrixdependency.h"
#include "rule.h"


#define RETURN_VAL_STR ""

hecura::Region::RegionType hecura::Region::strToRegionType(const std::string& str){
  if(str=="cell")   return REGION_CELL;
  if(str=="row")    return REGION_ROW;
  if(str=="col")    return REGION_COL;
  if(str=="column") return REGION_COL;
  if(str=="region") return REGION_BOX;
  JASSERT(false)(str).Text("Unknown region type");
  return REGION_INVALID;
}

hecura::Region::Region(const char* fromMatrix, const char* type, const FormulaList& bounds) 
  : _name(RETURN_VAL_STR)
  , _fromMatrixName(fromMatrix)
  , _originalType(strToRegionType(type))
  , _originalBounds(bounds)
{}

void hecura::Region::print(std::ostream& o) const {
  o << _fromMatrixName << ".region(" ;
  printStlList(o, _minCoord.begin(), _minCoord.end(), ", ");
  o << ", ";
  printStlList(o, _maxCoord.begin(), _maxCoord.end(), ", ");
  o << ") " << _name;
}

void hecura::Region::setName(const char* name){ 
  JWARNING(_name=="")(_name); 
  _name=name; 
  if(_name=="") _name=RETURN_VAL_STR;
}

void hecura::Region::initialize(Transform& trans) {
  _originalBounds.normalize();
   _fromMatrix = trans.lookupMatrix( _fromMatrixName );
  JASSERT(_fromMatrix);
  
  //convert given coordinates to a region
  switch(_originalType){
  case REGION_CELL: {
      //min = max = given
      JASSERT(_fromMatrix->numDimensions()==_originalBounds.size())(*this)(_fromMatrix->numDimensions())(_originalBounds.size())
        .Text("Wrong number of args given for cell");
      for(size_t i=0; i<_originalBounds.size(); ++i){
        _minCoord.push_back(_originalBounds[i]);
        _maxCoord.push_back(_originalBounds[i]);
      }
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
      _maxCoord.push_back(_originalBounds.front());
      _maxCoord.push_back(new FormulaSubtract(_fromMatrix->height(), FormulaInteger::one()));
      JTRACE("made column")(_fromMatrix)(_minCoord)(_maxCoord);
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
      _maxCoord.push_back(new FormulaSubtract(_fromMatrix->width(), FormulaInteger::one()));
      _maxCoord.push_back(_originalBounds.front());
      JTRACE("made row")(_fromMatrix)(_minCoord)(_maxCoord);
    }
    break;
  case REGION_BOX: {
      //min = (given[0], given[1])
      //max = (given[2], given[3])
      size_t s = _originalBounds.size();
      JASSERT(s%2==0)(s).Text("expected even number of args for 'region()'");
      JASSERT(_fromMatrix->numDimensions()==s/2)(*this)(_fromMatrix->numDimensions())(s)
        .Text("Wrong number of args given for region");
      for(size_t i=0; i<s/2; ++i)
        _minCoord.push_back(_originalBounds[i]);
      for(size_t i=s/2; i<s; ++i)
        _maxCoord.push_back(_originalBounds[i]);
    }
    break;
  default:
    JASSERT(false).Text("Unreachable");
    break;
  }
}

hecura::CoordinateFormula hecura::Region::calculateCenter() const {
  CoordinateFormula tmp;
  JASSERT(_minCoord.size()==_maxCoord.size())(_minCoord.size())(_maxCoord.size());
  for(size_t i=0; i<_minCoord.size(); ++i){
    tmp.push_back( new FormulaDivide(new FormulaAdd(_minCoord[i],_maxCoord[i]),
                                     new FormulaInteger(2)) );
  }
  tmp.normalize();
  return tmp;
}

void hecura::RegionList::makeRelativeTo(const FormulaList& defs){
  for(iterator i=begin(); i!=end(); ++i)
    (*i)->makeRelativeTo(defs);
}

void hecura::SimpleRegion::print(std::ostream& o) const {
  o << _minCoord << ", " << _maxCoord;
}

hecura::SimpleRegionPtr hecura::Region::getApplicableRegion(Rule& rule, const FormulaList& _defs){
  CoordinateFormula min;
  CoordinateFormula max;

  //first do min
  {
    FormulaList defs /*= _defs*/;
    for(size_t i=0; i<_minCoord.size(); ++i){
      defs.push_back(new FormulaEQ(FormulaInteger::zero(), _minCoord[i]));
    }
    FreeVarsPtr fv = defs.getFreeVariables();
    for(size_t i=0; i<_minCoord.size(); ++i){
      std::string var = rule.getOffsetVar(i)->toString();
      if(fv->contains(var))
        min.push_back(rule.trimImpossible(MaximaWrapper::instance().solve(defs, var))->rhs());
      else if(_minCoord[i]->getFreeVariables()->empty())
        min.push_back(_minCoord[i]);
      else
        min.push_back(FormulaInteger::zero());
    }
  }

  //then do max
  {
    FormulaList defs /*= _defs*/;
    for(size_t i=0; i<_maxCoord.size(); ++i){
      defs.push_back(new FormulaEQ( _fromMatrix->getMaxValueInDimension(i), _maxCoord[i]));
    }
    FreeVarsPtr fv = defs.getFreeVariables();
    for(size_t i=0; i<_maxCoord.size(); ++i){
      std::string var = rule.getOffsetVar(i)->toString();
      if(fv->contains(var))
        max.push_back(rule.trimImpossible(MaximaWrapper::instance().solve(defs, var))->rhs());
      else if(_maxCoord[i]->getFreeVariables()->empty())
        max.push_back(_maxCoord[i]);
      else
        max.push_back(_fromMatrix->getMaxValueInDimension(i));
    }
  }

  return new SimpleRegion(min,max);
}

hecura::SimpleRegionPtr hecura::SimpleRegion::intersect(const SimpleRegion& that) const{
  JASSERT(dimensions()==that.dimensions());
  CoordinateFormula min,max;
  for(size_t i=0; i<dimensions(); ++i){
    min.push_back( MaximaWrapper::instance().max(_minCoord[i], that._minCoord[i]) );
    max.push_back( MaximaWrapper::instance().min(_maxCoord[i], that._maxCoord[i]) );
  }
  return new SimpleRegion(min, max);
}

hecura::SimpleRegionPtr hecura::SimpleRegion::regionUnion(const SimpleRegion& that) const{
  JASSERT(dimensions()==that.dimensions());
  CoordinateFormula min,max;
  for(size_t i=0; i<dimensions(); ++i){
    min.push_back( MaximaWrapper::instance().min(_minCoord[i], that._minCoord[i]) );
    max.push_back( MaximaWrapper::instance().max(_maxCoord[i], that._maxCoord[i]) );
  }
  return new SimpleRegion(min, max);
}

bool hecura::SimpleRegion::hasIntersect(const SimpleRegion& that) const {
  if(toString() == that.toString()) //optimization
    return true;
  JASSERT(dimensions()==that.dimensions());
  for(size_t i=0; i<dimensions(); ++i){
    if( MaximaWrapper::instance().compare(maxCoord()[i], "<", minCoord()[i]) )
      return false;
    if( MaximaWrapper::instance().compare(minCoord()[i], ">", maxCoord()[i]) )
      return false;
  }
  return true;
}

std::string hecura::Region::generateSignatureCode(CodeGenerator& o) const{
  switch(_originalType){
  case REGION_CELL:
    return "Value& "  + _name;
  case REGION_COL:
    return "Column& " + _name;
  case REGION_ROW:
    return "Row& "    + _name;
  case REGION_BOX:
    return "Region& " + _name;
  default:
    JASSERT(false).Text("Unreachable");
    return "";
  }
}

std::string hecura::Region::generateAccessorCode(CodeGenerator& o) const{
  switch(_originalType){
  case REGION_CELL:
    return _fromMatrix->name() + ".get("+_minCoord.toString()+")";
  case REGION_COL:
    return _fromMatrix->name() + ".col("+_minCoord[0]->toString()+")";
  case REGION_ROW:
    return _fromMatrix->name() + ".row("+_minCoord[1]->toString()+")";
  case REGION_BOX:
    return _fromMatrix->name() + ".region("+_minCoord.toString() +", "+_maxCoord.toString()+")";
  default:
    JASSERT(false).Text("Unreachable");
    return "";
  }
}

void hecura::Region::collectDependencies(const Rule& rule, MatrixDependencyMap& map) const {
  //Determine dependency direction
  DependencyDirection direction(dimensions());
  for(size_t i=0; i<dimensions(); ++i){
    if(MaximaWrapper::instance().tryCompare(rule.getOffsetVar(i), "<=", _maxCoord[i])==MaximaWrapper::YES){
      switch(MaximaWrapper::instance().tryCompare(rule.getOffsetVar(i), "<", _minCoord[i])){
      case MaximaWrapper::YES:     direction.addDirection(i, DependencyDirection::D_LT); break;
      case MaximaWrapper::NO:      direction.addDirection(i, DependencyDirection::D_EQ); break;
      case MaximaWrapper::UNKNOWN: direction.addDirection(i, DependencyDirection::D_LE); break;
      default: JASSERT(false);
      }
    }else if(MaximaWrapper::instance().tryCompare(rule.getOffsetVar(i), ">=", _minCoord[i])==MaximaWrapper::YES){
      switch(MaximaWrapper::instance().tryCompare(rule.getOffsetVar(i), ">", _maxCoord[i])){
      case MaximaWrapper::YES:     direction.addDirection(i, DependencyDirection::D_GT); break;
      case MaximaWrapper::NO:      direction.addDirection(i, DependencyDirection::D_EQ); break;
      case MaximaWrapper::UNKNOWN: direction.addDirection(i, DependencyDirection::D_GE); break;
      default: JASSERT(false);
      }
    }else{
      direction.addDirection(i, DependencyDirection::D_ALL);
    }
  }

  const SimpleRegion& applicable = rule.applicanbleRegion();
  FormulaList minDefs;
  FormulaList maxDefs;
  for(size_t i=0; i<applicable.dimensions(); ++i){
    minDefs.push_back(new FormulaEQ(rule.getOffsetVar(i), applicable.minCoord()[i]));
    maxDefs.push_back(new FormulaEQ(rule.getOffsetVar(i), applicable.maxCoord()[i]));
  }
  CoordinateFormula minAbsolute = minCoord();
  CoordinateFormula maxAbsolute = maxCoord();
  minAbsolute.makeRelativeTo(minDefs);
  maxAbsolute.makeRelativeTo(maxDefs);
  SimpleRegionPtr region = new SimpleRegion(minAbsolute, maxAbsolute);

  //Merge with existing entry
  MatrixDependencyPtr dep = new MatrixDependency(direction, region);
  MatrixDependencyPtr& element = map[_fromMatrix];
  if(!element) element = dep;
  else         element->mergeWith(dep);
}

void hecura::Region::addAssumptions() const{

}
