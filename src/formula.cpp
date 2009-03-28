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
#include "formula.h"
#include "maximawrapper.h"


namespace { //file local
  void printPower( std::string& o, const petabricks::FormulaPtr& base, const petabricks::FormulaPtr& power){
    JASSERT(power->size()==1)(base)(power).Text("Non-integer powers not yet supported");
    JASSERT(power->getFreeVariables()->size()==0)(base)(power).Text("Non-integer powers not yet supported");
    int p=jalib::StringToInt(power->toString());
    o="1";
    if(p>0){
      o = base->toString();
      while(--p>0) o+="*"+base->toString();
    }
  }
}

petabricks::FormulaList::FormulaList(const FormulaList& that) 
  : std::vector<FormulaPtr>(that) 
{} 

petabricks::FormulaList::FormulaList() {}

static const petabricks::FreeVarsPtr& theNullFreeVarsList(){
  static petabricks::FreeVarsPtr inst = new petabricks::FreeVars();
  return inst;
}

petabricks::FormulaVariable::FormulaVariable(const char* name) 
  : Formula(theNullFreeVarsList())
  , _name(name) 
{
  FreeVars* fv;
  _freeVars = fv = new FreeVars();
  fv->insert(_name);
}

petabricks::FormulaVariable::FormulaVariable(const std::string& name) 
  : Formula(theNullFreeVarsList())
  , _name(name) 
{
  FreeVars* fv;
  _freeVars = fv = new FreeVars();
  fv->insert(_name);
}

void petabricks::FormulaVariable::print(std::ostream& o) const { 
  o << _name; 
}

template < typename T >
petabricks::FormulaLiteral<T>::FormulaLiteral(T v) 
  : Formula(theNullFreeVarsList())
  , _value(v) 
{}

template < typename T >
void petabricks::FormulaLiteral<T>::print(std::ostream& o) const { 
  o << _value; 
}

template < char OP >
petabricks::FormulaBinop<OP>::FormulaBinop(const FormulaPtr& left, const FormulaPtr& right)
  : Formula(theNullFreeVarsList())
  , _left(left)
  , _right(right) 
{
  _size = _left->size() + _right->size();
  FreeVars* fv;
  _freeVars = fv = new FreeVars();
  _left->getFreeVariables( *fv );
  _right->getFreeVariables( *fv );
}

template < char OP >
void petabricks::FormulaBinop<OP>::print(std::ostream& o) const {
  if(_toStringCache.length()==0){
    if(OP=='^'){
      printPower(_toStringCache, _left, _right);
    }else{
      std::ostringstream ss;
      ss << '(' << _left << opStr() << _right << ')';
      _toStringCache = ss.str();
    }
  }
  o << _toStringCache;
}

template < char OP >
const char* petabricks::FormulaBinop<OP>::opStr() {
  if(OP=='G') return ">=";
  if(OP=='L') return "<=";
  if(OP=='&') return "&&";
  if(OP=='|') return "||";
  static const char v[] = {OP , 0};
  return v;
}

template < char OP >
void petabricks::FormulaBinop<OP>::explodeEquality(FormulaPtr& l, FormulaPtr& r) const {
  JASSERT(OP=='=')(*this).Text("expected an equality");
  l=_left;
  r=_right;
}

template < char OP >
petabricks::FormulaPtr petabricks::FormulaBinop<OP>::replace(const FormulaPtr& what, const FormulaPtr& with) const{
  if(what->toString()==toString()) return with;
  else return new FormulaBinop(_left->replace(what,with), _right->replace(what,with));
}
  

void petabricks::FormulaList::normalize(){
  for(iterator i = begin(); i!=end(); ++i)
    (*i) = MaximaWrapper::instance().normalize( *i );
}

void petabricks::FormulaList::print(std::ostream& o) const{
  printStlList(o, begin(), end(), ", ");
} 

petabricks::FreeVarsPtr petabricks::FormulaList::getFreeVariables() const {
  FreeVarsPtr ret;
  FreeVars* fv;
  ret = fv = new FreeVars();
  for(const_iterator i=begin(); i!=end(); ++i) 
    (*i)->getFreeVariables(*fv);
  return ret;
}

void petabricks::FormulaList::makeRelativeTo(const FormulaList& defs){
  for(iterator i=begin(); i!=end(); ++i){
    for(const_iterator d=defs.begin(); d!=defs.end(); ++d){
      *i = MaximaWrapper::instance().subst(*d, *i);
    }
  }
} 

bool petabricks::Formula::hasIntersection(const Formula& that) const{
  const FreeVars& a = getFreeVariables();
  const FreeVars& b = that.getFreeVariables();

  // speed optimization
  if(b.size() < a.size()) 
    return that.hasIntersection(*this);

  for(FreeVars::const_iterator i=a.begin(); i!=a.end(); ++i){
    if(b.find(*i) != b.end())
      return true;
  }
  return false;
}

petabricks::FormulaPtr petabricks::FormulaVariable::mktmp(){
  static jalib::AtomicT i = 0;
  std::string name = "_tmp" + jalib::XToString(jalib::atomicIncrementReturn(&i));
  return new FormulaVariable(name);
}

void petabricks::Formula::explodeEquality(FormulaPtr& l, FormulaPtr& r) const {
  JASSERT(false)(*this).Text("expected an equality");
}

std::string petabricks::Formula::printAsAssumption() const { return toString(); }

petabricks::FormulaPtr petabricks::Formula::replace(const FormulaPtr& what, const FormulaPtr& with) const {
  if(what->toString()==toString()) return with;
  else                             return this;
}


petabricks::FormulaPtr petabricks::Formula::plusOne() const {
  return MaximaWrapper::instance().normalize(new FormulaAdd(this, FormulaInteger::one()));
}
petabricks::FormulaPtr petabricks::Formula::minusOne() const {
  return MaximaWrapper::instance().normalize(new FormulaSubtract(this, FormulaInteger::one()));
}
petabricks::FormulaPtr petabricks::Formula::negative() const {
  return MaximaWrapper::instance().normalize(new FormulaSubtract(FormulaInteger::zero(), this));
}

std::string petabricks::Formula::explodePrint() const { JASSERT(false); return "";}

template < char OP >
std::string petabricks::FormulaBinop<OP>::printAsAssumption() const {
  if(OP=='=')
    return "equal(" + _left->toString()
              + "," + _right->toString() + ")";
  return toString();
}

template < char OP >
std::string petabricks::FormulaBinop<OP>::explodePrint() const{
  return _left->toString() +opStr()+ _right->toString();
}

template < char OP >
petabricks::FormulaPtr petabricks::FormulaBinop<OP>::ceiling() const { 
  if(OP=='/'){
    if(_freeVars->size()>0){
      JTRACE("Giving up on ceiling of")(*this);
      return this;
    }
    return MaximaWrapper::instance().ceiling(this);
  }else if(OP=='-')
    return new FormulaBinop(_left->ceiling(), _right->floor());
  else 
    return new FormulaBinop(_left->ceiling(), _right->ceiling());
}

template < char OP >
petabricks::FormulaPtr petabricks::FormulaBinop<OP>::floor() const { 
  if(OP=='/'){
    if(_freeVars->size()>0){
      JTRACE("Giving up on floor of")(*this);
      return this;
    }
    return MaximaWrapper::instance().floor(this);
  }else if(OP=='-')
    return new FormulaBinop(_left->floor(), _right->ceiling());
  else 
    return new FormulaBinop(_left->floor(), _right->floor());
}

void petabricks::FormulaList::addToEach(const FormulaPtr& x){
  for(iterator i=begin(); i!=end(); ++i)
    *i=new FormulaAdd(*i,x);
  normalize();
}

void petabricks::FormulaList::subToEach(const FormulaPtr& x){
  for(iterator i=begin(); i!=end(); ++i)
    *i=new FormulaSubtract(*i,x);
  normalize();
}

petabricks::FormulaPtr petabricks::Formula::inf(){
  return new FormulaVariable("inf");
}

//force implementations to be generated for templates
template class petabricks::FormulaLiteral<int>;
template class petabricks::FormulaLiteral<double>;
template class petabricks::FormulaLiteral<bool>;
template class petabricks::FormulaBinop<'+'>;
template class petabricks::FormulaBinop<'-'>;
template class petabricks::FormulaBinop<'*'>;
template class petabricks::FormulaBinop<'/'>;
template class petabricks::FormulaBinop<'^'>;
template class petabricks::FormulaBinop<'='>;
template class petabricks::FormulaBinop<'>'>;
template class petabricks::FormulaBinop<'G'>;
template class petabricks::FormulaBinop<'<'>;
template class petabricks::FormulaBinop<'L'>;
template class petabricks::FormulaBinop<'&'>;
template class petabricks::FormulaBinop<'|'>;
