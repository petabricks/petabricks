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
  :std::vector<FormulaPtr>(that), jalib::JRefCounted() 
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
  //o << "/*[*/" << _name << "/*]*/"; 
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
  JASSERT(left);
  JASSERT(right);
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
      ss << '(' << _left << " " << opStr() << " " << _right << ')';
      _toStringCache = ss.str();
    }
  }
  o << _toStringCache;
}

template < char OP >
const char* petabricks::FormulaBinop<OP>::opStr() {
  if(OP=='G') return ">=";
  if(OP=='L') return "<=";
  if(OP=='&') return "and";
  if(OP=='|') return "or";
  static const char v[] = {OP , 0};
  return v;
}

template < char OP >
void petabricks::FormulaBinop<OP>::explodeEquality(FormulaPtr& l, FormulaPtr& r) const {
  JASSERT(OP=='=' || OP=='<' || OP=='>' || OP==FormulaGE::CODE || OP==FormulaLE::CODE)
    (*this).Text("expected an equality");
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
  JASSERT(false)(l)(r)(*this).Text("expected an equality");
}

std::string petabricks::Formula::printAsAssumption() const { return toString(); }
std::string petabricks::Formula::toCppString() const { return toString(); }

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
  if(OP=='='){
    //maxima seems to like this form of equals better in older versions:
    return _left->toString() + "<=" + _right->toString() + " and "
         + _left->toString() + ">=" + _right->toString();
    //return "equal(" + _left->toString()
    //          + "," + _right->toString() + ")";
  }
  return toString();
}

template < char OP >
std::string petabricks::FormulaBinop<OP>::toCppString() const {
  if(OP=='=') return _left->toCppString() + "==" + _right->toCppString();
  if(OP=='&') return _left->toCppString() + "&&" + _right->toCppString();
  if(OP=='|') return _left->toCppString() + "||" + _right->toCppString();
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


char petabricks::Formula::opType() const {
  JASSERT(false).Text("not a binop");
  return 0;
}
  
template < char OP >
char petabricks::FormulaBinop<OP>::opType() const {
  return OP;
}
  
void petabricks::FormulaIf::print(std::ostream& o) const
{
  std::string elseClause= _else ? " else "+_else->toString() +" ": "";
  o << "(if " << _cond << " then " << _then << elseClause << ")";
}

petabricks::FormulaIf::FormulaIf(const FormulaPtr& cond, const FormulaPtr& thenClause, const FormulaPtr& elseClause) : 
    Formula(theNullFreeVarsList()),
    _cond(cond),
    _then(thenClause),
    _else(elseClause) {}
  
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
