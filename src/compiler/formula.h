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
#ifndef PETABRICKSFORMULA_H
#define PETABRICKSFORMULA_H

#include <set>
#include <vector>
#include <cstdlib>

#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/srcpos.h"
#include "common/jassert.h"

namespace petabricks {

class CoordinateFormula;
class Formula;
class FormulaList;
class FreeVars;
typedef jalib::JRef<const Formula> FormulaPtr;
typedef jalib::JRef<const FreeVars> FreeVarsPtr;
typedef jalib::JRef<CoordinateFormula> CoordinateFormulaPtr;
typedef jalib::JRef<FormulaList> FormulaListPtr;

class OrderedFreeVars : public std::vector<std::string> , public jalib::JRefCounted , public jalib::SrcPosTaggable {};

class FreeVar : public std::string {
public:
  FreeVar(const std::string& s, int flags=0) : std::string(s), _flags(flags) {}
  FreeVar(const char* s="", int flags=0)     : std::string(s), _flags(flags) {}

  enum FlagT {
    FLAG_SIZEVAR         = 1<<0,
    FLAG_SIZESPECIFICCFG = 1<<1,
    FLAG_FROMTUNABLE     = 1<<2
  };
  
  friend bool operator< (const FreeVar& a, const FreeVar& b){
    return reinterpret_cast<const std::string&>(a)
         < reinterpret_cast<const std::string&>(b);
  }


  bool hasFlag(FlagT f) const { return (_flags&f)!=0; }
  void addFlag(FlagT f) { _flags |= f; }
private:
  int _flags;
};


class FreeVars : public std::set<FreeVar>, public jalib::JRefCounted, public jalib::SrcPosTaggable {
public:
  bool contains(const std::string& s) const{ return find(s)!=end(); } 

  template <typename T>
  void eraseAll(const T& that) {
    typename T::const_iterator i;
    for(i=that.begin(); i!=that.end(); ++i)
      erase(*i);
  }

  template< typename T>
  void insertAll(const T& t) { insert(t.begin(), t.end()); }
};

/**
 * Collection of Formulas
 */
class FormulaList : public std::vector<FormulaPtr> 
                  , public jalib::JRefCounted
                  , public jalib::JPrintable
                  , public jalib::SrcPosTaggable 
{
public:
  FormulaList();
  FormulaList(const FormulaList& that);
  void normalize();
  void print(std::ostream& o) const;
  FreeVarsPtr getFreeVariables() const;
  void makeRelativeTo(const FormulaList& defs);

  void addToEach(const FormulaPtr& x);

  void subToEach(const FormulaPtr& x);

  void extend(const FormulaList& fl){
    insert(end(), fl.begin(), fl.end());
  }
};

/**
 * Collection of Formulas representing a N-dimensional coordinate
 */
class CoordinateFormula : public FormulaList  {
public:
};


/**
 * Abstract base class for formla tree
 */
class Formula : public jalib::JRefCounted, public jalib::JPrintable, public jalib::SrcPosTaggable {
protected:
  Formula(const FreeVarsPtr& fv) : _freeVars(fv), _size(1) {}
public:
  
  static FormulaPtr inf();

  FreeVarsPtr getFreeVariables() const { return _freeVars; } 
  
  void getFreeVariables(FreeVars& s) const {
    s.insert(_freeVars->begin(), _freeVars->end());
  }

  virtual void explodeEquality(FormulaPtr& l, FormulaPtr& r) const;
  
  FormulaPtr rhs() const {
    FormulaPtr l,r;
    explodeEquality(l,r);
    return r;
  }
  FormulaPtr lhs() const {
    FormulaPtr l,r;
    explodeEquality(l,r);
    return l;
  }
  ///
  /// Test if this set of free variables intersects that's set set of free variables
  bool hasIntersection(const Formula& that) const;

  int size() const { return _size; }

  virtual std::string printAsAssumption() const;
  virtual std::string toCppString() const;

  virtual FormulaPtr replace(const FormulaPtr& what, const FormulaPtr& with) const;

  virtual std::string explodePrint() const;
  
  virtual FormulaPtr ceiling() const { return this; }
  virtual FormulaPtr floor() const { return this; }

  FormulaPtr plusOne() const;
  FormulaPtr minusOne() const;
  FormulaPtr negative() const;

  virtual char opType() const;
  
  virtual FormulaPtr clone() const = 0;
  virtual double value() const {  JTRACE("Formula.value()")(toCppString());
                                  UNIMPLEMENTED(); abort(); }
  
protected:
  /// Set of all free variables in the tree
  FreeVarsPtr _freeVars;
  /// The number of elements in the tree
  int         _size;
  /// String representation of this Formula
};

/**
 * Node in a formula tree representing a variable
 */
class FormulaVariable : public Formula {
public:
  static FormulaPtr mktmp();
  FormulaVariable(const char* name);
  FormulaVariable(const std::string& name);
  void print(std::ostream& o) const;
  FormulaPtr clone() const { return FormulaPtr(new FormulaVariable(*this)); }
private:
  std::string _name;
};

/**
 * Node in a formula tree representing a conditional
 */
class FormulaIf : public Formula {
public:
  FormulaIf(const FormulaPtr& cond, const FormulaPtr& thenClause, const FormulaPtr& elseClause=FormulaPtr());
  void print(std::ostream& o) const;
  virtual FormulaPtr clone() const { FormulaPtr newCond = _cond->clone();
                                     FormulaPtr newThen = _then->clone();
                                     FormulaPtr newElse = _else->clone();
                                     return FormulaPtr(new FormulaIf(newCond, newThen, newElse));
                                   }
private:
  FormulaPtr _cond;
  FormulaPtr _then;
  FormulaPtr _else;
};

/**
 * Node in a formula tree representing an int or double
 */
template< typename T >
class FormulaLiteral: public Formula {
public:
  static FormulaPtr one()    { return new FormulaLiteral( 1); }
  static FormulaPtr two()    { return new FormulaLiteral( 2); }
  static FormulaPtr negOne() { return new FormulaLiteral(-1); }
  static FormulaPtr zero()   { return new FormulaLiteral( 0); }
  FormulaLiteral(T v);
  void print(std::ostream& o) const;
  virtual FormulaPtr clone() const { return FormulaPtr(new FormulaLiteral(*this)); }
  virtual double value() const { return _value; }
private:
  T _value;
};

typedef FormulaLiteral<int>    FormulaInteger;
typedef FormulaLiteral<double> FormulaFloat;
typedef FormulaLiteral<bool>   FormulaBool;

/**
 * Node in formula tree representing an operator
 */
template < char OP >
class FormulaBinop: public Formula {
public:
  enum { CODE = OP };
  FormulaBinop(const FormulaPtr& left, const FormulaPtr& right);
  void print(std::ostream& o) const;
  static const char* opStr();
  
  virtual void explodeEquality(FormulaPtr& l, FormulaPtr& r) const;
  
  virtual FormulaPtr replace(const FormulaPtr& what, const FormulaPtr& with) const;

  virtual FormulaPtr ceiling() const;
  virtual FormulaPtr floor() const;


  std::string toCppString() const;
  std::string printAsAssumption() const;
  std::string explodePrint() const;

  virtual char opType() const;

  virtual FormulaPtr clone() const {
    FormulaPtr newLeft = _left->clone();
    FormulaPtr newRight= _right->clone();
    FormulaBinop<OP>* newFormula= new FormulaBinop<OP>(newLeft, newRight);
    return FormulaPtr(newFormula);
  }
private:
  FormulaPtr _left;
  FormulaPtr _right;
  mutable std::string _toStringCache;
};

typedef FormulaBinop<'+'> FormulaAdd;
typedef FormulaBinop<'-'> FormulaSubtract;
typedef FormulaBinop<'*'> FormulaMultiply;
typedef FormulaBinop<'/'> FormulaDivide;
typedef FormulaBinop<'^'> FormulaExponent;
typedef FormulaBinop<'='> FormulaEQ;
typedef FormulaBinop<'>'> FormulaGT;
typedef FormulaBinop<'G'> FormulaGE;
typedef FormulaBinop<'<'> FormulaLT;
typedef FormulaBinop<'L'> FormulaLE;
typedef FormulaBinop<'&'> FormulaAnd;
typedef FormulaBinop<'|'> FormulaOr;

}

inline std::ostream& operator<<(std::ostream& o,  const petabricks::FormulaList& obj){
  jalib::JPrintable::printStlList(o, obj.begin(), obj.end(), ",");
  return o;
}


#endif
