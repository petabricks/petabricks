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
#ifndef HECURAFORMULA_H
#define HECURAFORMULA_H

#include "jrefcounted.h"
#include "jprintable.h"
#include <vector>
#include <set>

namespace hecura {

class Formula;
class FormulaList;
class FreeVars;
typedef jalib::JRef<const Formula> FormulaPtr;
typedef jalib::JRef<FormulaList> FormulaListPtr;
typedef jalib::JRef<const FreeVars> FreeVarsPtr;
class FreeVars : public std::set<std::string> , public jalib::JRefCounted {
public:
  bool contains(const std::string& s) const{ return find(s)!=end(); } 
};

/**
 * Collection of Formulas
 */
class FormulaList : public std::vector<FormulaPtr> 
                  , public jalib::JRefCounted
                  , public jalib::JPrintable 
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
class Formula : public jalib::JRefCounted, public jalib::JPrintable {
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

  virtual FormulaPtr replace(const FormulaPtr& what, const FormulaPtr& with) const;

  virtual std::string explodePrint() const;
  
  virtual FormulaPtr staticCeiling() const { return this; }

  FormulaPtr plusOne() const;
  FormulaPtr minusOne() const;
  FormulaPtr negative() const;
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
private:
  std::string _name;
};

/**
 * Node in a formula tree representing an int or double
 */
template< typename T >
class FormulaLiteral: public Formula {
public:
  static FormulaPtr one()    { return new FormulaLiteral( 1); }
  static FormulaPtr negOne() { return new FormulaLiteral(-1); }
  static FormulaPtr zero()   { return new FormulaLiteral( 0); }
  FormulaLiteral(T v);
  void print(std::ostream& o) const;
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

  virtual FormulaPtr staticCeiling() const;
  std::string printAsAssumption() const;
  std::string explodePrint() const;
private:
  FormulaPtr _left;
  FormulaPtr _right;
  mutable std::string _toStringCache;
};

typedef FormulaBinop<'+'> FormulaAdd;
typedef FormulaBinop<'-'> FormulaSubtract;
typedef FormulaBinop<'*'> FormulaMultiply;
typedef FormulaBinop<'/'> FormulaDivide;
typedef FormulaBinop<'='> FormulaEQ;
typedef FormulaBinop<'>'> FormulaGT;
typedef FormulaBinop<'G'> FormulaGE;
typedef FormulaBinop<'<'> FormulaLT;
typedef FormulaBinop<'L'> FormulaLE;
typedef FormulaBinop<'&'> FormulaAnd;
typedef FormulaBinop<'|'> FormulaOr;

}

inline std::ostream& operator<<(std::ostream& o,  const hecura::FormulaList& obj){
  jalib::JPrintable::printStlList(o, obj.begin(), obj.end(), ",");
  return o;
}


#endif
