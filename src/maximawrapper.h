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
#ifndef HECURAMAXIMAWRAPPER_H
#define HECURAMAXIMAWRAPPER_H

#include "formula.h"
#include "jconvert.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdio.h>

namespace hecura {

/**
 * A wrapper around an external maxima process
 */
class MaximaWrapper{
protected:
  MaximaWrapper();
  ~MaximaWrapper();
public:
  ///
  /// Singleton instance
  static MaximaWrapper& instance();

  ///
  /// Pass a command to maxima, parse result
  FormulaListPtr runCommand(const char* cmd, int len);
  FormulaListPtr runCommand(const std::string& cmd){ return runCommand(cmd.c_str(), cmd.length()); }

  ///
  /// Run a command and assert a single output
  FormulaPtr runCommandSingleOutput(const std::string& cmd){
    FormulaListPtr rslt = runCommand(cmd);
    JASSERT(rslt->size()==1)(rslt);
    return rslt->front();
  }

  ///
  /// Expand and simplify the given formula
  FormulaPtr normalize(const FormulaPtr& eq){
    if(eq->size()==1) 
      return eq; //cant simplify a leaf node
    return runCommandSingleOutput("expand(" + eq->toString() + ")");
  }

  FormulaPtr subst(const std::string& with, const std::string& what, const FormulaPtr& eq){
    return runCommandSingleOutput("subst("+with+", "+what+"," + eq->toString() + ")");
  }

  FormulaPtr subst(const Formula& formula, const FormulaPtr& eq){
    FormulaPtr l,r;
    formula.explodeEquality(l,r);
    if(l->hasIntersection(eq))
      return subst(r->toString(), l->toString(), eq);
    else
      return eq;
  }

  FormulaListPtr solve(const FormulaList& eqs, const std::string& var){
    FormulaList tmp;
    for(FormulaList::const_iterator i=eqs.begin(); i!=eqs.end(); ++i){
      if((*i)->getFreeVariables()->contains(var))
        tmp.push_back(*i);
    }
    return runCommand("solve([" + tmp.toString() + "], "+var+")");
  }

  enum tryCompareResult { NO, YES, UNKNOWN };

  bool compare(const FormulaPtr& a, const char* op, const FormulaPtr& b){
    tryCompareResult rslt = tryCompare(a,op,b);
    JASSERT(rslt==NO || rslt==YES);
    return rslt==YES;
  }

  tryCompareResult tryCompare(const FormulaPtr& a, const char* op, const FormulaPtr& b){
    std::string aStr = a->toString();
    std::string bStr = b->toString();
    if(strcmp(op,"=")==0){
      //optimize equal
      if(aStr == bStr) return YES;
    }
    std::string rslt = runCommandSingleOutput( "is("+ aStr + op + bStr + ")" )->toString();
    if(rslt=="1") return YES;
    if(rslt=="0") return NO;
    JWARNING(false)(a)(op)(b)(rslt).Text("Failed to determine relation between a and b");
    return UNKNOWN;
  }
  
  
  FormulaPtr min(const FormulaPtr& a, const FormulaPtr& b){
    return compare(a,">",b) ? b : a;
  }

  FormulaPtr max(const FormulaPtr& a, const FormulaPtr& b){
    return compare(a,"<",b) ? b : a;
  }

  void assume(const FormulaPtr& fact){
    runCommand("assume(" + fact->printAsAssumption() + ")");
  }

  
  void pushContext(){
    runCommand("supcontext(_ctx_stack_" + jalib::XToString(++_stackDepth) + ")");
  }
  
  void popContext(){
    JASSERT(_stackDepth>0);
    runCommand("killcontext(_ctx_stack_" + jalib::XToString(_stackDepth--) + ")");
  }

private:
  int _fd;
  int _stackDepth;
};

}

#endif

