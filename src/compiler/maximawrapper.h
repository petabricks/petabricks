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
#ifndef PETABRICKSMAXIMAWRAPPER_H
#define PETABRICKSMAXIMAWRAPPER_H

#include "formula.h"

#include "common/jconvert.h"

#include <stdio.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define MAXIMA MaximaWrapper::instance()

namespace petabricks {

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

  FormulaPtr floor(const FormulaPtr& eq){
    if(eq->size()==1) 
      return eq; //cant simplify a leaf node
    return runCommandSingleOutput("floor(" + eq->toString() + ")");
  }
  
  FormulaPtr ceiling(const FormulaPtr& eq){
    if(eq->size()==1) 
      return eq; //cant simplify a leaf node
    return runCommandSingleOutput("ceiling(" + eq->toString() + ")");
  }

  FormulaPtr staticCeiling(const FormulaPtr& eq){
    if(eq->size()==1) 
      return eq; //cant simplify a leaf node
    return runCommandSingleOutput("_tmp:fullratsimp("+eq->toString()+")$ fullratsimp( (num(_tmp)+denom(_tmp)-1) / denom(_tmp) )");
  }
  
  FormulaPtr normalize(const FormulaPtr& eq){
    if(eq->size()==1) 
      return eq; //cant simplify a leaf node
    return runCommandSingleOutput("fullratsimp(expand(" + eq->toString() + "))");
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

  FormulaPtr diff(const Formula& formula, const Formula& var){
    return runCommandSingleOutput("diff("+formula.toString()+","+var.toString()+")");
  }

  FormulaListPtr solve(const FormulaList& eqs, const std::string& var){
    FormulaList tmp;
    for(FormulaList::const_iterator i=eqs.begin(); i!=eqs.end(); ++i){
      if((*i)->getFreeVariables()->contains(var))
        tmp.push_back(*i);
    }
    return runCommand("solve([" + tmp.toString() + "], "+var+")");
  }
  
  FormulaListPtr solve(const FormulaPtr& eq, const std::string& var){
    FormulaList t;
    t.push_back(eq);
    return solve(t, var);
  }
  
  FormulaListPtr retrySolve(const FormulaList& eqs, const std::string& var, const char* cmp){
    FormulaListPtr rv = solve(eqs, var);
    if(rv->size()==0){
      // backup strategy:
      // try solving each part independently and merging results
      FormulaList tmp;
      tmp.resize(1);
      for(FormulaList::const_iterator i=eqs.begin(); i!=eqs.end(); ++i){
        if((*i)->getFreeVariables()->contains(var)){
          tmp[0]=*i;
          rv->extend(solve(tmp, var));
        }
      }
      JTRACE("used component based solve")(var)(rv);
    }
    if(rv->size()>1 && cmp!=NULL){
      JTRACE("trimming multiple results")(rv);
      while(rv->size()>1){
        if(tryCompare((*rv)[1]->rhs(), cmp, (*rv)[0]->rhs()) == YES){
          rv->erase(rv->begin());
        }else if(tryCompare((*rv)[0]->rhs(), cmp, (*rv)[1]->rhs()) == YES){
          rv->erase(rv->begin()+1);
        }else{
          break;
        }
      }
    }
    return rv;
  }
  
  //try harder to solve, returning the min answer on conflict
  FormulaListPtr minSolve(const FormulaList& eqs, const std::string& var){
    return retrySolve(eqs,var,"<");
  }

  //try harder to solve, returning the max answer on conflict
  FormulaListPtr maxSolve(const FormulaList& eqs, const std::string& var){
    return retrySolve(eqs,var,">");
  }

  enum tryCompareResult { NO, YES, UNKNOWN };

  bool compare(const FormulaPtr& a, const char* op, const FormulaPtr& b){
    tryCompareResult rslt = tryCompare(a,op,b);
    JASSERT(rslt==NO || rslt==YES)(a)(op)(b);
    return rslt==YES;
  }

  tryCompareResult tryCompare(const FormulaPtr& a, const char* op, const FormulaPtr& b){
    std::string aStr = a->toString();
    std::string bStr = b->toString();
    if(strcmp(op,"=")==0){
      //optimize equal
      if(aStr == bStr) return YES;
      return is( "equal("+ aStr + "," + bStr + ")" );
    }
    else
    {
      return is( aStr + op + bStr );
    }
  }

  tryCompareResult is(const std::string& formula){
    std::string rslt = runCommandSingleOutput( "is("+ formula + ")" )->toString();
    if(rslt=="1") return YES;
    if(rslt=="0") return NO;
    //JWARNING(false)(a)(op)(b)(rslt).Text("Failed to determine relation between a and b");
    return UNKNOWN;

  }
  
  
  FormulaPtr min(const FormulaPtr& a, const FormulaPtr& b){
    return tryCompare(a,"<=",b)==YES ? a : b;
  }

  FormulaPtr max(const FormulaPtr& a, const FormulaPtr& b){
    return tryCompare(a,">=",b)==YES ? a : b;
  }

  void assume(const FormulaPtr& fact){
    runCommand("assume(" + fact->printAsAssumption() + ")");
  }

  void declareInteger(const FormulaPtr& var){
    declareInteger(var->toString());
  }
  void declareInteger(const std::string& var){
    runCommand("declare(" + var + ", integer)");
  }
  
  void pushContext(){
    runCommand("supcontext(_ctx_stack_" + jalib::XToString(++_stackDepth) + ")");
  }
  
  void popContext(){
    JASSERT(_stackDepth>0);
    runCommand("killcontext(_ctx_stack_" + jalib::XToString(_stackDepth--) + ")");
  }

  void sanityCheck(){
    std::string rslt=runCommand("666.667")->toString();
    JASSERT(rslt=="666.667")(rslt)
      .Text("problem with maxima");
  }

private:
  int _fd;
  int _stackDepth;
};

}

#endif

