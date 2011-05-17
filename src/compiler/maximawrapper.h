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
#ifndef PETABRICKSMAXIMAWRAPPER_H
#define PETABRICKSMAXIMAWRAPPER_H

#include "formula.h"

#include "common/jconvert.h"

#include <stdio.h>
#include <map>
#include <string>

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
  FormulaListPtr runCommandRaw(const char* cmd, int len);

  ///
  /// Perform simple caching and call runCommandRaw
  FormulaListPtr runCommand(const std::string& cmd);
  
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
    std::string test = aStr + op + bStr;
    if( strcmp(op,"=") == 0 ){ 
      // maxima doesn't like '=' or equal()
      test = aStr + "<=" + bStr + " and " + aStr + ">=" + bStr;
    }
    
    if(aStr == bStr) {
      //optimize when identical
      if(strcmp(op,"=")==0 || strcmp(op,"<=")==0 || strcmp(op,">=")==0){ 
        return YES;
      }
      if(strcmp(op,"<")==0 || strcmp(op,">")==0){ 
        return NO;
      }
    }
    return is(test);
  }

  tryCompareResult is(const std::string& formula){
    //some simple optimizations:
    if(formula=="1<0") return NO;
    if(formula=="0>1") return NO;
    if(formula=="1>0") return YES;
    if(formula=="0<1") return YES;

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
    _pendingAssume.insert("assume(" + fact->printAsAssumption() + ")");
  }

  void declareInteger(const FormulaPtr& var){
    declareInteger(var->toString());
  }
  void declareInteger(const std::string& var){
    _pendingAssume.insert("declare(" + var + ", integer)");
  }
  
  void pushContext(){
    runCommand("supcontext(_ctx_stack_" + jalib::XToString(++_stackDepth) + ")");
  }
  
  void popContext(){
    JASSERT(_stackDepth>0);
    _pendingAssume.clear();
    runCommand("killcontext(_ctx_stack_" + jalib::XToString(_stackDepth--) + ")");
    clearCache();
  }

  void sanityCheck(){
    clearCache();
    std::string rslt=runCommand("666.667")->toString();
    JASSERT(rslt=="666.667")(rslt).Text("problem with maxima");
  }

  void clearCache(){ _cache.clear(); }
private:
  int _fd;
  int _stackDepth;
  typedef std::map<std::string, FormulaListPtr> CacheT;
  typedef std::set<std::string> ContextT;
  ContextT _pendingAssume;
  CacheT   _cache;
};

}

#endif

