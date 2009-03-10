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
#ifndef HECURARIRCOMPILERPASS_H
#define HECURARIRCOMPILERPASS_H

#include "ruleir.h"

namespace hecura {

class RIRCompilerPass : public RIRVisitor {
protected:
  template<typename T> class Context {
  public:
    typedef std::list<T> TList;
    Context(TList* bk, TList* fw, int l) 
      : _backward(bk), _forward(fw), _lvl(l)
    {}
  private:
    TList* _backward;
    TList* _forward;
    int    _lvl;
  };
  typedef Context<RIRExprPtr> ExprContext;
  typedef Context<RIRStmtPtr> StmtContext;
  typedef std::vector<ExprContext> ExprContextStack;
  typedef std::vector<StmtContext> StmtContextStack;

public:
  //void before(RIRExprPtr&) {}
  //void before(RIRStmtPtr&) {}
  //void before(RIRBlockPtr&){}
  //void after(RIRExprPtr&)  {}
  //void after(RIRStmtPtr&)  {}
  //void after(RIRBlockPtr&) {}

  //bool shouldDescend(const RIRNode&) { return true; }

  virtual void beforeAny(const RIRNodePtr& n){}
  virtual void afterAny(const RIRNodePtr& n){}
protected:
  int depth() const { return _stack.size(); } 

private:
  void _before(RIRExprPtr& p)  { 
    _beforeAny(p.asPtr());
    RIRVisitor::_before(p);
  }
  void _before(RIRStmtPtr& p)  { 
    _beforeAny(p.asPtr());
    RIRVisitor::_before(p);
  }
  void _before(RIRBlockPtr& p) { 
    _beforeAny(p.asPtr());
    RIRVisitor::_before(p);
  }
  void _after(RIRExprPtr& p)   { 
    RIRVisitor::_after(p);
    _afterAny(p.asPtr());
  }
  void _after(RIRStmtPtr& p)   { 
    RIRVisitor::_after(p);
    _afterAny(p.asPtr());
  }
  void _after(RIRBlockPtr& p)  { 
    RIRVisitor::_after(p);
    _afterAny(p.asPtr());
  } 
  void _beforeAny(const RIRNodePtr& n){
    _stack.push_back(n);
    beforeAny(n);
  }
  void _afterAny(const RIRNodePtr& n){
    afterAny(n);
    JASSERT(!_stack.empty());
    _stack.pop_back();
  }
  void pushSplicer(RIRStmtList* bk, RIRStmtList* fwd){
    _stmtCtx.push_back(StmtContext(bk, fwd, level()));
  }
  void popSplicer(RIRStmtList* bk, RIRStmtList* fwd){
    JASSERT(!_stmtCtx.empty());
    _stmtCtx.pop_back();
  }
  void pushSplicer(RIRExprList* bk, RIRExprList* fwd){
    _exprCtx.push_back(ExprContext(bk, fwd, level()));
  }
  void popSplicer(RIRExprList* bk, RIRExprList* fwd){
    JASSERT(!_exprCtx.empty());
    _exprCtx.pop_back();
  }
  int level() const { return _stack.size(); }
protected:
  RIRNodeList _stack;
  ExprContextStack _exprCtx;
  StmtContextStack _stmtCtx;
};

class DebugPrintPass : public RIRCompilerPass {
public:
  void beforeAny(const RIRNodePtr& n){
    std::cout << std::string(depth()*2, ' ')
              << n->debugStr() << std::endl; 
  }
//void afterAny(const RIRNodePtr& n){
//  std::cout << std::string(depth()*2, ' ')
//            << n->debugStr() << std::endl; 
//}
};


}

#endif
