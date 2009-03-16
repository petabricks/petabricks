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
#ifndef PETABRICKSRIRCOMPILERPASS_H
#define PETABRICKSRIRCOMPILERPASS_H

#include "ruleir.h"
#include "rirscope.h"

namespace petabricks {

class RIRCompilerPass : public RIRVisitor {
protected:
  template<typename T> class Context {
  public:
    typedef std::list<T> TList;
    Context(TList* bk, TList* fw, int l) 
      : _backward(bk), _forward(fw), _lvl(l)
    {}
    TList* backward() const { return _backward;}
    TList* forward () const { return _forward; }
    int    lvl     () const { return _lvl;     }
  private:
    TList* _backward;
    TList* _forward;
    int    _lvl;
  };
  typedef Context<RIRExprCopyRef> ExprContext;
  typedef Context<RIRStmtCopyRef> StmtContext;
  typedef std::vector<ExprContext> ExprContextStack;
  typedef std::vector<StmtContext> StmtContextStack;
  
  RIRExprRef peekExprForward() { 
    RIRExprRef p = _exprCtx.back().forward()->front().asPtr();
    JASSERT(p).Text("unexpected end of statement");
    return p;
  }
  RIRExprRef popExprForward() { 
    RIRExprRef p = peekExprForward();
    _exprCtx.back().forward()->pop_front();
    return p;
  }

public:
  //void before(RIRExprCopyRef&) {}
  //void before(RIRStmtCopyRef&) {}
  //void before(RIRBlockCopyRef&){}
  //void after(RIRExprCopyRef&)  {}
  //void after(RIRStmtCopyRef&)  {}
  //void after(RIRBlockCopyRef&) {}

  //bool shouldDescend(const RIRNode&) { return true; }

  virtual void beforeAny(const RIRNodeRef& n){}
  virtual void afterAny(const RIRNodeRef& n){}
protected:
  int depth() const { return _stack.size(); } 

  RIRCompilerPass() : _scope(RIRScope::global()->createChildLayer()) {}

private:
  void _before(RIRExprCopyRef& p)  { 
    _beforeAny(p.asPtr());
    RIRVisitor::_before(p);
  }
  void _after(RIRExprCopyRef& p)   { 
    RIRVisitor::_after(p);
    _afterAny(p.asPtr());
  }
  
  void _before(RIRStmtCopyRef& p)  { 
    _beforeAny(p.asPtr());
    RIRVisitor::_before(p);
  }
  void _after(RIRStmtCopyRef& p)   { 
    RIRVisitor::_after(p);
    _afterAny(p.asPtr());
  }
 
  void _before(RIRBlockCopyRef& p) { 
    _scope=_scope->createChildLayer();
    _beforeAny(p.asPtr());
    RIRVisitor::_before(p);
  }
  void _after(RIRBlockCopyRef& p)  { 
    RIRVisitor::_after(p);
    _afterAny(p.asPtr());
    _scope=_scope->parentLayer();
  } 
  void _beforeAny(const RIRNodeRef& n){
    _stack.push_back(n);
    beforeAny(n);
  }
  void _afterAny(const RIRNodeRef& n){
    afterAny(n);
    JASSERT(!_stack.empty());
    _stack.pop_back();
  }
  void pushSplicer(RIRStmtList* bk, RIRStmtList* fwd){
    _stmtCtx.push_back(StmtContext(bk, fwd, depth()));
  }
  void popSplicer(RIRStmtList* bk, RIRStmtList* fwd){
    JASSERT(!_stmtCtx.empty());
    _stmtCtx.pop_back();
  }
  void pushSplicer(RIRExprList* bk, RIRExprList* fwd){
    _exprCtx.push_back(ExprContext(bk, fwd, depth()));
  }
  void popSplicer(RIRExprList* bk, RIRExprList* fwd){
    JASSERT(!_exprCtx.empty());
    _exprCtx.pop_back();
  }
protected:
  std::vector<RIRNodeRef>_stack;
  ExprContextStack _exprCtx;
  StmtContextStack _stmtCtx;
  RIRScopePtr      _scope;
};

class DebugPrintPass : public RIRCompilerPass {
public:
  void beforeAny(const RIRNodeCopyRef& n){
    std::cout << std::string(depth()*2, ' ')
              << n->debugStr() << std::endl; 
  }
//void afterAny(const RIRNodeCopyRef& n){
//  std::cout << std::string(depth()*2, ' ')
//            << n->debugStr() << std::endl; 
//}
};

class ExpansionPass : public RIRCompilerPass {
public:
  void before(RIRExprCopyRef& e){
    if(e->type() == RIRNode::EXPR_IDENT){
      RIRSymbolPtr sym = _scope->lookup(e->toString());
      if(sym && sym->type() == RIRSymbol::SYM_TRANSFORM_TEMPLATE){
        RIRExprList tmp;
        JASSERT(peekExprForward()->isLeaf("<"))(peekExprForward())
          .Text("Expected < after template transform");
        popExprForward();
        while(!peekExprForward()->isLeaf(">")){
          tmp.push_back(popExprForward().asPtr());
        }
        tmp.push_back(new RIROpExpr(","));
        popExprForward();
        JASSERT(!peekExprForward()->isLeaf())(peekExprForward())
          .Text("Expected (...) after template transform");
        RIRExprList::iterator i=peekExprForward()->parts().begin();
        JASSERT((*i)->isLeaf("("))(*i);
        ++i;
        peekExprForward()->parts().insert(i, tmp.begin(), tmp.end());
        JTRACE("handled template")(e)(tmp.size())(peekExprForward()->toString());
      }
    }
  }
};


}

#endif
