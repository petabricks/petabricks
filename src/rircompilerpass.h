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
  virtual void splice(const RIRStmtPtr& stmt){
    JASSERT(!_stmtSplicers.empty());
    _stmtSplicers.back()->push_back(stmt);
  }

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
  void pushSplicer(RIRStmtList* s){
    _stmtSplicers.push_back(s); 
  }
  void popSplicer(RIRStmtList* s){
    JASSERT(!_stmtSplicers.empty());
    JASSERT(_stmtSplicers.back()==s);
    _stmtSplicers.pop_back();
  }
  //void popSplicer(RIRExprList* s){}
  //void pushSplicer(RIRExprList* s){}
protected:
  RIRNodeList _stack;
  std::vector<RIRStmtList*> _stmtSplicers;
};

class DebugPrintPass : public RIRCompilerPass {
public:
  void beforeAny(const RIRNodePtr& n){
    std::cout << std::string(depth(), '\t')
              << n->typeStr() << std::endl; 
  }
  void afterAny(const RIRNodePtr& n){
    std::cout << std::string(depth(), '\t')
              << n->typeStr() << std::endl; 
  }
};


}

#endif
