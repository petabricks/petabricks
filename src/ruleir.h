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
#ifndef PETABRICKSRULEIR_H
#define PETABRICKSRULEIR_H

#include "jrefcounted.h"
#include "jprintable.h"

#include <list>

namespace petabricks {

class RIRNode;
class RIRBlock;
class RIRStmt;
class RIRExpr;
typedef jalib::JRef<RIRNode,  jalib::JRefPolicyCopied<RIRNode>  > RIRNodeCopyRef;
typedef jalib::JRef<RIRBlock, jalib::JRefPolicyCopied<RIRBlock> > RIRBlockCopyRef;
typedef jalib::JRef<RIRStmt,  jalib::JRefPolicyCopied<RIRStmt>  > RIRStmtCopyRef;
typedef jalib::JRef<RIRExpr,  jalib::JRefPolicyCopied<RIRExpr>  > RIRExprCopyRef;
typedef jalib::JRef<RIRNode > RIRNodeRef;
typedef jalib::JRef<RIRBlock> RIRBlockRef;
typedef jalib::JRef<RIRStmt > RIRStmtRef;
typedef jalib::JRef<RIRExpr > RIRExprRef;
typedef std::list<RIRNodeCopyRef>  RIRNodeList;
typedef std::list<RIRStmtCopyRef>  RIRStmtList;
typedef std::list<RIRExprCopyRef>  RIRExprList;

// interface for compiler passes
class RIRVisitor {
public: 
  //hooks called as we walk the tree -- used by compiler passes
  virtual void before(RIRExprCopyRef&)    {}
  virtual void before(RIRStmtCopyRef&)    {}
  virtual void before(RIRBlockCopyRef&)   {}
  virtual void after(RIRExprCopyRef&)     {}
  virtual void after(RIRStmtCopyRef&)     {}
  virtual void after(RIRBlockCopyRef&)    {}

  //base versions of the hooks used by infrastructure
  virtual void _before(RIRExprCopyRef& p)  { before(p); }
  virtual void _before(RIRStmtCopyRef& p)  { before(p); }
  virtual void _before(RIRBlockCopyRef& p) { before(p); }
  virtual void _after(RIRExprCopyRef& p)   { after(p);  }
  virtual void _after(RIRStmtCopyRef& p)   { after(p);  }
  virtual void _after(RIRBlockCopyRef& p)  { after(p);  }
   
  //allow visitor to short circuit over uninteresting nodes
  virtual bool shouldDescend(const RIRNode&) { return true; }

  //splicers allow new code to be inserted into the tree
  virtual void pushSplicer(RIRStmtList* bk, RIRStmtList* fwd){}
  virtual void pushSplicer(RIRExprList* bk, RIRExprList* fwd){}
  virtual void popSplicer(RIRStmtList* bk, RIRStmtList* fwd){}
  virtual void popSplicer(RIRExprList* bk, RIRExprList* fwd){}
};

/**
 * Base class for all Rule IR types
 */
class RIRNode : public jalib::JRefCounted, public jalib::JPrintable {
public:
  enum Type {
    INVALID,
    EXPR          = 0x10000,
    EXPR_NIL,
    EXPR_OP,
    EXPR_LIT,
    EXPR_IDENT,
    EXPR_CHAIN,
    EXPR_CALL,
    EXPR_ARGS,
    EXPR_KEYWORD,
    STMT          = 0x20000,
    STMT_BASIC,
    STMT_BLOCK,
    STMT_RAW,
    STMT_LOOP,
    STMT_COND,
    STMT_BREAKCONTINUE,
    STMT_SWITCH,
    BLOCK         = 0x40000
  };
  RIRNode(Type t) : _type(t) {}
  Type type() const { return _type; }
  const char* typeStr() const;
  virtual std::string debugStr() const;
  bool isExpr()  const { return (_type&EXPR)  != 0; }
  bool isStmt()  const { return (_type&STMT)  != 0; }
  bool isBlock() const { return (_type&BLOCK) != 0; }
  virtual void accept(RIRVisitor&) = 0;
  virtual RIRNode* clone() const = 0;

  bool isControl() const { return _type==STMT_LOOP || _type==STMT_COND || _type==STMT_SWITCH || _type==STMT_BREAKCONTINUE; }


  virtual void print(std::ostream& o, RIRVisitor* printVisitor) = 0;
  void print(std::ostream& o) const {
    RIRNodeRef t = clone();
    t->print(o, NULL);
  }
protected:
  Type _type;
};

/**
 * Rule IR Expression Types
 */
class RIRExpr  : public RIRNode {
public:
  RIRExpr(Type t, const std::string& str="") : RIRNode(t), _str(str) {}
  void addSubExpr(const RIRExprCopyRef& p) { _parts.push_back(p); }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRExpr* clone() const;
  std::string debugStr() const;

  bool isLeaf(const char* val) const{
    return _parts.empty() && _str==val;
  }
  bool isLeaf() const{
    return _parts.empty();
  }

  bool containsLeaf(const char* val) const{
    if(isLeaf())
      return isLeaf(val);
    for(RIRExprList::const_iterator i=_parts.begin(); i!=_parts.end(); ++i)
      if((*i)->containsLeaf(val))
        return true;
    return false;
  }
  RIRExprList& parts(){ return _parts; }
protected:
  std::string _str;
  RIRExprList _parts;
};

class RIRCallExpr  : public RIRExpr{
public:
  RIRCallExpr(): RIRExpr(EXPR_CALL) {}
  void print(std::ostream& o, RIRVisitor* printVisitor);
  RIRCallExpr* clone() const;
};

class RIRArgsExpr: public RIRExpr{
public:
  RIRArgsExpr(): RIRExpr(EXPR_ARGS) {}
  void print(std::ostream& o, RIRVisitor* printVisitor);
  RIRArgsExpr* clone() const;
};

#define RIRNilExpr()       RIRExpr(RIRNode::EXPR_NIL)
#define RIROpExpr(s)       RIRExpr(RIRNode::EXPR_OP,s)
#define RIRLitExpr(s)      RIRExpr(RIRNode::EXPR_LIT,s)
#define RIRIdentExpr(s)    RIRExpr(RIRNode::EXPR_IDENT,s)
#define RIRChainExpr()     RIRExpr(RIRNode::EXPR_CHAIN)
#define RIRKeywordExpr(s)  RIRExpr(RIRNode::EXPR_KEYWORD, s)

/**
 * Rule IR Statement types
 */
class RIRStmt  : public RIRNode {
public:
  RIRStmt(Type t) : RIRNode(t) {}
  void addExpr(const RIRExprCopyRef& p)   { _exprs.push_back(p); }
  void accept(RIRVisitor&);
  virtual RIRStmt* clone() const = 0;
  
  //WARNING: this does not descend into sub-blocks
  virtual bool containsLeaf(const char* val) const{
    for(RIRExprList::const_iterator i=_exprs.begin(); i!=_exprs.end(); ++i)
      if((*i)->containsLeaf(val))
        return true;
    return false;
  }
protected:
  RIRExprList _exprs;
};

class RIRBasicStmt  : public RIRStmt {
public:
  RIRBasicStmt() : RIRStmt(STMT_BASIC) {}
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRBasicStmt* clone() const;
};

class RIRControlStmt  : public RIRStmt {
public:
  RIRControlStmt(Type t) : RIRStmt(t) {}
};

class RIRLoopStmt: public RIRControlStmt{
public:
  RIRLoopStmt(const RIRStmtCopyRef& p) : RIRControlStmt(STMT_LOOP) { _body=p; }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRLoopStmt* clone() const;
  bool containsLeaf(const char* val) const{
    return RIRStmt::containsLeaf(val)
        || _body->containsLeaf(val);
  }
private:
  RIRStmtCopyRef _body;
};

class RIRSwitchStmt: public RIRControlStmt{
public:
  RIRSwitchStmt(const RIRStmtCopyRef& p) : RIRControlStmt(STMT_SWITCH) { _body=p; }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRSwitchStmt* clone() const;
  bool containsLeaf(const char* val) const{
    return RIRStmt::containsLeaf(val)
        || _body->containsLeaf(val);
  }
private:
  RIRStmtCopyRef _body;
};

class RIRIfStmt: public RIRControlStmt{
public:
  RIRIfStmt(const RIRStmtCopyRef& t, const RIRStmtCopyRef& e=0) 
    :RIRControlStmt(STMT_COND)
    ,  _then(t)
    , _else(e) 
  {}
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRIfStmt* clone() const;
  bool containsLeaf(const char* val) const{
    return RIRStmt::containsLeaf(val)
        || _then->containsLeaf(val)
        || _else->containsLeaf(val);
  }
private:
  RIRStmtCopyRef _then;
  RIRStmtCopyRef _else;
};

typedef RIRBasicStmt   RIRReturnStmt;
typedef RIRBasicStmt   RIRCaseStmt;
typedef RIRControlStmt RIRBreakStmt;
typedef RIRControlStmt RIRContinueStmt;
typedef RIRControlStmt RIRInlineConditional;

class RIRBlockStmt  : public RIRStmt {
public:
  RIRBlockStmt(const RIRBlockCopyRef& p) : RIRStmt(STMT_BLOCK) { _block=p; }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRBlockStmt* clone() const;
  bool containsLeaf(const char* val) const;
private:
  RIRBlockCopyRef _block;
};

class RIRRawStmt  : public RIRStmt {
public:
  RIRRawStmt(const std::string& txt) : RIRStmt(STMT_RAW) { _src=txt; }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRRawStmt* clone() const;
private:
  std::string _src;
};

/**
 * Rule IR Basic Block
 */
class RIRBlock : public RIRNode {
public:
  RIRBlock() : RIRNode(BLOCK) {}
  void addStmt(const RIRStmtCopyRef& p) { _stmts.push_back(p); }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRBlock* clone() const;
  bool containsLeaf(const char* val) const{
    for(RIRStmtList::const_iterator i=_stmts.begin(); i!=_stmts.end(); ++i)
      if((*i)->containsLeaf(val))
        return true;
    return false;
  }
private:
  RIRStmtList _stmts;
};


}

#endif
