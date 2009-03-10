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
#ifndef HECURARULEIR_H
#define HECURARULEIR_H

#include "jrefcounted.h"
#include "jprintable.h"

namespace hecura {

class RIRNode;
class RIRBlock;
class RIRStmt;
class RIRExpr;
typedef jalib::JRef<RIRNode,  jalib::JRefPolicyCopied<RIRNode>  > RIRNodePtr;
typedef jalib::JRef<RIRBlock, jalib::JRefPolicyCopied<RIRBlock> > RIRBlockPtr;
typedef jalib::JRef<RIRStmt,  jalib::JRefPolicyCopied<RIRStmt>  > RIRStmtPtr;
typedef jalib::JRef<RIRExpr,  jalib::JRefPolicyCopied<RIRExpr>  > RIRExprPtr;
typedef std::vector<RIRNodePtr>  RIRNodeList;
typedef std::vector<RIRStmtPtr>  RIRStmtList;
typedef std::vector<RIRExprPtr>  RIRExprList;

// interface for compiler passes
class RIRVisitor {
public: 
  //hooks called as we walk the tree -- used by compiler passes
  virtual void before(RIRExprPtr&)    {}
  virtual void before(RIRStmtPtr&)    {}
  virtual void before(RIRBlockPtr&)   {}
  virtual void after(RIRExprPtr&)     {}
  virtual void after(RIRStmtPtr&)     {}
  virtual void after(RIRBlockPtr&)    {}

  //base versions of the hooks used by infrastructure
  virtual void _before(RIRExprPtr& p)  { before(p); }
  virtual void _before(RIRStmtPtr& p)  { before(p); }
  virtual void _before(RIRBlockPtr& p) { before(p); }
  virtual void _after(RIRExprPtr& p)   { after(p);  }
  virtual void _after(RIRStmtPtr& p)   { after(p);  }
  virtual void _after(RIRBlockPtr& p)  { after(p);  }
   
  //allow visitor to short circuit over uninteresting nodes
  virtual bool shouldDescend(const RIRNode&) { return true; }

  //splicers allow new code to be inserted into the tree
  virtual void pushSplicer(RIRStmtList* s){}
  virtual void pushSplicer(RIRExprList* s){}
  virtual void popSplicer(RIRStmtList* s){}
  virtual void popSplicer(RIRExprList* s){}
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
    STMT_CONTROL,
    STMT_BLOCK,
    STMT_RAW,
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
protected:
  Type _type;
};

/**
 * Rule IR Expression Types
 */
class RIRExpr  : public RIRNode {
public:
  RIRExpr(Type t, const std::string& str="") : RIRNode(t), _str(str) {}
  void addSubExpr(const RIRExprPtr& p) { _parts.push_back(p); }
  void print(std::ostream& o) const;
  void accept(RIRVisitor&);
  RIRExpr* clone() const;
  std::string debugStr() const;
protected:
  std::string _str;
  RIRExprList _parts;
};

class RIRCallExpr  : public RIRExpr{
public:
  RIRCallExpr(): RIRExpr(EXPR_CALL) {}
  void print(std::ostream& o) const;
  RIRCallExpr* clone() const;
};

class RIRArgsExpr: public RIRExpr{
public:
  RIRArgsExpr(): RIRExpr(EXPR_ARGS) {}
  void print(std::ostream& o) const;
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
  void addExpr(const RIRExprPtr& p)   { _exprs.push_back(p); }
  void accept(RIRVisitor&);
  virtual RIRStmt* clone() const = 0;
protected:
  RIRExprList _exprs;
};

class RIRBasicStmt  : public RIRStmt {
public:
  RIRBasicStmt() : RIRStmt(STMT_BASIC) {}
  void print(std::ostream& o) const;
  void accept(RIRVisitor&);
  RIRBasicStmt* clone() const;
};

class RIRControlStmt  : public RIRStmt {
public:
  RIRControlStmt() : RIRStmt(STMT_CONTROL) {}
};

class RIRLoopStmt: public RIRControlStmt{
public:
  RIRLoopStmt(const RIRStmtPtr& p) { _body=p; }
  void print(std::ostream& o) const;
  void accept(RIRVisitor&);
  RIRLoopStmt* clone() const;
private:
  RIRStmtPtr _body;
};

class RIRSwitchStmt: public RIRControlStmt{
public:
  RIRSwitchStmt(const RIRStmtPtr& p) { _body=p; }
  void print(std::ostream& o) const;
  void accept(RIRVisitor&);
  RIRSwitchStmt* clone() const;
private:
  RIRStmtPtr _body;
};

class RIRIfStmt: public RIRControlStmt{
public:
  RIRIfStmt(const RIRStmtPtr& t, const RIRStmtPtr& e=0) 
    : _then(t)
    , _else(e) 
  {}
  void print(std::ostream& o) const;
  void accept(RIRVisitor&);
  RIRIfStmt* clone() const;
private:
  RIRStmtPtr _then;
  RIRStmtPtr _else;
};

typedef RIRBasicStmt   RIRReturnStmt;
typedef RIRBasicStmt   RIRCaseStmt;
typedef RIRControlStmt RIRBreakStmt;
typedef RIRControlStmt RIRContinueStmt;
typedef RIRControlStmt RIRInlineConditional;

class RIRBlockStmt  : public RIRStmt {
public:
  RIRBlockStmt(const RIRBlockPtr& p) : RIRStmt(STMT_BLOCK) { _block=p; }
  void print(std::ostream& o) const;
  void accept(RIRVisitor&);
  RIRBlockStmt* clone() const;
private:
  RIRBlockPtr _block;
};

class RIRRawStmt  : public RIRStmt {
public:
  RIRRawStmt(const std::string& txt) : RIRStmt(STMT_RAW) { _src=txt; }
  void print(std::ostream& o) const;
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
  void addStmt(const RIRStmtPtr& p) { _stmts.push_back(p); }
  void print(std::ostream& o) const;
  void accept(RIRVisitor&);
  RIRBlock* clone() const;
private:
  RIRStmtList _stmts;
};


}

#endif
