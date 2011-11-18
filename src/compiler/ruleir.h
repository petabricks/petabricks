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
#ifndef PETABRICKSRULEIR_H
#define PETABRICKSRULEIR_H

#include "common/jconvert.h"
#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/srcpos.h"

#include <list>
#include <map>
#include <string>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

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
  virtual void pushSplicer(RIRStmtList* /*bk*/, RIRStmtList* /*fwd*/){}
  virtual void pushSplicer(RIRExprList* /*bk*/, RIRExprList* /*fwd*/){}
  virtual void popSplicer(RIRStmtList*  /*bk*/, RIRStmtList* /*fwd*/){}
  virtual void popSplicer(RIRExprList*  /*bk*/, RIRExprList* /*fwd*/){}
};

/**
 * Base class for all Rule IR types
 */
class RIRNode : public jalib::JRefCounted,
                public jalib::JPrintable,
                public jalib::SrcPosTaggable {
  typedef std::map<std::string, std::string> AnnotationT;
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

  bool isControl() const { return _type==STMT_LOOP || _type==STMT_COND || _type==STMT_SWITCH; }

  virtual void print(std::ostream& o, RIRVisitor* printVisitor) = 0;
  void print(std::ostream& o) const {
    RIRNodeRef t = clone();
    t->print(o, NULL);
  }
  
  void addAnnotation(const std::string& name, const std::string& val=""){
    JWARNING(!hasAnnotation(name))(name);
    _annotations[name]=val;
  }
  void removeAnnotation(const std::string& name){
    JASSERT(hasAnnotation(name))(name);
    _annotations.erase(_annotations.find(name));
  }
  bool hasAnnotation(const std::string& name) const {
    return _annotations.find(name) != _annotations.end();
  }
  const std::string& getAnnotation(const std::string& name) const {
    AnnotationT::const_iterator i = _annotations.find(name);
    JASSERT(i != _annotations.end())(name);
    return i->second;
  }
  
  /** Return the number of operations executed by this node, approximated
   * by the number of EXPR_OP it contains */
  virtual unsigned int opsNumber() const = 0;
  
protected:
  Type _type;
  AnnotationT _annotations;
};

/**
 * Rule IR Expression Types
 */
class RIRExpr  : public RIRNode {
public:
  static RIRExprCopyRef parse(const std::string& str, const jalib::SrcPosTaggable*);
  RIRExpr(Type t, const std::string& str="") : RIRNode(t), _str(str) {}
  void addSubExpr(const RIRExprCopyRef& p) { _parts.push_back(p); }
  void prependSubExpr(const RIRExprCopyRef& p) { _parts.push_front(p); }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRExpr* clone() const;
  std::string debugStr() const;

  const std::string& str() const
    {
      return _str;
    }

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
  RIRExprCopyRef part(int n) const{ 
    JASSERT(n<(int)_parts.size());
    RIRExprList::const_iterator i=_parts.begin();
    while(n-->0) ++i;
    return *i;
  }
  
  virtual unsigned int opsNumber() const {
    if(type()==EXPR_OP) {
      return 1;
    }
    unsigned int sum = 0;
    for (RIRExprList::const_iterator i=_parts.begin(), e=_parts.end();
         i != e;
         ++i) {
      sum += (*i)->opsNumber();
    }
    return sum;
  }
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
  static RIRStmtCopyRef parse(const std::string& str, const jalib::SrcPosTaggable*);

  RIRStmt(Type t) : RIRNode(t) {}
  void addExpr(const RIRExprCopyRef& p){ _exprs.push_back(p); }
  void accept(RIRVisitor&);
  virtual RIRStmt* clone() const = 0;
  
  virtual bool containsLeaf(const char* val) const{
    for(RIRExprList::const_iterator i=_exprs.begin(); i!=_exprs.end(); ++i)
      if((*i)->containsLeaf(val))
        return true;
    return false;
  }
  
  const RIRExprCopyRef& part(int n) const;
  RIRExprCopyRef& part(int n);
  
  //remove the last Expr and return it
  RIRExprCopyRef popExpr(){
    RIRExprCopyRef t = _exprs.back();
    _exprs.pop_back();
    return t;
  }

  int numExprs() const { return (int)_exprs.size(); }
  
  virtual const RIRBlockCopyRef& extractBlock() const { UNIMPLEMENTED(); return *static_cast<const RIRBlockCopyRef*>(0); }
  
  virtual unsigned int opsNumber() const { unsigned int sum = 0;
                                           for (RIRExprList::const_iterator i=_exprs.begin(),
                                                                            e=_exprs.end();
                                                i != e;
                                                ++i) {
                                             sum += (*i)->opsNumber();
                                           }
                                           return sum;
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
  const RIRExprCopyRef& declPart() const { return part(0); }
  const RIRExprCopyRef& testPart() const { return part(1); }
  const RIRExprCopyRef& incPart() const { return part(2); }
  const RIRStmtCopyRef& body() const { return _body; }
  RIRExprCopyRef& declPart() { return part(0); }
  RIRExprCopyRef& testPart() { return part(1); }
  RIRExprCopyRef& incPart() { return part(2); }
  RIRStmtCopyRef& body() { return _body; }

  RIRLoopStmt* initForEnough(const RIRExprCopyRef& min = new RIRLitExpr(jalib::XToString(FORENOUGH_MIN_ITERS)),
                             const RIRExprCopyRef& max = new RIRLitExpr(jalib::XToString(FORENOUGH_MAX_ITERS)))
  {
    addAnnotation("for_enough");
    addExpr(new RIRNilExpr());
    addExpr(new RIRNilExpr());
    addExpr(new RIRNilExpr());
    addExpr(min);
    addExpr(max);
    return this;
  }
  
  virtual unsigned int opsNumber() const {
    return declPart()->opsNumber() +
           testPart()->opsNumber() +
           incPart()->opsNumber() +
           body()->opsNumber();
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
  
  virtual unsigned int opsNumber() const { return RIRStmt::opsNumber() +
                                                  _body->opsNumber();
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
        ||(_else &&  _else->containsLeaf(val));
  }
  const RIRExprCopyRef& condPart() const { return _exprs.front(); }
  const RIRStmtCopyRef& thenPart() const { return _then; }
  const RIRStmtCopyRef& elsePart() const { return _else; }
  
  virtual unsigned int opsNumber() const {
    return condPart()->opsNumber() +
           thenPart()->opsNumber() +
           (elsePart() ? elsePart()->opsNumber() : 0);
  }
private:
  RIRStmtCopyRef _then;
  RIRStmtCopyRef _else;
};

typedef RIRBasicStmt   RIRReturnStmt;
typedef RIRBasicStmt   RIRCaseStmt;
typedef RIRBasicStmt   RIRBreakStmt;
typedef RIRBasicStmt   RIRContinueStmt;
typedef RIRControlStmt RIRInlineConditional;

class RIRBlockStmt  : public RIRStmt {
public:
  RIRBlockStmt(const RIRBlockCopyRef& p) : RIRStmt(STMT_BLOCK) { _block=p; }
  void print(std::ostream& o, RIRVisitor* printVisitor);
  void accept(RIRVisitor&);
  RIRBlockStmt* clone() const;
  bool containsLeaf(const char* val) const;
  const RIRBlockCopyRef& extractBlock() const { return _block; }
  virtual unsigned int opsNumber() const;
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
  static RIRBlockCopyRef parse(const std::string& str, const jalib::SrcPosTaggable*);

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

  const RIRStmtList& stmts() const { return _stmts; }
  
  unsigned int opsNumber() const { unsigned int sum = 0;
                                   for (RIRStmtList::const_iterator i=_stmts.begin(),
                                                                    e=_stmts.end();
                                        i != e;
                                        ++i) {
                                     sum += (*i)->opsNumber();
                                   }
                                   return sum;
                                 }
private:
  RIRStmtList _stmts;
};


}

#endif
