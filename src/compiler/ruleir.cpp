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
#include "ruleir.h"

#include <algorithm>


//defined in ruleirparser.{ypp,cpp}
petabricks::RIRBlockCopyRef parseRuleBody(const std::string& str);

petabricks::RIRBlockCopyRef petabricks::RIRBlock::parse(const std::string& str){
  return parseRuleBody(str);
}
petabricks::RIRStmtCopyRef petabricks::RIRStmt::parse(const std::string& str){
  RIRBlockCopyRef t = RIRBlock::parse(str);
  JASSERT(t->stmts().size()==1)(t->stmts().size());
  return t->stmts().front();
}
petabricks::RIRExprCopyRef petabricks::RIRExpr::parse(const std::string& str){
  RIRStmtCopyRef t = RIRStmt::parse(str+";");
  JASSERT(t->numExprs()==1)(t->numExprs());
  return t->part(0);
}
  

namespace{ 
  template<typename T> T& get(std::list<T>& lst, int n) {
    typename std::list<T>::iterator i=lst.begin();
    for(; n>0; --n,++i) ; 
    return *i;
  }
}

const char* petabricks::RIRNode::typeStr() const {
  switch(type()){
    case EXPR        : return "EXPR";
    case EXPR_NIL    : return "EXPR_NIL";
    case EXPR_OP     : return "EXPR_OP";
    case EXPR_LIT    : return "EXPR_LIT";
    case EXPR_IDENT  : return "EXPR_IDENT";
    case EXPR_CHAIN  : return "EXPR_CHAIN";
    case EXPR_CALL   : return "EXPR_CALL";
    case EXPR_ARGS   : return "EXPR_ARGS";
    case EXPR_KEYWORD: return "EXPR_KEYWORD";
    case STMT        : return "STMT";
    case STMT_BASIC  : return "STMT_BASIC";
    case STMT_BLOCK  : return "STMT_BLOCK";
    case STMT_RAW    : return "STMT_RAW";
    case STMT_LOOP   : return "STMT_LOOP";
    case STMT_COND   : return "STMT_COND";
    case STMT_SWITCH : return "STMT_SWITCH";
    case BLOCK       : return "BLOCK";
    default          : return "INVALID";
  }
}

template<typename A>
static void pvHook(A& a, std::ostream& o, petabricks::RIRVisitor* v){
    if(v!=NULL)
      a->accept(*v);
    else
      o<<a;
}
template<typename A>
static void pvHook(A& a, std::ostream& o, petabricks::RIRVisitor* v, const char* delim){
  for(typename A::iterator i=a.begin(); i!=a.end(); ++i){
    if(i!=a.begin()) 
      o<<delim;
    pvHook((*i), o, v);
  }
}


void petabricks::RIRExpr::print(std::ostream& o, RIRVisitor* v) {
  o << _str;
  pvHook(_parts, o, v, " ");
}
void petabricks::RIRArgsExpr::print(std::ostream& o, RIRVisitor* v) {
  pvHook(_parts, o, v, ", ");
}
void petabricks::RIRCallExpr::print(std::ostream& o, RIRVisitor* v) {
  JASSERT(_parts.size()==2)(_parts.size())(_str);
  pvHook(get(_parts,0),o,v);
  o << '(';
  pvHook(get(_parts,1),o,v);
  o << ')';
}
void petabricks::RIRBlock::print(std::ostream& o, RIRVisitor* v) {
  pvHook(_stmts, o, v, "\n");
}
void petabricks::RIRBasicStmt::print(std::ostream& o, RIRVisitor* v) {
  pvHook(_exprs, o, v, " ");
  o<<";";
}
void petabricks::RIRBlockStmt::print(std::ostream& o, RIRVisitor* v) {
  JASSERT(_exprs.size()==0);
  o << "{\n";
  pvHook(_block,o,v);
  o << "\n}";
}
void petabricks::RIRLoopStmt::print(std::ostream& o, RIRVisitor* v) {
  JASSERT(_exprs.size()>=3);
  o << "for("  ; pvHook(get(_exprs,0),o,v); o<< "; ";
                 pvHook(get(_exprs,1),o,v); o<< "; ";
                 pvHook(get(_exprs,2),o,v); o<< ") ";
                 pvHook(_body, o, v);
}
void petabricks::RIRSwitchStmt::print(std::ostream& o, RIRVisitor* v) {
  JASSERT(_exprs.size()==1);
  o << "switch(" ;
  pvHook(_exprs.front(),o,v); 
  o << ") ";
  pvHook(_body, o,v);
}
void petabricks::RIRIfStmt::print(std::ostream& o, RIRVisitor* v) {
  JASSERT(_exprs.size()==1);
  o << "if(" ; 
  pvHook(_exprs.front(),o,v); 
  o << ")\n" ;
  pvHook(_then,o,v);
  if (_else){
    o << "\nelse\n";
    pvHook(_else,o,v);
  }
}
void petabricks::RIRRawStmt::print(std::ostream& o, RIRVisitor*) {
  o << _src;
}

namespace{
  template<typename T>
  void _visithelper(petabricks::RIRVisitor& v, T& t){
    v._before(t);
    if(t && v.shouldDescend(*t)) t->accept(v);
    v._after(t);
  }
  template<typename T>
  void _visitlisthelper(petabricks::RIRVisitor& v, std::list<T>& bk){
    std::list<T> fwd;
    bk.swap(fwd);
    v.pushSplicer(&bk, &fwd);
    while(!fwd.empty()){
      T p = fwd.front(); 
      fwd.pop_front();
      _visithelper(v, p);
      if(p) bk.push_back(p);
    }
    v.popSplicer(&bk, &fwd);
  }

}

void petabricks::RIRExpr::accept(petabricks::RIRVisitor& v) { 
  _visitlisthelper(v, _parts);
}
void petabricks::RIRStmt::accept(petabricks::RIRVisitor& v) {
  _visitlisthelper(v, _exprs);
}
void petabricks::RIRBlock::accept(petabricks::RIRVisitor& v) {
  _visitlisthelper(v, _stmts);
}
void petabricks::RIRBasicStmt::accept(petabricks::RIRVisitor& v) {
  RIRStmt::accept(v);
}
void petabricks::RIRBlockStmt::accept(petabricks::RIRVisitor& v) {
  RIRStmt::accept(v);
  _visithelper(v, _block);
}
void petabricks::RIRLoopStmt::accept(petabricks::RIRVisitor& v) {
  RIRStmt::accept(v);
  _visithelper(v, _body);
}
void petabricks::RIRIfStmt::accept(petabricks::RIRVisitor& v) {
  RIRStmt::accept(v);
  _visithelper(v, _then);
  if(_else) _visithelper(v, _else);
}
void petabricks::RIRSwitchStmt::accept(petabricks::RIRVisitor& v) {
  RIRStmt::accept(v);
  _visithelper(v, _body);
}
void petabricks::RIRRawStmt::accept(petabricks::RIRVisitor& v) {
  RIRStmt::accept(v);
}

petabricks::RIRExpr      * petabricks::RIRExpr      ::clone() const { return new RIRExpr      (*this); }
petabricks::RIRBlock     * petabricks::RIRBlock     ::clone() const { return new RIRBlock     (*this); }
petabricks::RIRBasicStmt * petabricks::RIRBasicStmt ::clone() const { return new RIRBasicStmt (*this); }
petabricks::RIRBlockStmt * petabricks::RIRBlockStmt ::clone() const { return new RIRBlockStmt (*this); }
petabricks::RIRLoopStmt  * petabricks::RIRLoopStmt  ::clone() const { return new RIRLoopStmt  (*this); }
petabricks::RIRIfStmt    * petabricks::RIRIfStmt    ::clone() const { return new RIRIfStmt    (*this); }
petabricks::RIRRawStmt   * petabricks::RIRRawStmt   ::clone() const { return new RIRRawStmt   (*this); }
petabricks::RIRSwitchStmt* petabricks::RIRSwitchStmt::clone() const { return new RIRSwitchStmt(*this); }
petabricks::RIRCallExpr  * petabricks::RIRCallExpr  ::clone() const { return new RIRCallExpr  (*this); }
petabricks::RIRArgsExpr  * petabricks::RIRArgsExpr  ::clone() const { return new RIRArgsExpr  (*this); }

std::string petabricks::RIRNode::debugStr() const { 
  return typeStr(); 
}
std::string petabricks::RIRExpr::debugStr() const { 
  return typeStr() + std::string(" ") + _str;
}

bool petabricks::RIRBlockStmt::containsLeaf(const char* val) const{
  return RIRStmt::containsLeaf(val)
      || _block->containsLeaf(val);
}

const petabricks::RIRExprCopyRef& petabricks::RIRStmt::part(int n) const { 
  JASSERT((int)_exprs.size()>n)(n)(_exprs.size());
  RIRExprList::const_iterator i = _exprs.begin();
  std::advance(i, n);
  return *i;
}
petabricks::RIRExprCopyRef& petabricks::RIRStmt::part(int n) { 
  JASSERT((int)_exprs.size()>n)(n)(_exprs.size());
  RIRExprList::iterator i = _exprs.begin();
  std::advance(i, n);
  return *i;
}








