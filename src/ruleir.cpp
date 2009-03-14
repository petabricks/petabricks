/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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
#include "ruleir.h"

namespace{ 
  template<typename T> const T& get(const std::list<T>& lst, int n) {
    typename std::list<T>::const_iterator i=lst.begin();
    for(; n>0; --n,++i); 
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
    case STMT_CONTROL: return "STMT_CONTROL";
    case STMT_BLOCK  : return "STMT_BLOCK";
    case STMT_RAW    : return "STMT_RAW";
    case BLOCK       : return "BLOCK";
    default          : return "INVALID";
  }
}

void petabricks::RIRExpr::print(std::ostream& o) const {
  o<<_str;
  printStlList(o, _parts.begin(), _parts.end(), " ");
}
void petabricks::RIRArgsExpr::print(std::ostream& o) const {
  printStlList(o, _parts.begin(), _parts.end(), ", ");
}
void petabricks::RIRCallExpr::print(std::ostream& o) const {
  JASSERT(_parts.size()==2)(_parts.size())(_str);
  o << get(_parts,0) << '(' << get(_parts,1) << ')';
}
void petabricks::RIRBlock::print(std::ostream& o) const {
  printStlList(o, _stmts.begin(), _stmts.end(), "\n");
}
void petabricks::RIRBasicStmt::print(std::ostream& o) const {
  printStlList(o, _exprs.begin(), _exprs.end(), " ");
  o<<";";
}
void petabricks::RIRBlockStmt::print(std::ostream& o) const {
  JASSERT(_exprs.size()==0);
  o << "{\n" << _block << "\n}";
}
void petabricks::RIRLoopStmt::print(std::ostream& o) const {
  JASSERT(_exprs.size()==3);
  o << "for(" << get(_exprs,0) << "; " 
              << get(_exprs,1) << "; "
              << get(_exprs,2) << ") "
              << _body;
}
void petabricks::RIRSwitchStmt::print(std::ostream& o) const {
  JASSERT(_exprs.size()==1);
  o << "switch(" << _exprs.front() << ") " << _body;
}
void petabricks::RIRIfStmt::print(std::ostream& o) const {
  JASSERT(_exprs.size()==1);
  o << "if(" << _exprs.front() << ")\n" 
              << _then;
  if (_else) o << "\nelse\n" << _else;
}
void petabricks::RIRRawStmt::print(std::ostream& o) const {
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


