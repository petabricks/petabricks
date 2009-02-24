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

const char* hecura::RIRNode::typeStr() const {
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

void hecura::RIRExpr::print(std::ostream& o) const {
  o<<_str;
  printStlList(o, _parts.begin(), _parts.end(), " ");
}
void hecura::RIRBlock::print(std::ostream& o) const {
  printStlList(o, _stmts.begin(), _stmts.end(), "\n");
}
void hecura::RIRBasicStmt::print(std::ostream& o) const {
  printStlList(o, _exprs.begin(), _exprs.end(), " ");
  o<<";";
}
void hecura::RIRBlockStmt::print(std::ostream& o) const {
  JASSERT(_exprs.size()==0);
  o << "{\n" << _block << "\n}";
}
void hecura::RIRLoopStmt::print(std::ostream& o) const {
  JASSERT(_exprs.size()==3);
  o << "for(" << _exprs[0] << "; " 
              << _exprs[1] << "; "
              << _exprs[2] << ") "
              << _body;
}
void hecura::RIRIfStmt::print(std::ostream& o) const {
  JASSERT(_exprs.size()==1);
  o << "if(" << _exprs[0] << ")\n" 
              << _then;
  if (_else) o << "\nelse\n" << _else;
}
void hecura::RIRRawStmt::print(std::ostream& o) const {
  o << _src;
}


namespace{
  template<typename T>
  void _visithelper(hecura::RIRVisitor& v, jalib::JRef<T>& t){
    v._before(t);
    if(t && v.shouldDescend(*t)) t->accept(v);
    v._after(t);
  }
  template<typename T>
  void _visitlisthelper(hecura::RIRVisitor& v, std::vector<jalib::JRef<T> >& t){
    std::vector<jalib::JRef<T> > t_alt;
    std::vector<jalib::JRef<T> > splicer;
    t_alt.swap(t);
    t.reserve(t_alt.size());
    v.pushSplicer(&splicer);
    for(typename std::vector<jalib::JRef<T> >::const_iterator i=t_alt.begin(); i!=t_alt.end(); ++i){
      jalib::JRef<T> p = *i;
      //call before
      v._before(p); 
      //recurse
      if(p && v.shouldDescend(*p)) p->accept(v);
      //insert spliced statements
      t.insert(t.end(), splicer.begin(), splicer.end());
      splicer.clear();
      //call after
      v._after(p);
      if(p) t.push_back(p);
      //insert spliced statements
      t.insert(t.end(), splicer.begin(), splicer.end());
      splicer.clear();
    }
    v.popSplicer(&splicer);
  }

}

void hecura::RIRExpr::accept(hecura::RIRVisitor& v) { 
  _visitlisthelper(v, _parts);
}
void hecura::RIRStmt::accept(hecura::RIRVisitor& v) {
  _visitlisthelper(v, _exprs);
}
void hecura::RIRBlock::accept(hecura::RIRVisitor& v) {
  _visitlisthelper(v, _stmts);
}
void hecura::RIRBasicStmt::accept(hecura::RIRVisitor& v) {
  RIRStmt::accept(v);
}
void hecura::RIRBlockStmt::accept(hecura::RIRVisitor& v) {
  RIRStmt::accept(v);
  _visithelper(v, _block);
}
void hecura::RIRLoopStmt::accept(hecura::RIRVisitor& v) {
  RIRStmt::accept(v);
  _visithelper(v, _body);
}
void hecura::RIRIfStmt::accept(hecura::RIRVisitor& v) {
  RIRStmt::accept(v);
  _visithelper(v, _then);
  _visithelper(v, _else);
}
void hecura::RIRRawStmt::accept(hecura::RIRVisitor& v) {
  RIRStmt::accept(v);
}

hecura::RIRExpr     * hecura::RIRExpr     ::clone() const { return new RIRExpr     (*this); }
hecura::RIRBlock    * hecura::RIRBlock    ::clone() const { return new RIRBlock    (*this); }
hecura::RIRBasicStmt* hecura::RIRBasicStmt::clone() const { return new RIRBasicStmt(*this); }
hecura::RIRBlockStmt* hecura::RIRBlockStmt::clone() const { return new RIRBlockStmt(*this); }
hecura::RIRLoopStmt * hecura::RIRLoopStmt ::clone() const { return new RIRLoopStmt (*this); }
hecura::RIRIfStmt   * hecura::RIRIfStmt   ::clone() const { return new RIRIfStmt   (*this); }
hecura::RIRRawStmt  * hecura::RIRRawStmt  ::clone() const { return new RIRRawStmt  (*this); }

std::string hecura::RIRNode::debugStr() const { 
  return typeStr(); 
}
std::string hecura::RIRExpr::debugStr() const { 
  return typeStr() + std::string(" ") + _str;
}


