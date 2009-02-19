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
  o << "if(" << _exprs[0] << ") " 
              << _then;
  if (_else) o << " else " << _else;
}

