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
#include "matrixdef.h"
#include "transform.h"
#include "maximawrapper.h"

// A hack for now, for inference we assume matrix is big
inline static hecura::FormulaPtr LARGE(){
  return new hecura::FormulaInteger(1000);
}

hecura::MatrixDef::MatrixDef(const char* name, const FormulaList& version, const FormulaList& size)
  : _name(name), _version(version), _size(size) 
{}

void hecura::MatrixDef::print(std::ostream& o) const {
  o << _name;
  if(!_version.empty()){
    o << '<';
    printStlList(o, _version.begin(), _version.end(), "..");
    o << '>';
  }
  if(!_size.empty()){
    o << '[';
    printStlList(o, _size.begin(), _size.end(), ", ");
    o << ']';
  }
}

void hecura::MatrixDef::initialize(Transform& trans){
  _version.normalize();
  _size.normalize();
}

void hecura::MatrixDef::exportConstants(Transform& trans){
  FreeVarsPtr tmp = _size.getFreeVariables();
  trans.constants().insert(tmp->begin(), tmp->end());
}

void hecura::MatrixDef::exportAssumptions(){
  for(FormulaList::const_iterator i=_size.begin(); i!=_size.end(); ++i){
    MaximaWrapper::instance().assume(new FormulaGT(*i, LARGE()));
  }
}

void hecura::MatrixDef::argDeclRW(std::vector<std::string>& args) const {
  args.push_back(matrixTypeName()+"& " + _name);
}
void hecura::MatrixDef::argDeclRO(std::vector<std::string>& args) const {
  args.push_back(constMatrixTypeName()+"& " + _name);
}
void hecura::MatrixDef::genAllocTmpCode(CodeGenerator& o){
  o.varDecl(matrixTypeName()+" "+_name);
}
void hecura::MatrixDef::generateCodeSimple(CodeGenerator& o){
  o.varDecl("Matrix" + jalib::XToString(_size.size()) + "D " + _name);
}
