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
#include "formula.h"

// A hack for now, for inference we assume matrix is big
inline static petabricks::FormulaPtr LARGE(){
  return new petabricks::FormulaInteger(1000);
}

petabricks::MatrixDef::MatrixDef(const char* name, const FormulaList& version, const FormulaList& size)
  : _name(name), _version(version), _size(size) 
{}

void petabricks::MatrixDef::print(std::ostream& o) const {
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

void petabricks::MatrixDef::initialize(Transform& trans){
//   _version.normalize();
//   _size.normalize();
  if(_version.size()>0){
    //TODO support for min region size
    JASSERT(_version.size()<=2);
    JASSERT(_version[0]->getFreeVariables()->size()==0)(_version)
      .Text("Non constant minimum version size not yet supported");
    _size.push_back(_version.back()->plusOne());
  }
}

void petabricks::MatrixDef::exportConstants(Transform& trans){
  FreeVarsPtr tmp = _size.getFreeVariables();
  trans.constants().insert(tmp->begin(), tmp->end());
}

void petabricks::MatrixDef::exportAssumptions(){
  for(FormulaList::const_iterator i=_size.begin(); i!=_size.end(); ++i){
    MaximaWrapper::instance().assume(new FormulaGT(*i, LARGE()));
  }
}

void petabricks::MatrixDef::argDeclRW(std::vector<std::string>& args, bool byRef) const {
  if(byRef)
    args.push_back("const "+matrixTypeName()+"& " + _name);
  else
    args.push_back("const "+matrixTypeName()+" " + _name);
}
void petabricks::MatrixDef::argDeclRO(std::vector<std::string>& args, bool byRef) const {
  if(byRef)
    args.push_back("const "+constMatrixTypeName()+"& " + _name);
  else
    args.push_back("const "+constMatrixTypeName()+" " + _name);
}
void petabricks::MatrixDef::genAllocTmpCode(CodeGenerator& o){
  o.varDecl(matrixTypeName()+" "+_name);
}
void petabricks::MatrixDef::generateCodeSimple(CodeGenerator& o){
  o.varDecl("Matrix" + jalib::XToString(_size.size()) + "D " + _name);
}
void petabricks::MatrixDef::extractDefines(FreeVars& defined, CodeGenerator& o){
  int d=0;
  for(FormulaList::const_iterator i=_size.begin(); i!=_size.end(); ++i,++d){
    FreeVarsPtr fv = (*i)->getFreeVariables();
    if(fv->size()==1){
      std::string var = *fv->begin();
      FormulaPtr tmp = FormulaVariable::mktmp();
      if(!defined.contains(var)){
        defined.insert(var);
        FormulaList l;
        l.push_back(new FormulaEQ(tmp, *i));
        l = *MaximaWrapper::instance().solve(l, var);
        JASSERT(l.size()==1)(*i)(var).Text("Failed to solve");
        o.varDecl("const IndexT "+var+"="+(*l.begin())->rhs()->replace(tmp, new FormulaVariable(_name+".size("+jalib::XToString(d)+")"))->toString());
      }
    }
  }
}
void petabricks::MatrixDef::verifyDefines(CodeGenerator& o){
  int d=0;
  for(FormulaList::const_iterator i=_size.begin(); i!=_size.end(); ++i,++d){
    o.addAssert((*i)->toString(), _name+".size("+jalib::XToString(d)+")");
  }
}
void petabricks::MatrixDef::allocateTemporary(CodeGenerator& o, bool setOnly){
  if(setOnly)
    o.varDecl(name()+" = "+matrixTypeName()+"::allocate("+_size.toString()+")");
  else
    o.varDecl(matrixTypeName()+" "+name()+" = "+matrixTypeName()+"::allocate("+_size.toString()+")");
}

void petabricks::MatrixDef::readFromFileCode(CodeGenerator& o, const std::string& fn){
  o.varDecl(name()
      +" = petabricks::MatrixIO("+fn+",\"r\").read<"+jalib::XToString(numDimensions())+">()");
}
void petabricks::MatrixDef::writeToFileCode(CodeGenerator& o, const std::string& fn){
  o.write("petabricks::MatrixIO("+fn+",\"w\").write("+name()+");");
}
void petabricks::MatrixDef::varDeclCodeRO(CodeGenerator& o){
  o.write(constMatrixTypeName()+" "+name()+";");
}
void petabricks::MatrixDef::varDeclCodeRW(CodeGenerator& o){
  o.write(matrixTypeName()+" "+name()+";");
}
