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
#include "matrixdef.h"

#include "codegenerator.h"
#include "formula.h"
#include "maximawrapper.h"
#include "transform.h"

// A hack for now, for inference we assume matrix is big
inline static petabricks::FormulaPtr LARGE(){
  return new petabricks::FormulaInteger(1000);
}

petabricks::MatrixDef::MatrixDef(const char* name, const FormulaList& version, const FormulaList& size)
  : _name(name), _version(version), _size(size), _type(T_UNKNOWN)
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

void petabricks::MatrixDef::initialize(Transform& ){
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
  FreeVars::const_iterator i;
  for(i=tmp->begin(); i!=tmp->end(); ++i)
    trans.addConstant(*i, FreeVar::FLAG_SIZEVAR);
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
        o.addMember("IndexT", var, "");
        o.write(var + " = " 
                + (*l.begin())->rhs()->replace(tmp, new FormulaVariable(_name+".size("+jalib::XToString(d)+")"))->toString()
                + ";");
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
  if(!setOnly)
    o.addMember(matrixTypeName(), name(), "");
  o.varDecl(name()+" = "+matrixTypeName()+"::allocate("+_size.toString()+")");
}

void petabricks::MatrixDef::readFromFileCode(CodeGenerator& o, const std::string& fn){
  o.varDecl(name()
      +" = petabricks::MatrixIO("+fn+",\"r\").read<"+jalib::XToString(numDimensions())+">()");
}
void petabricks::MatrixDef::writeToFileCode(CodeGenerator& o, const std::string& fn){
  o.write("petabricks::MatrixIO("+fn+",\"w\").write("+name()+");");
}
void petabricks::MatrixDef::varDeclCodeRO(CodeGenerator& o){
  o.addMember(constMatrixTypeName(), name(), "");
}
void petabricks::MatrixDef::varDeclCodeRW(CodeGenerator& o){
  o.addMember(matrixTypeName(), name(), "");
}
