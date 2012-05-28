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
#include "matrixdef.h"

#include "codegenerator.h"
#include "clcodegenerator.h"
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
  SRCPOSSCOPE();
//   _version.normalize();
//   _size.normalize();
  if(_version.size()>0){
    //TODO support for min region size
    JASSERT(_version.size()<=2);
    if(_version.size()==1){ 
      _version.insert(_version.begin(), FormulaInteger::zero());
    }
    JASSERT(_version[0]->getFreeVariables()->size()==0)(_version)
      .Text("Non constant minimum version size not yet supported");
    _size.push_back(_version.back()->plusOne());
  }
}

void petabricks::MatrixDef::exportConstants(Transform& trans){
  SRCPOSSCOPE();
  FreeVarsPtr tmp = _size.getFreeVariables();
  FreeVars::const_iterator i;
  for(i=tmp->begin(); i!=tmp->end(); ++i)
    trans.addSizeVar(*i);
}

void petabricks::MatrixDef::exportAssumptions(){
  SRCPOSSCOPE();
  for(FormulaList::const_iterator i=_size.begin(); i!=_size.end(); ++i){
    MaximaWrapper::instance().assume(new FormulaGT(*i, LARGE()));
  }
}

void petabricks::MatrixDef::argDecl(std::vector<std::string>& args, RuleFlavor rf, bool isConst, bool byRef) const {
  if(byRef)
    args.push_back("const "+typeName(rf, isConst)+"& " + _name);
  else
    args.push_back("const "+typeName(rf, isConst)+" " + _name);
}
void petabricks::MatrixDef::genAllocTmpCode(CodeGenerator& o, RuleFlavor rf){
  o.varDecl(typeName(rf)+" "+_name);
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

#ifdef HAVE_OPENCL
//TODO: don't need this
void petabricks::MatrixDef::extractCLDefines(FreeVars& defined, CLCodeGenerator& clo, unsigned int dims, std::map<std::string, std::string> &map){
  unsigned int d=0;
  for(FormulaList::const_iterator i=_size.begin(); i!=_size.end(); ++i,++d){
    FreeVarsPtr fv = (*i)->getFreeVariables();
    if(fv->size()==1 && d < dims){
      std::string var = *fv->begin();
      FormulaPtr tmp = FormulaVariable::mktmp();
      if(!defined.contains(var)){
        defined.insert(var);
        FormulaList l;
        l.push_back(new FormulaEQ(tmp, *i));
        l = *MaximaWrapper::instance().solve(l, var);
        JASSERT(l.size()==1)(*i)(var).Text("Failed to solve");
        clo.os() << "unsigned int " << var << " = " 
               << (*l.begin())->rhs()->replace(tmp, new FormulaVariable("dim_"+map[_name]+"_d"+jalib::XToString(d)))->toString() << ";\n";
      }
    }
  }
}
#endif

void petabricks::MatrixDef::verifyDefines(CodeGenerator& o){
  int d=0;
  for(FormulaList::const_iterator i=_size.begin(); i!=_size.end(); ++i,++d){
    o.addAssert((*i)->toString(), _name+".size("+jalib::XToString(d)+")");
  }
}
void petabricks::MatrixDef::allocateTemporary(CodeGenerator& o, RuleFlavor rf, bool setOnly, bool reallocAllowed){
  if(!setOnly)
    o.addMember(typeName(rf), name(), "");

  if(reallocAllowed)
    o.beginIf("!"+name()+".isSize("+_size.toString()+")");

  o.write(name()+" = "+allocateStr(rf)+";");

  if(reallocAllowed)
    o.endIf();
}

std::string petabricks::MatrixDef::allocateStr(RuleFlavor rf) const{
  return typeName(rf)+"::allocate("+_size.toString()+")";
}
std::string petabricks::MatrixDef::genericAllocateStr() const{
  return genericTypeName()+"::allocate("+_size.toString()+")";
}

void petabricks::MatrixDef::readFromFileCode(CodeGenerator& o, const std::string& fn, RuleFlavor rf){
  o.varDecl(name()+" = petabricks::MatrixIO("+fn+",\"r\").read_"+rf.str()+"<"+jalib::XToString(numDimensions())+">()");
}
void petabricks::MatrixDef::writeToFileCode(CodeGenerator& o, const std::string& fn){
  o.write("petabricks::MatrixIO("+fn+",\"w\").write("+name()+");");
}
void petabricks::MatrixDef::varDeclCode(CodeGenerator& o, RuleFlavor rf, bool isConst){
  o.addMember(typeName(rf, isConst), name(), "");
}
