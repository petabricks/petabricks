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
#include "codegenerator.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

std::stringstream& petabricks::CodeGenerator::theFilePrefix() { 
  static std::stringstream t; 
  return t; 
}

petabricks::TunableDefs& petabricks::CodeGenerator::theTunableDefs() {
  static TunableDefs t;
  return t;
}

petabricks::CodeGenerator::CodeGenerator() : _contCounter(0), _indent(0) {}

void petabricks::CodeGenerator::beginFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step){
  indent();
  os() << "for(int " << var << "=" << begin << "; "<< var << "<" << end << "; " << var << "+="<< step <<" ){\n";
  _indent++;
}

void petabricks::CodeGenerator::beginReverseFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step){
  indent();
  os() << "for(int " << var << "=" << end->minusOne() << "; "<< var << ">=" << begin << "; " << var << "-="<< step <<" ){\n";
  _indent++;
}

void petabricks::CodeGenerator::endFor(){
  _indent--;
    indent();
  os() << "}\n";
}

void petabricks::CodeGenerator::call(const std::string& func, const std::string& args){
  indent();
  os() << func << '(' << args << ");\n";
}

void petabricks::CodeGenerator::call(const std::string& func, const std::vector<std::string>& args){
  indent();
  os() << func << '(';
  jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
  os() << ");\n";
}

void petabricks::CodeGenerator::setcall(const std::string& lv, const std::string& func, const std::vector<std::string>& args){
  indent();
  os() << lv << " = " << func << '(';
  jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
  os() << ");\n";
}

// void petabricks::CodeGenerator::beginFunc(const std::string& rt, const std::string& func, const std::string& args){
//   indent();
//   os() << rt << " " << func << '(' << args << "){\n";
//   _indent++;
// }

// void petabricks::CodeGenerator::declareFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args){
//   indent();
//   os() << rt << " " << func << '(';
//   jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
//   os() << ");\n";
// }

void petabricks::CodeGenerator::beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args){
  indent();
  os() << rt << " ";
  if(inClass()) os() << _curClass << "::";
  os() << func << '(';
  jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
  os() << "){\n";
  _indent++;

  if(inClass()) hos() << "  ";
  hos() << rt << " " << func << '(';
  jalib::JPrintable::printStlList(hos(), args.begin(), args.end(), ", ");
  hos() << ");\n";
}

void petabricks::CodeGenerator::varDecl(const std::string& var){
  indent();
  os() << var << ";\n";
} 

void petabricks::CodeGenerator::addAssert(const std::string& l, const std::string& r){
  indent();
  os() << "JASSERT("<<l<<" == "<<r<<")"
    << "(" << l << ")"
    << "(" << r << ")"
    << ";\n";
} 

void petabricks::CodeGenerator::endFunc(){
  _indent--;
    indent();
  os() << "}\n";
}

void petabricks::CodeGenerator::indent(){ 
  if(_indent>0)
    os() << std::string(_indent*2,' '); 
}

void petabricks::CodeGenerator::write(const std::string& str){
  indent();
  os() << str << "\n";
}

void petabricks::CodeGenerator::newline(){
  os() << "\n";
}

void petabricks::CodeGenerator::comment(const std::string& str){
  indent();
  os() << "// " << str << "\n";
}

void petabricks::CodeGenerator::beginIf(const std::string& v){
  indent();
  os() << "if(" << v << "){\n";
  _indent++;
}

void petabricks::CodeGenerator::elseIf(const std::string& v /*= "true"*/){
  _indent--;
  indent();
  if(v=="true")
    os() << "}else{\n";
  else
    os() << "}else if(" << v << "){\n";
  _indent++;
}

void petabricks::CodeGenerator::endIf(){
  _indent--;
    indent();
  os() << "}\n";
}

petabricks::TaskCodeGenerator& petabricks::BufferedCodeGenerator::createTask(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string&){
  UNIMPLEMENTED();
  return *(TaskCodeGenerator*)0;
}

petabricks::TaskCodeGenerator& petabricks::MainCodeGenerator::createTask(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string& postfix){
  _tasks.push_back(new TaskCodeGenerator(func, args, taskType, postfix));
  return *_tasks.back();
}

namespace{//file local
  void _splitTypeArgs(std::string& type, std::string& name, const std::string& str){
    const char* begin=str.c_str();
    const char* mid=begin+str.length();
    const char* end=mid;
    while(mid>begin && *(--mid)!=' ');
//     JTRACE("SPLIT")(begin)(mid+1);
    JASSERT(mid!=begin);
    type.assign(begin,mid);
    name.assign(mid+1,end);
  }
}

petabricks::TaskCodeGenerator::TaskCodeGenerator(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string& postfix){
  _name=func + postfix;
  _indent=1;
  _types.resize(args.size());
  _names.resize(args.size());
  for(size_t i=0; i!=args.size(); ++i)
    _splitTypeArgs(_types[i], _names[i], args[i]);

  os() << "class " << _name << " : public petabricks::"<< taskType <<" {\n";
  for(size_t i=0; i!=args.size(); ++i){
    indent();
    os() << args[i] << ";\n";
  }
  os() << "public:\n"; 
  indent();
  os() << _name << "(";
  for(size_t i=0; i!=args.size(); ++i){
    if(i>0) os()<<", ";
    os() << _types[i] << "& a_" << _names[i];
  }
  os() << ") \n    : ";
  for(size_t i=0; i!=args.size(); ++i){
    if(i>0) os()<<", ";
    os() << _names[i] << "(a_" << _names[i] << ")";
  }
  os() << "\n  {}\n\n";
}


static std::string _typeToConstRef(std::string s){
  if(s[s.length()-1] != '&'){
    s+='&';
    if(   s[0]=='c'
       && s[1]=='o'
       && s[2]=='n'
       && s[3]=='s'
       && s[4]=='t'
       && s[5]==' '){
      return s;
    }
    s="const "+s;
  }
  return s;
}

void petabricks::CodeGenerator::beginClass(const std::string& name, const std::string& base){
  hos() << "class " << name << " : public " << base << " {\n";
  hos() << ("  typedef "+base+" BASE;\npublic:\n");
  _curClass=name;
  _contCounter=0;
  JASSERT(_curMembers.empty())(_curMembers.size());
}
void petabricks::CodeGenerator::endClass(){
  const char* delim="";
  indent();
  os() << _curClass << "::" << _curClass << "(";
  hos() << _curClass << "(";
  for(ClassMembers::const_iterator i=_curMembers.begin(); i!=_curMembers.end(); ++i){
    if(i->initializer == ClassMember::PASSED()){
      os() << delim << _typeToConstRef(i->type) <<" t_"<<i->name;
      hos() << delim << _typeToConstRef(i->type) <<" t_"<<i->name;
      delim=", ";
    }
  }
  os() << ")\n";
  hos() << ");\n";
  indent();
  os() << "  : BASE()";
  for(ClassMembers::const_iterator i=_curMembers.begin(); i!=_curMembers.end(); ++i){
    if(i->initializer == ClassMember::PASSED())
      os() << ", " << i->name<<"(t_"<<i->name<<")";
    else if(i->initializer.size()>0)
      os() << ", " << i->name<<"("<<i->initializer<<")";
  }
  newline();
  write("{"+_curConstructorBody+"}");
  _curConstructorBody="";
  hos() << "//private:\n";
  for(ClassMembers::const_iterator i=_curMembers.begin(); i!=_curMembers.end(); ++i){
    hos() << "  " << i->type << " " << i->name <<";\n";
  }
  _curMembers.clear();
  _curClass="";
  hos() << "};\n\n";
  newline();
  newline();
}
void petabricks::CodeGenerator::addMember(const std::string& type, const std::string& name, const std::string& initializer){
  if(_curClass.size()>0){
    ClassMember tmp;
    tmp.type=type;
    tmp.name=name;
    tmp.initializer=initializer;
    _curMembers.push_back(tmp);
  }else{
    if(initializer.size()>0)
      varDecl(type+" "+name+" = "+initializer);
    else
      varDecl(type+" "+name);
  }
}

std::string petabricks::CodeGenerator::nextContName(const std::string& base){
  return base+"cont_" + jalib::XToString(_contCounter++);
}

void petabricks::CodeGenerator::continuationPoint(){
#ifndef DISABLE_CONTINUATIONS
  std::string n = "cont_" + jalib::XToString(_contCounter++);
  beginIf("useContinuation()");
  write("return new petabricks::MethodCallTask<"+_curClass+", &"+_curClass+"::"+n+">( this );"); 
  elseIf();
  write("return "+n+"();"); 
  endIf();
  endFunc();
  beginFunc("DynamicTaskPtr", n);
#endif
}

void petabricks::CodeGenerator::continuationRequired(const std::string& hookname){
  std::string n = "cont_" + jalib::XToString(_contCounter++);
  newline();
  write("return "+hookname+" new petabricks::MethodCallTask<"+_curClass+", &"+_curClass+"::"+n+">(this));"); 
  endFunc();
  beginFunc("DynamicTaskPtr", n);
}


