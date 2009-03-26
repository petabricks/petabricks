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

