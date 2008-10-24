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

std::stringstream& hecura::CodeGenerator::theFilePrefix() { 
  static std::stringstream t; 
  return t; 
}

hecura::CodeGenerator::CodeGenerator() : _indent(0) {}

void hecura::CodeGenerator::beginFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step){
  indent();
  os() << "for(int " << var << "=" << begin << "; "<< var << "<" << end << "; " << var << "+="<< step <<" ){\n";
  _indent++;
}

void hecura::CodeGenerator::beginReverseFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step){
  indent();
  os() << "for(int " << var << "=" << end->minusOne() << "; "<< var << ">=" << begin << "; " << var << "-="<< step <<" ){\n";
  _indent++;
}

void hecura::CodeGenerator::endFor(){
  _indent--;
    indent();
  os() << "}\n";
}

void hecura::CodeGenerator::call(const std::string& func, const std::string& args){
  indent();
  os() << func << '(' << args << ");\n";
}

void hecura::CodeGenerator::call(const std::string& func, const std::vector<std::string>& args){
  indent();
  os() << func << '(';
  jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
  os() << ");\n";
}

void hecura::CodeGenerator::setcall(const std::string& lv, const std::string& func, const std::vector<std::string>& args){
  indent();
  os() << lv << " = " << func << '(';
  jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
  os() << ");\n";
}

// void hecura::CodeGenerator::beginFunc(const std::string& rt, const std::string& func, const std::string& args){
//   indent();
//   os() << rt << " " << func << '(' << args << "){\n";
//   _indent++;
// }

// void hecura::CodeGenerator::declareFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args){
//   indent();
//   os() << rt << " " << func << '(';
//   jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
//   os() << ");\n";
// }

void hecura::MainCodeGenerator::beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args){
  _forwardDecls << rt << " " << func << '(';
  jalib::JPrintable::printStlList(_forwardDecls, args.begin(), args.end(), ", ");
  _forwardDecls << ");\n";
  hecura::CodeGenerator::beginFunc(rt, func, args);
}

void hecura::CodeGenerator::beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args){
  indent();
  os() << rt << " " << func << '(';
  jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
  os() << "){\n";
  _indent++;
}

void hecura::CodeGenerator::varDecl(const std::string& var){
  indent();
  os() << var << ";\n";
} 

void hecura::CodeGenerator::addAssert(const std::string& l, const std::string& r){
  indent();
  os() << "JASSERT("<<l<<" == "<<r<<")"
    << "(" << l << ")"
    << "(" << r << ")"
    << ";\n";
} 

void hecura::CodeGenerator::endFunc(){
  _indent--;
    indent();
  os() << "}\n";
}

void hecura::CodeGenerator::indent(){ 
  os() << std::string(_indent*2,' '); 
}

void hecura::CodeGenerator::write(const std::string& str){
  indent();
  os() << str << "\n";
}

void hecura::CodeGenerator::newline(){
  os() << "\n";
}

void hecura::CodeGenerator::comment(const std::string& str){
  indent();
  os() << "// " << str << "\n";
}

void hecura::CodeGenerator::beginIf(const std::string& v){
  indent();
  os() << "if(" << v << "){\n";
  _indent++;
}

void hecura::CodeGenerator::elseIf(const std::string& v /*= "true"*/){
  _indent--;
  indent();
  if(v=="true")
    os() << "}else{\n";
  else
    os() << "}else if(" << v << "){\n";
  _indent++;
}

void hecura::CodeGenerator::endIf(){
  _indent--;
    indent();
  os() << "}\n";
}

hecura::TaskCodeGenerator& hecura::MainCodeGenerator::createTask(const std::string& func, const std::vector<std::string>& args){
  _tasks.push_back(new TaskCodeGenerator());
  return *_tasks.back();
}
