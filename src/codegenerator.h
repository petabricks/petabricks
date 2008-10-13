/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#ifndef HECURACODEGENERATOR_H
#define HECURACODEGENERATOR_H

#include "formula.h"
#include "jprintable.h"

#include <iostream>
#include <string>
#include <vector>


namespace hecura {
class CodeGenerator{
public:
  CodeGenerator(std::ostream& o) : _indent(0), _os(o) {}

  const char* varName(int d) const {
    const char* const table[] = {"x", "y", "z", "d4", "d5", "d6", "d7", "d8", "d9"};
    JASSERT(d>=0 && d<9)(d);
    return table[d];
  }

  void beginFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end){
    indent();
    _os << "for(int " << var << "=" << begin << "; "<< var << "<" << end << "; ++" << var << "){\n";
    _indent++;
  }

  void endFor(){
    _indent--;
      indent();
    _os << "}\n";
  }

  void call(const std::string& func, const std::string& args){
    indent();
    _os << func << '(' << args << ");\n";
  }

  void call(const std::string& func, const std::vector<std::string>& args){
    indent();
    _os << func << '(';
    jalib::JPrintable::printStlList(_os, args.begin(), args.end(), ", ");
    _os << ");\n";
  }

  void setcall(const std::string& lv, const std::string& func, const std::vector<std::string>& args){
    indent();
    _os << lv << " = " << func << '(';
    jalib::JPrintable::printStlList(_os, args.begin(), args.end(), ", ");
    _os << ");\n";
  }

  void beginFunc(const std::string& rt, const std::string& func, const std::string& args){
    indent();
    _os << rt << " " << func << '(' << args << "){\n";
    _indent++;
  }

  void beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args){
    indent();
    _os << rt << " " << func << '(';
    jalib::JPrintable::printStlList(_os, args.begin(), args.end(), ", ");
    _os << "){\n";
    _indent++;
  }

  void varDecl(const std::string& var){
    indent();
    _os << var << ";\n";
  } 

  void addAssert(const std::string& l, const std::string& r){
    indent();
    _os << "JASSERT("<<l<<" == "<<r<<")"
      << "(" << l << ")"
      << "(" << r << ")"
      << ";\n";
  } 

  void endFunc(){
    _indent--;
      indent();
    _os << "}\n";
  }

  void indent(){ 
    _os << std::string(_indent*2,' '); 
  }

  void write(const std::string& str){
    indent();
    _os << str << "\n";
  }

  void newline(){
    _os << "\n";
  }

  void comment(const std::string& str){
    indent();
    _os << "// " << str << "\n";
  }

  void beginIf(const std::string& v){
    indent();
    _os << "if(" << v << "){\n";
    _indent++;
  }

  void elseIf(const std::string& v = "true"){
    _indent--;
    indent();
    if(v=="true")
      _os << "}else{\n";
    else
      _os << "}else if(" << v << "){\n";
    _indent++;
  }

  void endIf(){
    _indent--;
      indent();
    _os << "}\n";
  }
private:
  int _indent;
  std::ostream& _os;
};

}

#endif
