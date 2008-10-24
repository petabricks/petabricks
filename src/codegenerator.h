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
#include <sstream>
#include <vector>
#include <algorithm>


namespace hecura {
class CodeGenerator {
public:
  static std::stringstream& theFilePrefix();

  CodeGenerator();

  void beginFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void beginReverseFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void endFor();

  void call(const std::string& func, const std::string& args);
  void call(const std::string& func, const std::vector<std::string>& args);
  void setcall(const std::string& lv, const std::string& func, const std::vector<std::string>& args);

//   void declareFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args);
//   void beginFunc(const std::string& rt, const std::string& func, const std::string& args);
  void beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args);
  void endFunc();

  void varDecl(const std::string& var);
  void addAssert(const std::string& l, const std::string& r);

  void write(const std::string& str);
  void newline();

  void comment(const std::string& str);

  void beginIf(const std::string& v);
  void elseIf(const std::string& v = "true");
  void endIf();


  void outputFileTo(std::ostream& os){
    os << theFilePrefix().str();
    os << "\n// Forward declarations\n";
    os << _forwardDecls.str();
    os << "\n\n";
    os << _os.str();
  }
protected:
  void indent();

private:
  int _indent;
  std::ostringstream _forwardDecls;
  std::ostringstream _os;
};

}

#endif
