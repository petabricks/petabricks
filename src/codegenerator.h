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
#include "jrefcounted.h"


#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <list>
#include <algorithm>


namespace hecura {

class TaskCodeGenerator;

class CodeGenerator {
public:
  static std::stringstream& theFilePrefix();

  CodeGenerator();
  virtual ~CodeGenerator(){}

  void beginFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void beginReverseFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void endFor();

  void call(const std::string& func, const std::string& args);
  void call(const std::string& func, const std::vector<std::string>& args);
  void setcall(const std::string& lv, const std::string& func, const std::vector<std::string>& args);

  virtual void beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args);
  void endFunc();

  void varDecl(const std::string& var);
  void addAssert(const std::string& l, const std::string& r);

  void write(const std::string& str);
  void newline();

  void comment(const std::string& str);

  void beginIf(const std::string& v);
  void elseIf(const std::string& v = "true");
  void endIf();

  virtual TaskCodeGenerator& createTask(const std::string& func, const std::vector<std::string>& args) = 0;
protected:
  void indent();
  virtual std::ostream& os() = 0;
private:
  int _indent;

};

class TaskCodeGenerator : public CodeGenerator, public jalib::JRefCounted {
public:
  std::string str() const { return _os.str(); }

  TaskCodeGenerator& createTask(const std::string& func, const std::vector<std::string>& args){ return *this;}
protected:
  std::ostream& os() { return _os; }
private:
  std::ostringstream _os;
};

class MainCodeGenerator : public CodeGenerator {
public:
  void beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args);

  void outputFileTo(std::ostream& o){
    o << theFilePrefix().str();
    o << "\n// Forward declarations\n";
    o << _forwardDecls.str();
    o << "\n// Task declarations\n";
    for(TaskDecls::iterator i=_tasks.begin(); i!=_tasks.end(); ++i)
      o << (*i)->str();
    o << "\n\n";
    o << _os.str();
  }

  TaskCodeGenerator& createTask(const std::string& func, const std::vector<std::string>& args);
protected:
  std::ostream& os() { return _os; }
private:
  std::ostringstream _forwardDecls;
  std::ostringstream _os;
  typedef std::list<jalib::JRef<TaskCodeGenerator> > TaskDecls;
  TaskDecls _tasks;
};

}

#endif
