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
#ifndef PETABRICKSCODEGENERATOR_H
#define PETABRICKSCODEGENERATOR_H

#include "formula.h"
#include "jprintable.h"
#include "jrefcounted.h"
#include "jconvert.h"
#include "trainingdeps.h"

#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <list>
#include <map>
#include <algorithm>
#include <limits>

namespace petabricks {

typedef std::map<std::string, std::string> TunableDefs;

class TaskCodeGenerator;

class CodeGenerator {
public:
  struct ClassMember {
    std::string type;
    std::string name;
    std::string initializer;
    static const char* PASSED() { return "PASSED"; }
  };
  typedef std::vector<ClassMember> ClassMembers;

  
  static std::stringstream& theFilePrefix();
  static TunableDefs& theTunableDefs();

  void incIndent(){++_indent;}
  void decIndent(){--_indent;}
  
  CodeGenerator();
  virtual ~CodeGenerator(){}

  void beginFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void beginReverseFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void endFor();

  void call(const std::string& func, const std::string& args);
  void call(const std::string& func, const std::vector<std::string>& args);
  void setcall(const std::string& lv, const std::string& func, const std::vector<std::string>& args);

  virtual void beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args = std::vector<std::string>());
  void endFunc();

  void varDecl(const std::string& var);
  void addAssert(const std::string& l, const std::string& r);

  void write(const std::string& str);
  void newline();

  void comment(const std::string& str);

  void beginIf(const std::string& v);
  void elseIf(const std::string& v = "true");
  void endIf();

  virtual TaskCodeGenerator& createTask(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string& postfix) = 0;

  void createTunable( bool isTunable
                    , const std::string& category
                    , const std::string& name
                    , int initial
                    , int min=0
                    , int max=std::numeric_limits<int>::max())
  {
    JTRACE("new tunable")(name)(initial)(min)(max);
    theTunableDefs()[name] =
       "JTUNABLE("+name
              +","+jalib::XToString(initial)
              +","+jalib::XToString(min)
              +","+jalib::XToString(max)+")";
    _cg.addTunable( isTunable, category, name, initial, min, max);
  }

  void beginSwitch(const std::string& var){
    write("switch("+var+"){");
  }
  void endSwitch(){
    write("}");
  }
  void beginCase(int n){
    write("case "+jalib::XToString(n)+":");
    _indent++;
  }
  void endCase(){
    write("break;");
    _indent--;
  }

  TrainingDeps& cg() { return _cg; }


  void beginClass(const std::string& name, const std::string& base);
  void endClass();
  void addMember(const std::string& type, const std::string& name, const std::string& initializer = ClassMember::PASSED());
  void continuationPoint();
  void continuationRequired(const std::string& prereq);

  void define(const std::string& name, const std::string& val){
    _defines.push_back(name);
    write("#define "+name+" "+val);
  }
  void undefineAll(){
    for(size_t i=0; i<_defines.size(); ++i)
      write("#undef "+_defines[i]);
    _defines.clear();
  }
  bool inClass() const { return _curClass.size()>0; }


  void staticMember() { hos() << "static "; }
protected:
  void indent();
  virtual std::ostream& os() = 0;
  virtual std::ostream& hos() = 0;
protected:
  std::vector<std::string> _defines;
  ClassMembers _curMembers;
  std::string _curClass;
  int         _contCounter;
  int _indent;
  TrainingDeps _cg;
};

class BufferedCodeGenerator : public CodeGenerator {
public:
  virtual std::string str() const { return _os.str() ; }
  TaskCodeGenerator& createTask(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string& postfix);
protected:
  std::ostream& hos() { return _header; }
  std::ostream& os() { return _os; }
protected:
  std::ostringstream _os;
  std::ostringstream _header;
};



class TaskCodeGenerator : public BufferedCodeGenerator, public jalib::JRefCounted {
public:
  std::string str() const { return _os.str() + "};\n"; }
  TaskCodeGenerator& createTask(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string& postfix){ return *this;}

  TaskCodeGenerator(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string& postfix);

  void beginRunFunc(){
    beginFunc("petabricks::DynamicTaskPtr", "run", std::vector<std::string>());
  }

  void beginSizeFunc(){
    write("size_t size() const {");
    ++_indent;
  }

  void beginCanSplitFunc(){
    write("bool canSplit() const {");
    ++_indent;
  }

  void beginSplitFunc(){
    write("petabricks::DynamicTaskPtr split(){");
    ++_indent;
  }

  void beginBeginPosFunc(){
    write("IndexT beginPos(int d) const {");
    ++_indent;
  }


  void beginEndPosFunc(){
    write("IndexT endPos(int d) const {");
    ++_indent;
  }

  void beginSpatialSplitFunc(){
    write("void spatialSplit(SpatialTaskList& _list, int _dim, int _thresh) {");
    ++_indent;
  }

  void beginDimensionsFunc(){
    write("int dimensions() const {");
    ++_indent;
  }

  const std::vector<std::string>& argnames() const { return _names; }
  const std::vector<std::string>& argtypes() const { return _types; }
  const std::string& name() const { return _name; }
private:
  std::string _name;
  std::vector<std::string> _types;
  std::vector<std::string> _names;
};

class MainCodeGenerator : public BufferedCodeGenerator {
public:
  void outputFileTo(std::ostream& o){
    o << theFilePrefix().str();
    o << "\n// Tunable declarations\n";
    for(TunableDefs::const_iterator i=theTunableDefs().begin(); i!=theTunableDefs().end(); ++i)
      o << i->second << ";\n";
    o << "\n// Forward declarations\n";
    o << _header.str();
    o << "\n// Task declarations\n";
    for(TaskDecls::iterator i=_tasks.begin(); i!=_tasks.end(); ++i)
      o << (*i)->str();
    o << "\n\n";
    o << _os.str();
  }

  TaskCodeGenerator& createTask(const std::string& func, const std::vector<std::string>& args, const char* taskType, const std::string& postfix);

//void merge(const MainCodeGenerator& that){
//  _os << that._os.str();
//  _forwardDecls << that._forwardDecls.str();
//  _tasks.insert(_tasks.begin(), that._tasks.begin(), that._tasks.end());
//}

private:
  typedef std::list<jalib::JRef<TaskCodeGenerator> > TaskDecls;
  TaskDecls _tasks;
};

}

#endif
