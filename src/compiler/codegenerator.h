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
#ifndef PETABRICKSCODEGENERATOR_H
#define PETABRICKSCODEGENERATOR_H

#include "formula.h"
#include "pbc.h"
#include "trainingdeps.h"

#include "common/jconvert.h"
#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/jtunable.h"

#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <sstream>
#include <string>
#include <vector>

namespace petabricks {

class StreamTree;
typedef jalib::JRef<StreamTree>       StreamTreePtr;
typedef std::vector<StreamTreePtr>    StreamTrees;

class StreamTree : public jalib::JRefCounted {
public:
  StreamTree(const std::string& n): _name(n) {}

  StreamTreePtr add(const StreamTreePtr& t){
    _nodes.push_back(t);
    //t->_parent = this;
    return t;
  }

  void writeTo(std::ostream& o) {
    o << "// :::: " << _name << "\n";
    StreamTrees::iterator i;
    for(i=_nodes.begin(); i!=_nodes.end(); ++i)
      (*i)->writeTo(o);
    o << _os.str();
  }

  template<typename T>
  StreamTree& operator<<(const T& t) {
    _os << t;
    return *this;
  }

  operator std::ostream& () { return _os; }

  std::string str() {
    std::ostringstream tmp;
    writeTo(tmp);
    return tmp.str();
  }

private:
  std::string  _name;
  StreamTrees  _nodes;
  std::ostringstream _os;
};

typedef std::map<std::string, std::string> TunableDefs;
class SimpleRegion;
class CodeGenerator;
typedef jalib::JRef<CodeGenerator>    CodeGeneratorPtr;
typedef std::vector<CodeGeneratorPtr> CodeGenerators;


class CodeGenerator : public jalib::JRefCounted {
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
  static jalib::TunableValueMap& theHardcodedTunables();

  void incIndent(){++_indent;}
  void decIndent(){--_indent;}

  CodeGenerator(const StreamTreePtr& root, const TrainingDepsPtr& cg);
  CodeGenerator(CodeGenerator& that);
  virtual ~CodeGenerator(){}

  void beginFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void beginReverseFor(const std::string& var, const FormulaPtr& begin, const FormulaPtr& end,  const FormulaPtr& step);
  void endFor();

  void call(const std::string& func, const std::string& args);
  void call(const std::string& func, const std::vector<std::string>& args);
  void setcall(const std::string& lv, const std::string& func, const std::vector<std::string>& args);

  virtual void beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args = std::vector<std::string>(), bool is_static = false);
  void endFunc();

  void varDecl(const std::string& var);
  void addAssert(const std::string& l, const std::string& r);

  void write(const std::string& str);
  void newline();

  void comment(const std::string& str);

  void beginIf(const std::string& v);
  void beginIfNot(const std::string& v){
    beginIf(" ! ("+v+") ");
  }
  void elseIf(const std::string& v = "true");
  void endIf();

  void createTunable( bool isTunable
                    , const std::string& category
                    , const std::string& name
                    , jalib::TunableValue initial
                    , jalib::TunableValue min=0
                    , jalib::TunableValue max=jalib::maxval<int>()
                    , const char* type = "")
  {
    std::ostringstream o;
    jalib::TunableValueMap::const_iterator i = theHardcodedTunables().find(name);
    if(i==theHardcodedTunables().end()){
      o << "JTUNABLE" << type << " (" << name<< ","  << initial << ","  << min << ","  << max << ");\n";
    }else{
      o << "JTUNABLE" << type << "STATIC(" << name<< ","<< i->second << ");\n";
    }
    theTunableDefs()[name] = o.str();
    _cg->addTunable(isTunable, category, name, initial, min, max);
  }
  void createTunableArray( const std::string& category
                         , const std::string& name
                         , jalib::TunableValue count
                         , jalib::TunableValue initial
                         , jalib::TunableValue min
                         , jalib::TunableValue max
                         , bool isTunable
                         , const std::string& type = "")
  {
    //JTRACE("new tunable")(name)(initial)(min)(max);
    theTunableDefs()[name] =
       "JTUNABLE"+ type +"ARRAY("+name
              +","+jalib::XToString(count)
              +","+jalib::XToString(initial)
              +","+jalib::XToString(min)
              +","+jalib::XToString(max)+");";
    _cg->addTunable(isTunable, category, name, initial, min, max);
  }

  void beginSwitch(const std::string& var){
    write("switch("+var+"){");
  }
  void endSwitch(){
    write("}");
  }
  void beginCase(int n){
    _indent++;
    write("case "+jalib::XToString(n)+": {");
    _indent++;
  }
  void endCase(){
    _indent--;
    write("}");
    write("break;");
    _indent--;
  }

  TrainingDeps& cg() { return *_cg; }
  TrainingDepsPtr cgPtr() { return _cg; }

  void beginClass(const std::string& name, const std::string& base);
  void endClass();
  void addMember(const std::string& type, const std::string& name, const std::string& initializer = ClassMember::PASSED());
  void continuationPoint();
  void continuationRequired(const std::string& prereq);
  std::string nextContName(const std::string& base);

  void continueLabel(const std::string& fn){
    continueJump(fn);
    endFunc();
    beginFunc("petabricks::DynamicTaskPtr", fn);
    if(_rf != RuleFlavor::INVALID)
      beginUserCode(_rf);
  }
  void continueJump(const std::string& fn){
    write("return "+fn+"();");
  }

  void define(const std::string& name, const std::string& val){
    _defines.push_back(name);
    _define(name,val);
  }

  void _define(const std::string& name, const std::string& val){
    write("#define "+name+" "+val);
  }
  void _undefine(const std::string& name){
    write("#undef "+name);
  }

  void undefineAll(){
    for(size_t i=0; i<_defines.size(); ++i)
      write("#undef "+_defines[i]);
    _defines.clear();
  }
  bool inClass() const { return _curClass.size()>0; }

  void beginUserCode(RuleFlavor rf) {
    _rf = rf;
    write("using namespace petabricks::"+rf.string()+";");
  }
  void endUserCode(){
    _rf = RuleFlavor::INVALID;
  }

  void constructorBody(const std::string& s){_curConstructorBody=s;}

  void staticMember() { hos() << "static "; }

  void withEachMember(const std::string& type, const std::string& code){
    for(size_t i=0; i<_curMembers.size(); ++i)
      if(_curMembers[i].type == type)
        write(_curMembers[i].name+code+";");
  }

  void globalDefine(const std::string& n, const std::string& v){
    dos() << "#define " << n << " " << v << "\n";
  }

  ///
  /// Create a parallel code generator
  /// (for writing other function bodies before current function is finished)
  CodeGenerator& forkhelper();

  ///
  /// Write the bodies from any calles to forkhelp
  void mergehelpers();

  //void outputFileTo(std::ostream& o){
  //  o << theFilePrefix().str();
  //  o << "\n\n// Defines: //////////////////////////////////////////////////////\n\n";
  //  _odefines->writeTo(o);
  //  o << "\n\n// Tunables: /////////////////////////////////////////////////////\n\n";
  //  for(TunableDefs::const_iterator i=theTunableDefs().begin(); i!=theTunableDefs().end(); ++i)
  //    o << i->second << "\n";
  //  o << "\n\n// Header Decls: /////////////////////////////////////////////////\n\n";
  //  _oheaders->writeTo(o);
  //  o << "\n\n// Body Decls: ///////////////////////////////////////////////////\n\n";
  //  _omain->writeTo(o);
  //}

  void outputTunables(std::ostream& o){
    TunableDefs::const_iterator i;
    for(i=theTunableDefs().begin(); i!=theTunableDefs().end(); ++i)
      o << i->second << "\n";
  }

  void outputTunableHeaders(std::ostream& o){
    TunableDefs::const_iterator i;
    for(i=theTunableDefs().begin(); i!=theTunableDefs().end(); ++i)
      o << "EXTERN" << i->second << "\n";
  }

  //void outputFileTo(std::ostream& o){
  //  o << theFilePrefix().str();
  //  o << "\n\n// Defines: //////////////////////////////////////////////////////\n\n";
  //  _odefines->writeTo(o);
  //  o << "\n\n// Tunables: /////////////////////////////////////////////////////\n\n";
  //  for(TunableDefs::const_iterator i=theTunableDefs().begin(); i!=theTunableDefs().end(); ++i)
  //    o << i->second << "\n";
  //  o << "\n\n// Header Decls: /////////////////////////////////////////////////\n\n";
  //  _oheaders->writeTo(o);
  //  o << "\n\n// Body Decls: ///////////////////////////////////////////////////\n\n";
  //  _omain->writeTo(o);
  //}


  void callSpatial(const std::string& methodname, const SimpleRegion& region);
  void mkSpatialTask(const std::string& taskname, const std::string& objname, const std::string& methodname, const SimpleRegion& region);
  void mkCreateGpuSpatialMethodCallTask(const std::string& taskname, const std::string& objname, const std::string& methodname, const SimpleRegion& region, std::vector<RegionNodeGroup>& regionNodesGroups, int nodeID, int gpuCopyOut);


  StreamTreePtr startSubfile(const std::string& name) {
    _bcur = new StreamTree(name+" subfile top");
    _ocur = _bcur->add(new StreamTree(name+" subfile main"));
    return _bcur;
  }

  void generateMigrationFunctions();
protected:
  void indent();
public:
  StreamTree& hos() { return _oheaders; }
  StreamTree& dos() { return _odefines; }
  StreamTree& os()  { return _ocur; }
protected:
  std::vector<std::string> _defines;
  StreamTreePtr  _oheaders;
  StreamTreePtr  _odefines;
  StreamTreePtr  _bcur;
  StreamTreePtr  _ocur;
  ClassMembers   _curMembers;
  std::string    _curClass;
  std::string    _curConstructorBody;
  int            _contCounter;
  int            _indent;
  TrainingDepsPtr _cg;
  CodeGenerators _helpers;
  RuleFlavor     _rf;
};

}

#endif
