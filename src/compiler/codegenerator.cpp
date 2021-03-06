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
#include "codegenerator.h"

#include "region.h"

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

jalib::TunableValueMap& petabricks::CodeGenerator::theHardcodedTunables() {
  static jalib::TunableValueMap t;
  return t;
}

petabricks::CodeGenerator::CodeGenerator(const StreamTreePtr& root, const TrainingDepsPtr& cg)
  : _contCounter(0), _indent(0), _cg(cg)
{
  if(!_cg) _cg = new TrainingDeps();
  _odefines = root->add(new StreamTree("defines"));
  _oheaders = root->add(new StreamTree("headers"));
  _bcur     = root->add(new StreamTree("top"));
  _ocur = _bcur->add(new StreamTree("main"));
}


petabricks::CodeGenerator::CodeGenerator(CodeGenerator& that)
  : jalib::JRefCounted(), _contCounter(0), _indent(0), _cg(that._cg)
{
  _odefines = that._odefines;
  _oheaders = that._oheaders;
  _bcur     = that._bcur;
  _ocur     = new StreamTree("childsection");
  _bcur->add(_ocur.asPtr());
  _curClass = that._curClass;
}


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

void petabricks::CodeGenerator::beginFunc(const std::string& rt, const std::string& func, const std::vector<std::string>& args, bool is_static){
  indent();
  os() << rt << " ";
  if(inClass()) os() << _curClass << "::";
  os() << func << '(';
  jalib::JPrintable::printStlList(os(), args.begin(), args.end(), ", ");
  os() << "){\n";
  _indent++;

  if(inClass()) hos() << "  ";
  if(is_static) hos() << "static ";
  hos() << rt << " " << func << '(';
  jalib::JPrintable::printStlList(hos(), args.begin(), args.end(), ", ");
  hos() << ");\n";

  if(!inClass()) hos() << "\n";
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
  if(_indent<0) _indent = 0;
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

void petabricks::CodeGenerator::trace(const std::string& str){
  indent();
  os() << "JTRACE(\"" << str << "\");\n";
}

namespace{//file local
  void _splitTypeArgs(std::string& type, std::string& name, const std::string& str){
    const char* begin=str.c_str();
    const char* mid=begin+str.length();
    const char* end=mid;
    while(mid>begin && *(--mid)!=' ') ;
//     JTRACE("SPLIT")(begin)(mid+1);
    JASSERT(mid!=begin);
    type.assign(begin,mid);
    name.assign(mid+1,end);
  }
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
  hos() << ("  typedef "+base+" BASE;\n")
        << ("  typedef "+name+" CLASS;\n")
        <<  "public:\n";
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

void petabricks::CodeGenerator::generateMigrationFunctions(){
  CodeGenerator& in = forkhelper();
  CodeGenerator& out = forkhelper();
  CodeGenerator& size = forkhelper();
  CodeGenerator& migrateRegion = forkhelper();
  CodeGenerator& getDataHosts = forkhelper();

  std::vector<std::string> args;
  args.push_back("char* _buf");
  args.push_back("RemoteHost& _host");

  size.beginFunc("size_t", "serialSize");
  out.beginFunc("void", "serialize", args);

  args[0] = "const char* _buf";
  in.beginFunc("void", "unserialize", args);
  size.write("size_t _sz = 0;");

  std::vector<std::string> args2;
  args2.push_back("RemoteHost& sender");
  migrateRegion.beginFunc("void", "migrateRegions", args2);

  std::vector<std::string> args3;
  args3.push_back("DataHostPidList& list");
  getDataHosts.beginFunc("void", "getDataHosts", args3);

  for(ClassMembers::const_iterator i=_curMembers.begin(); i!=_curMembers.end(); ++i){
    if(jalib::StartsWith(i->type, "distributed::")) {
      out.write(i->name + ".serialize(_buf, _host);");
      out.write("_buf += " + i->name + ".serialSize();");
      in.write(i->name + ".unserialize(_buf, _host);");
      in.write("_buf += " + i->name + ".serialSize();");
      size.write("_sz += " + i->name + ".serialSize();");
      migrateRegion.comment(i->name + ".updateHandlerChain();");
      getDataHosts.write(i->name + ".dataHosts(list);");

    }else if(i->type == "IndexT" || i->type == "int" || i->type == "double") {
      out.write("*reinterpret_cast<"+i->type+"*>(_buf) = "+i->name+";");
      in.write(i->name+" = *reinterpret_cast<const "+i->type+"*>(_buf);");
      size.write("_sz  += sizeof("+i->type+");");
      in  .write("_buf += sizeof("+i->type+");");
      out .write("_buf += sizeof("+i->type+");");
    }else if(jalib::StartsWith(i->type, "std::vector<")) {
      out.write("_serialize_vector(_buf, "+i->name+");");
      in.write("_unserialize_vector(_buf, "+i->name+");");
      size.write("_sz += _serialSize_vector("+i->name+");");
    }else if(i->type == "DynamicTaskPtr") {
      if(i->initializer != "") {
        in.write(i->name+" = "+i->initializer+";");
      }
    }else{
      JASSERT(false)(i->type).Text("cant yet serialize type");
    }
  }

  size.write("return _sz;");
  in.write("_sender = &_host;");

  in.endFunc();
  out.endFunc();
  size.endFunc();
  migrateRegion.endFunc();
  getDataHosts.endFunc();

  hos() << _curClass << "(const char*, RemoteHost&, bool=false);\n";
  os() << _curClass << "::" << _curClass << "(const char* _buf, RemoteHost& _host, bool shouldInit){\n";
  incIndent();
  write("unserialize(_buf, _host);");
  beginIf("shouldInit");
  write(_curConstructorBody);
  endIf();
  decIndent();
  os() << "\n}\n";

  beginFunc(_curClass+"*", "_new_constructor", std::vector<std::string>(1,"const char* _buf, RemoteHost& _host"), true);
  write("return new "+_curClass+"(_buf, _host);");
  endFunc();

  beginFunc("RemoteObjectGenerator", "generator");
  write("return &RemoteTaskReciever<"+_curClass+">::gen;");
  endFunc();
}


void petabricks::CodeGenerator::addMember(const std::string& type, const std::string& name, const std::string& initializer){
  if(_curClass.size()>0){
    //TODO
    for(ClassMembers::iterator i = _curMembers.begin(); i != _curMembers.end(); ++i) {
      JASSERT(i->type != type || i->name != name || i->initializer != initializer)(type)(name)(initializer);
    }
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
  if(_rf != RuleFlavor::INVALID)
    beginUserCode(_rf);
#endif
}

void petabricks::CodeGenerator::continuationRequired(const std::string& hookname){
  std::string n = "cont_" + jalib::XToString(_contCounter++);
  newline();
  write("return "+hookname+" new petabricks::MethodCallTask<"+_curClass+", &"+_curClass+"::"+n+">(this));");
  endFunc();
  beginFunc("DynamicTaskPtr", n);
  if(_rf != RuleFlavor::INVALID)
    beginUserCode(_rf);
}


petabricks::CodeGenerator& petabricks::CodeGenerator::forkhelper(){
  CodeGenerator* cg;
  _helpers.push_back(cg=new CodeGenerator(*this));
  return *cg;
}

void petabricks::CodeGenerator::mergehelpers(){
  _ocur = _bcur->add(new StreamTree("main"));
}
void petabricks::CodeGenerator::callSpatial(const std::string& methodname, const SimpleRegion& region) {
  write("{");
  incIndent();
  comment("MARKER 3");
  write("IndexT _tmp_begin[] = {" + region.getIterationLowerBounds() + "};");
  write("IndexT _tmp_end[] = {"   + region.getIterationUpperBounds() + "};");
  write(methodname+"(_tmp_begin, _tmp_end);");
  decIndent();
  write("}");
}


void petabricks::CodeGenerator::mkSpatialTask(const std::string& taskname, const std::string& /*objname*/, const std::string& methodname, const SimpleRegion& region, SpatialCallType spatialCallType) {
  JASSERT(spatialCallType != SpatialCallTypes::WORKSTEALING_PARTIAL);
  // apply_ruleX
  std::string suffix = (spatialCallType == SpatialCallTypes::DISTRIBUTED) ? "_distributed" : "";
  std::string taskclass = "petabricks::SpatialMethodCallTask" + suffix
    + "<CLASS, " + jalib::XToString(region.dimensions() + region.removedDimensions())
    + ", &CLASS::" + methodname;
  if (spatialCallType == SpatialCallTypes::DISTRIBUTED) {
    taskclass += ", &CLASS::" + methodname + "_getDataHosts";
  }
  taskclass += ">";

  write("{");
  incIndent();
  comment("MARKER 6");
  write("IndexT _tmp_begin[] = {" + region.getIterationLowerBounds() + "};");
  write("IndexT _tmp_end[] = {"   + region.getIterationUpperBounds() + "};");
  write(taskname+" = new "+taskclass+"(this,_tmp_begin, _tmp_end);");
  decIndent();
  write("}");
}

void petabricks::CodeGenerator::mkPartialSpatialTask(const std::string& taskname, const std::string& metadataname, const std::string& methodname, const SimpleRegion& region, SpatialCallType spatialCallType, bool shouldGenerateMetadata) {
  JASSERT(spatialCallType == SpatialCallTypes::WORKSTEALING_PARTIAL);
  JASSERT(!shouldGenerateMetadata);
  // apply_ruleX_partial
  std::string taskclass = "petabricks::SpatialMethodCallTask_partial<"
    + metadataname + ", " + jalib::XToString(region.dimensions() + region.removedDimensions())
    + ", &::" + methodname + ">";

  write("{");
  incIndent();
  comment("MARKER 11");
  write("IndexT _tmp_begin[] = {" + region.getIterationLowerBounds() + "};");
  write("IndexT _tmp_end[] = {"   + region.getIterationUpperBounds() + "};");
  write(taskname+" = new "+taskclass+"(_tmp_begin, _tmp_end, metadata);");
  decIndent();
  write("}");
}


void petabricks::CodeGenerator::mkIterationTrampTask(const std::string& taskname, const std::string& /*objname*/, const std::string& methodname, const std::string& metadataclass, const std::string& metadata, const CoordinateFormula& coord) {
  std::string taskclass = "petabricks::IterationTrampMethodCallTask<CLASS"
                          ", " + metadataclass
                        + ", " + jalib::XToString(coord.size())
                        + ", &CLASS::" + methodname
                        + ">";
  write("{");
  incIndent();
  write("IndexT _tmp_coord[] = {" + coord.toString() + "};");
  write(taskname+" = new "+taskclass+"(this, " + metadata + ", _tmp_coord);");
  decIndent();
  write("}");

}

#ifdef HAVE_OPENCL
void petabricks::CodeGenerator::mkCreateGpuSpatialMethodCallTask(
    const std::string& transname,
    const std::string& taskname, 
    const std::string& objname, 
    const std::string& methodname, 
    const SimpleRegion& region, 
    std::vector<RegionNodeGroup>& regionNodesGroups, 
    int nodeID, 
    int gpuCopyOut, 
    RegionList to, 
    bool divisible) {
  std::string taskclass
;
  int dim_int = region.totalDimensions();
  std::string lastdim = jalib::XToString(dim_int - 1);
  std::string max = region.maxCoord()[dim_int - 1]->toString();
  std::string min = region.minCoord()[dim_int - 1]->toString();

  if(!divisible) {
    taskclass = "petabricks::CreateGpuSpatialMethodCallTask<"+objname
      + ", " + jalib::XToString(region.totalDimensions())
      + ", &" + objname + "::" + methodname + TX_OPENCL_POSTFIX + "_createtasks"
      + ">";
    //beginIf(min+"<"+max);
    comment("MARKER 6");
    write("IndexT _tmp_begin[] = {" + region.getIterationLowerBounds() + "};");
    write("IndexT _tmp_end[] = {"   + region.getIterationUpperBounds() + "};");
    write("RegionNodeGroupMapPtr groups = new RegionNodeGroupMap();");
    for(std::vector<RegionNodeGroup>::iterator group = regionNodesGroups.begin(); group != regionNodesGroups.end(); ++group){
      write("{");
      incIndent();
      write("std::set<int> ids;");
      for(std::vector<int>::iterator id = group->nodeIDs().begin(); id != group->nodeIDs().end(); ++id){
	write("ids.insert("+jalib::XToString(*id)+");");
      }
      write("groups->insert(RegionNodeGroup(\""+group->matrixName()+"\",ids));");
      decIndent();
      write("}");
    }
    write(taskname+" = new "+taskclass+"(this,_tmp_begin, _tmp_end, "+jalib::XToString(nodeID)+", groups, "+jalib::XToString(gpuCopyOut)+");");
    //endIf();

    return;
  }

  std::string n_div = "cont_" + jalib::XToString(_contCounter++);
  write(taskname + " = new petabricks::MethodCallTask<"+_curClass+", &"+_curClass+"::"+n_div+">( this );");

  // Add divider function
  CodeGenerator helper = forkhelper();
  helper.beginFunc("DynamicTaskPtr", n_div);
  helper.write("DynamicTaskPtr _fini = new NullDynamicTask();");

  // Assign the gpu-cpu division point.
  helper.write("ElementT gpu_ratio = "+transname+"_gpuratio/8.0;");

  std::string div = "div";
  RegionPtr proxy = to.front();
  helper.write("IndexT totalRow = "+proxy->matrix()->name()+".size("+jalib::XToString(dim_int - 1)+");");
  helper.write("IndexT div = ceil(gpu_ratio * totalRow);");
  helper.beginIf("div > " + max);
  helper.write("div = "+max+";");
  helper.endIf();

  // GPU

  taskclass = "petabricks::CreateGpuSpatialMethodCallTask<"+objname
              + ", " + jalib::XToString(dim_int)
              + ", &" + objname + "::" + methodname + TX_OPENCL_POSTFIX + "_createtasks"
              + ">";
  helper.comment("MARKER 6");
  
  //helper.beginIf(min+" < div");
  helper.write("IndexT _gpu_begin[] = {" + region.getIterationLowerBounds() + "};");
  helper.write("IndexT _gpu_end[] = {" + region.getIterationMiddleEnd(div) + "};");
  helper.write("RegionNodeGroupMapPtr groups = new RegionNodeGroupMap();");

  for(std::vector<RegionNodeGroup>::iterator group = regionNodesGroups.begin(); group != regionNodesGroups.end(); ++group){
    helper.write("{");
    helper.incIndent();
    helper.write("std::set<int> ids;");
    for(std::vector<int>::iterator id = group->nodeIDs().begin(); id != group->nodeIDs().end(); ++id){
      helper.write("ids.insert("+jalib::XToString(*id)+");");
    }
    helper.write("groups->insert(RegionNodeGroup(\""+group->matrixName()+"\",ids));");
    helper.decIndent();
    helper.write("}");
  }

  helper.write("DynamicTaskPtr gpu_task = new "+taskclass+"(this,_gpu_begin, _gpu_end, "+jalib::XToString(nodeID)+", groups, "+jalib::XToString(gpuCopyOut)+");");
  helper.write("gpu_task->enqueue();");
  helper.write("_fini->dependsOn(gpu_task);");
  //helper.endIf();

  // CPU
  taskclass = "petabricks::SpatialMethodCallTask<CLASS"
              ", " + jalib::XToString(dim_int)
              + ", &CLASS::" + methodname + "_workstealing_wrap"
    //+ ", &CLASS::" + methodname + "_workstealing"
              + ">";
  helper.beginIf("div < " + max);
  helper.beginIf("div < " + min);
  helper.write("div = "+min+";");
  helper.endIf();
  helper.comment("MARKER 6");
  helper.write("IndexT _cpu_begin[] = {" + region.getIterationMiddleBegin(div) + "};");
  helper.write("IndexT _cpu_end[] = {"   + region.getIterationUpperBounds() + "};");
  helper.write("DynamicTaskPtr cpu_task = new "+taskclass+"(this,_cpu_begin, _cpu_end);");
  helper.write("cpu_task->enqueue();");
  helper.write("_fini->dependsOn(cpu_task);");
  helper.endIf();
  
  helper.write("return _fini;");
  helper.endFunc();
}

void petabricks::CodeGenerator::cout(const std::string& s) {
  write("std::cout << \"" + s + "\" << std::endl;");
}

void petabricks::CodeGenerator::cout(const std::string& s) {
  write("std::cout << \"" + s + "\" << std::endl;");
}

#endif


