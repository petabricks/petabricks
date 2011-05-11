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
#include "jargs.h"
#include "jtunable.h"

#include <algorithm>

namespace{ //helpers
  bool isParamLike(const char* param){
    static const std::string dash="-";
    static const std::string dashdash="--";
    return param[0]=='-' && dash!=param && dashdash!=param;
  }
}

jalib::JArgs::JArgs(int argc, const char** argv) 
  : _needHelp(false)
{
  _args.reserve(argc+1);
  JASSERT(argc>0);
  for(; argc>0; --argc, ++argv)
    _args.push_back(*argv);
  _args[0].setFlag(Arg::T_PROGNAME);
  _args.push_back(Arg::T_END_OF_ARGS);
  rebuildParamMap();
}

void jalib::JArgs::rebuildParamMap() {
  _params.clear();
  static const std::string dashdash="--";
  
  //parse params
  for(size_t i=1; i+1<_args.size(); ++i){
    const char* str=_args[i].c_str();
    if(str==dashdash)
      break;
    if(isParamLike(str)){
      while(*str=='-') ++str;
      if(jalib::Contains(str, '=')){ // --foo=bar
        std::string name,val;
        jalib::SplitFirst(name, val, str, '=');
        _params[name].push_back(&_args[i]);
        _args[i].setFlag(Arg::T_PARAM_NAMEVALUE);
      }else{ // --foo
        _params[str].push_back(&_args[i]);
        _args[i].setFlag(Arg::T_PARAM_NAME);
      }
    }
  }

  _needHelp=false;
  param("help", _needHelp);
}

std::string jalib::JArgs::getValueOfArg(Arg* arg) {
  arg->setFlag(Arg::T_USED);
  if(arg->hasFlag(Arg::T_PARAM_NAMEVALUE)){
    std::string name,val;
    jalib::SplitFirst(name, val, *arg, '=');
    return val;
  }else{
    JASSERT(arg->hasFlag(Arg::T_PARAM_NAME))(*arg);
    Arg* val = arg+1;
    JASSERT(!val->hasFlag(Arg::T_END_OF_ARGS))(*arg)
      .Text("expected value type after "+*arg);
    JASSERT(!val->hasFlag(Arg::T_PARAM_NAME))(*arg)(*val)
      .Text("expected value type after "+*arg);
    JASSERT(!val->hasFlag(Arg::T_ARG))(*arg)(*val)
      .Text("parameter extract after it was used as a default arg");
    val->setFlag(Arg::T_PARAM_VALUE);
    val->setFlag(Arg::T_USED);
    return *val;
  }
}

static bool _argToBool(const std::string& name, const char* v){
  if(*v=='1' || *v=='t' || *v=='T' || *v=='y' || *v=='Y') {
    return true;
  } else if(*v=='0' || *v=='f' || *v=='F' || *v=='n' || *v=='N') {
    return false;
  } else {
    JASSERT(false)(name)(v)
      .Text("expected 'true' or 'false'");
    return false;
  }
}

template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<bool>(const char* name, bool& val) {
  if(_needHelp){
    _help[name].type=HelpInfo::mktypestr<bool>();
    _help[name].initial=val?"yes":"no";
  }
  std::string invname="no"+std::string(name);
  ParamMap::const_iterator i = _params.find(name);
  ParamMap::const_iterator inv = _params.find(invname);
  if(i != _params.end()){
    JASSERT(i->second.size()==1)(i->second.size())(name).Text("parameter given multiple times");
    JASSERT(inv==_params.end())(name)(invname).Text("conflicting parameters");
    Arg* a = i->second.front();
    if(a->hasFlag(Arg::T_PARAM_NAMEVALUE)){
      val=_argToBool(name, getValueOfArg(a).c_str());
    }else{
      val=true;
      a->setFlag(Arg::T_USED);
    }
  }else if(inv != _params.end()){
    param<bool>(invname.c_str(), val);
    val = !val;
  }
  return ParamGlue(name, val, *this);
}

template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<std::vector<std::string> >(const char* name, std::vector<std::string>& val) {
  if(_needHelp){
    _help[name].type=HelpInfo::mktypestr<std::vector<std::string> >();
  }
  ParamMap::const_iterator i = _params.find(name);
  if(i == _params.end()){
    return ParamGlue(name, false, *this);
  }
  for(ArgPosList::const_iterator a=i->second.begin(); a!=i->second.end(); ++a){
    val.push_back(getValueOfArg(*a));
  }
  return ParamGlue(name, true, *this);
}

template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<jalib::JTunableInt>(const char* name, JTunableInt& val) {
  int t = val.value().i();
  ParamGlue rv = param(name, t);
  if(t != val.value())
    val.setValue(t);
  return rv;
}


template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<jalib::JTunableDouble>(const char* name, JTunableDouble& val) {
  double t = val.value().d();
  ParamGlue rv = param(name, t);
  if(t != val.value())
    val.setValue(t);
  return rv;
}
  
jalib::JArgs::ParamGlue jalib::JArgs::param(const char* name) {
  bool tmp=false;
  param(name, tmp);
  return ParamGlue(name, tmp, *this);
}

void jalib::JArgs::addHelpMsg(const char* name, const char* msg){
  if(_needHelp){
    std::vector<std::string>::const_iterator i;
    HelpInfo& h = _help[name];
    h.msg = msg;
    if(h.type=="bool"){
      if(h.initial=="no"){
        std::cerr << "  --" << name;
        for(i=h.aliases.begin(); i!=h.aliases.end(); ++i)
          std::cerr << ", --" << *i;
      }else{
        std::cerr << "  --no" << name;
        for(i=h.aliases.begin(); i!=h.aliases.end(); ++i)
          std::cerr << ", --no" << *i;
      }
    }else{
      std::cerr << "  --" << name << h.type;
      for(i=h.aliases.begin(); i!=h.aliases.end(); ++i)
        std::cerr << ", --" << *i << h.type;
      if(h.initial!="") std::cerr << " (default: " << h.initial << ")";
    }
    std::cerr << std::endl;
    std::cerr << "      " << h.msg << std::endl;
  }
}

void jalib::JArgs::finishParsing(std::vector<std::string>& outputArgs){
  for(size_t i=1; i+1<_args.size(); ++i){
    if( ! _args[i].hasFlag(Arg::T_USED)){
      JASSERT(! _args[i].hasFlag(Arg::T_PARAM_NAME))(_args[i])
        .Text("unknown parameter");
      JASSERT(! _args[i].hasFlag(Arg::T_PARAM_VALUE))(_args[i])
        .Text("unused parameter value");
      _args[i].setFlag(Arg::T_ARG);
      _args[i].setFlag(Arg::T_USED);
      outputArgs.push_back(_args[i]);
    }
  }
  if(needHelp()){
    for(HelpInfos::const_iterator i=_help.begin(); i!=_help.end(); ++i){
      if(i->second.msg==""){
        if(i->first=="help")
          addHelpMsg(i->first.c_str(), "display this message" );
        else
          addHelpMsg(i->first.c_str(), "undocumented" );
      }
    }
  }
}
  
void jalib::JArgs::alias(const char* alias, const char* orig){
  ParamMap::iterator i = _params.find(alias);
  if(i!=_params.end()){
    ArgPosList& a = _params[orig];
    ArgPosList& b = i->second;
    if(a.size()==0){
      a.swap(b);
    }else{
      a.insert(a.end(), b.begin(), b.end());
      sort(a.begin(), a.end());
    }
    _params.erase(i);
  }
  if(needHelp()){
    _help[orig].aliases.push_back(alias);
  }
}





