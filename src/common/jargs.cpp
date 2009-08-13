/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
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
#include "jargs.h"
#include "jconvert.h"


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
    }else{ // -- 
      break;
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
  }
}

template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<bool>(const char* name, bool& val) {
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
  return ParamGlue(name, val, _needHelp);
}

template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<std::vector<std::string> >(const char* name, std::vector<std::string>& val) {
  ParamMap::const_iterator i = _params.find(name);
  if(i == _params.end()){
    return ParamGlue(name, false, _needHelp);
  }
  for(ArgPosList::const_iterator a=i->second.begin(); a!=i->second.end(); ++a){
    val.push_back(getValueOfArg(*a));
  }
  return ParamGlue(name, true, _needHelp);
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
}

jalib::JArgs::ParamGlue jalib::JArgs::param(const char* name) {
  bool tmp=false;
  param(name, tmp);
  return ParamGlue(name, tmp, _needHelp);
}

