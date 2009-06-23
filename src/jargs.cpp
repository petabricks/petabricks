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
#define shift --argc, ++argv

  bool isParamLike(const char* param){
    static const std::string dash="-";
    static const std::string dashdash="--";
    return param[0]=='-' && dash!=param && dashdash!=param;
  }
    

}

jalib::JArgs::JArgs(int argc, const char** argv, const char* defaultParam) 
  : _needHelp(false)
{
  static const std::string dashdash="--";
  shift;// pop program name;

  //parse params
  for(;argc>0; shift){
    if(isParamLike(argv[0])){
      const char* str=argv[0];
      while(*str=='-') ++str;
      if(jalib::Contains(str, '=')){ // --foo=bar
        std::string name,val;
        jalib::SplitFirst(name, val, str, '=');
        _params[name].push_back(val);
      }else{ // --foo
        _params[str];
      }
    }else if(argv[0]!=dashdash){ // arg
      _params[defaultParam].push_back(argv[0]);
    }else{ // -- 
      shift;
      break;
    }
  }

  //parse what is left
  for(;argc>0; shift){
    _params[defaultParam].push_back(argv[0]);
  }

  param("help", _needHelp);
}

template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<bool>(const char* name, bool& val) const {
  ParamMap::const_iterator i = _params.find(name);
  if(i != _params.end()){
    int n = i->second.size();
    JASSERT(n<=1)(n)(name).Text("too many args for parameter");
    if(n==0){
      val = true;
    }else{
      const char* v = i->second.front().c_str();
      if(*v=='0' || *v=='t' || *v=='T' || *v=='y' || *v=='Y') {
        val = true;
      } else if(*v=='1' || *v=='f' || *v=='F' || *v=='n' || *v=='N') {
        val = false;
      } else {
        JASSERT(false)(name)(v)
          .Text("expected 'true' or 'false'");
      }
    }
  }
  return ParamGlue(name, val, _needHelp);
}

template <>
jalib::JArgs::ParamGlue jalib::JArgs::param<std::vector<std::string> >(const char* name, std::vector<std::string>& val) const {
  ParamMap::const_iterator i = _params.find(name);
  if(i == _params.end()){
    return ParamGlue(name, false, _needHelp);
  }
  val = i->second;
  return ParamGlue(name, true, _needHelp);
}


