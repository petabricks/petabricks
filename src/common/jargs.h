/***************************************************************************
 *   Copyright (C) 2009 by Jason Ansel                                     *
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
#ifndef JALIBJARGS_H
#define JALIBJARGS_H

#include "jassert.h"
#include "jconvert.h"

#include <map>
#include <string>
#include <vector>

namespace jalib {

class JArgs {
public:
  JArgs(int argc, const char** argv);
  
  class ParamGlue {
  public:
    ParamGlue(const char* name, bool exists, bool needHelp) 
      : _name(name)
      , _exists(exists)
      , _needHelp(needHelp)
    {}
    ParamGlue& help(const char* msg) {
      if(_needHelp){
        fprintf(stderr, "  --%s : %s\n", _name, msg);
      }
      return *this;
    }
    ParamGlue& required() {
      JASSERT(_exists)(_name).Text("Missing required parameter");
      return *this;
    }

    operator bool () const { return _exists && !_needHelp; }
  private:
    const char* _name;
    bool _exists;
    bool _needHelp;
  };

  template < typename T > 
  ParamGlue param(const char* name, T& val);
  
  ParamGlue param(const char* name);

  void finishParsing(std::vector<std::string>& outputArgs);
  void finishParsing(){ std::vector<std::string> t; finishParsing(t); }

  bool needHelp() const {
    return _needHelp; 
  }

protected:

  class Arg : public std::string {
  public:
    enum Flag {
      T_NONE            = 0,
      T_USED            = 1,
      T_PROGNAME        = 2,
      T_PARAM_NAME      = 4,   // --foo
      T_PARAM_VALUE     = 8,   // bar (in --foo bar)
      T_PARAM_NAMEVALUE = T_PARAM_NAME|T_PARAM_VALUE, //--foo=bar
      T_ARG             = 16,
      T_END_OF_ARGS     = 32
    };

    Arg(Flag t, const char* s="") : std::string(s), _flags(t) {}
    Arg(const char* s) : std::string(s), _flags(T_NONE) {}

    void setFlag(Flag t) { _flags = _flags | t; }
    bool hasFlag(Flag t) const { return (_flags&t)==t; }
  private:
    int _flags;
  };
  
  void rebuildParamMap();
  std::string getValueOfArg(Arg* arg);
private:
  typedef std::vector<Arg>  ArgList;
  typedef std::vector<Arg*> ArgPosList;
  typedef std::map<std::string, ArgPosList> ParamMap;
  bool _needHelp;

  ParamMap  _params;
  ArgList   _args;
};
  
//generic single arg parsing
template < typename T > 
inline JArgs::ParamGlue JArgs::param(const char* name, T& val){
  ParamMap::const_iterator i = _params.find(name);
  if(i == _params.end()){
    return ParamGlue(name, false, _needHelp);
  }
  JASSERT(i->second.size()==1)(i->second.size())(name)
    .Text("parameter given multiple times");
  val = jalib::StringToX<T>(getValueOfArg(i->second.front()));
  return ParamGlue(name, true, _needHelp);
}

//specialized version for parsing boolean parameters
template <>
JArgs::ParamGlue JArgs::param<bool>(const char* name, bool& val);

//specialized version for parsing list parameters
template <>
JArgs::ParamGlue JArgs::param<std::vector<std::string> >(const char* name, std::vector<std::string>& val);


}

#endif
