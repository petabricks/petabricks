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
#ifndef JALIBJARGS_H
#define JALIBJARGS_H

#include "jassert.h"
#include "jconvert.h"

#include <map>
#include <string>
#include <vector>

namespace jalib {
class JTunableInt;
class JTunableDouble;
class JArgs;

class JArgs {
public:
  JArgs(int argc, const char** argv);
  
  class ParamGlue {
  public:
    ParamGlue(const char* name, bool exists, JArgs& a) 
      : _name(name)
      , _exists(exists)
      , _args(a)
    {}
    ParamGlue& help(const char* msg) {
      _args.addHelpMsg(_name,msg);
      return *this;
    }
    ParamGlue& required() {
      JASSERT(_exists || _args.needHelp())(_name).Text("Missing required parameter");
      return *this;
    }

    operator bool () const { return _exists && !_args.needHelp(); }
  private:
    const char* _name;
    bool        _exists;
    JArgs&      _args;
  };

  template < typename T > 
  ParamGlue param(const char* name, T& val);
  
  ParamGlue param(const char* name);

  void finishParsing(std::vector<std::string>& outputArgs);
  void finishParsing(){ std::vector<std::string> t; finishParsing(t); }

  bool needHelp() const {
    return _needHelp; 
  }

  void alias(const char* alias, const char* orig);
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

  void addHelpMsg(const char* name, const char* msg);


private:
  typedef std::vector<Arg>  ArgList;
  typedef std::vector<Arg*> ArgPosList;
  typedef std::map<std::string, ArgPosList> ParamMap;
  bool _needHelp;

  ParamMap  _params;
  ArgList   _args;

  struct HelpInfo { 
    template <typename T> static const char* mktypestr() { return "=..."; }
    std::string type;
    std::string msg;
    std::string initial;
    std::vector<std::string> aliases;
  };
  typedef std::map<std::string, HelpInfo> HelpInfos;
  HelpInfos _help;
};
  
//generic single arg parsing
template < typename T > 
inline JArgs::ParamGlue JArgs::param(const char* name, T& val){
  if(_needHelp){
    _help[name].type=HelpInfo::mktypestr<T>();
    _help[name].initial=jalib::XToString<T>(val);
  }

  ParamMap::const_iterator i = _params.find(name);
  if(i == _params.end()){
    return ParamGlue(name, false, *this);
  }
  JASSERT(i->second.size()==1)(i->second.size())(name)
    .Text("parameter given multiple times");
  val = jalib::StringToX<T>(getValueOfArg(i->second.front()));
  return ParamGlue(name, true, *this);
}

//specialized version for parsing boolean parameters
template <>
JArgs::ParamGlue JArgs::param<bool>(const char* name, bool& val);

//specialized version for parsing list parameters
template <>
JArgs::ParamGlue JArgs::param<std::vector<std::string> >(const char* name, std::vector<std::string>& val);
template <>
JArgs::ParamGlue JArgs::param<JTunableInt>(const char* name, JTunableInt& val);
template <>
JArgs::ParamGlue JArgs::param<JTunableDouble>(const char* name, JTunableDouble& val);

template <> inline const char* JArgs::HelpInfo::mktypestr<bool>()         { return "bool"; }
template <> inline const char* JArgs::HelpInfo::mktypestr<int>()          { return "=<INTEGER>"; }
template <> inline const char* JArgs::HelpInfo::mktypestr<short>()        { return "=<INTEGER>"; }
template <> inline const char* JArgs::HelpInfo::mktypestr<unsigned int>() { return "=<INTEGER>"; }
template <> inline const char* JArgs::HelpInfo::mktypestr<double>()       { return "=<NUMBER>"; }
template <> inline const char* JArgs::HelpInfo::mktypestr<float>()        { return "=<NUMBER>"; }
template <> inline const char* JArgs::HelpInfo::mktypestr<std::string>()  { return "=<STRING>"; }
template <> inline const char* JArgs::HelpInfo::mktypestr<std::vector<std::string> >()  { return "=<STRING>"; }

}

#endif
