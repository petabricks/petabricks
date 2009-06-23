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

#include <vector>
#include <map>
#include <string>

namespace jalib {

class JArgs {
public:
  JArgs(int argc, const char** argv, const char* defaultParam = "args");
  
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
  ParamGlue param(const char* name, T& val) const;
  
  ParamGlue param(const char* name) const;

  bool needHelp() const {
    return _needHelp; 
  }

private:
  typedef std::vector<std::string> ArgList;
  typedef std::map<std::string, ArgList> ParamMap;
  ParamMap  _params;
  bool _needHelp;
};
  
//generic single arg parsing
template < typename T > 
inline JArgs::ParamGlue JArgs::param(const char* name, T& val) const {
  ParamMap::const_iterator i = _params.find(name);
  if(i == _params.end()){
    return ParamGlue(name, false, _needHelp);
  }
  JASSERT(i->second.size()==1)(i->second.size())(name)
    .Text("Expected exactly one argument for parameter");
  val = jalib::StringToX<T>(i->second.front());
  return ParamGlue(name, true, _needHelp);
}

//specialized version for parsing boolean parameters
template <>
JArgs::ParamGlue JArgs::param<bool>(const char* name, bool& val) const;

//specialized version for parsing list parameters
template <>
JArgs::ParamGlue JArgs::param<std::vector<std::string> >(const char* name, std::vector<std::string>& val) const;


}

#endif
