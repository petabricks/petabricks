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
#include "configitem.h"

#include "codegenerator.h"

#include "common/jconvert.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif
  
petabricks::ConfigItem::ConfigItem(int flags, std::string name, jalib::TunableValue initial, jalib::TunableValue min, jalib::TunableValue max)
    :_flags(flags),
     _name(name),
     _initial(initial),
     _min(min),
     _max(max),
     _arraySize(0)
{
}
  
petabricks::ConfigItem::ConfigItem(std::string name, jalib::TunableValue min, jalib::TunableValue max)
  :_flags(0),
  _name(name),
  _initial(min),
  _min(min),
  _max(max),
  _arraySize(0)
{
}

std::string petabricks::ConfigItem::category() const {
  std::string cat;

  if(hasFlag(ConfigItem::FLAG_USER))
    cat+="user.";
  else
    cat+="system.";

  if(hasFlag(ConfigItem::FLAG_TUNABLE))
    cat+="tunable";
  else
    cat+="config";

  if(hasFlag(ConfigItem::FLAG_DOUBLE))
    cat+=".double";
  
  if(hasFlag(ConfigItem::FLAG_ACCURACY))
    cat+=".accuracy";
  
  if(hasFlag(ConfigItem::FLAG_SIZESPECIFIC))
    cat+=".array";

  return cat;
}

void petabricks::ConfigItem::merge(const ConfigItem& that){
  JTRACE("merged cfg")(*this)(that);
  _flags|=that._flags;
  JASSERT(that._name==_name);
  _initial=jalib::TunableValue::max(_initial, that._initial);
  _min    =jalib::TunableValue::max(_min,     that._min);
  _max    =jalib::TunableValue::min(_max,     that._max);
}

void petabricks::ConfigItem::print(std::ostream& o) const{
  o << _name << "(range: " << _min << " to " << _max << " flags:";
  if(hasFlag(FLAG_TUNABLE))       o << " FLAG_TUNABLE";
  if(hasFlag(FLAG_USER))          o << " FLAG_USER";
  if(hasFlag(FLAG_SIZESPECIFIC))  o << " FLAG_SIZESPECIFIC";
  if(hasFlag(FLAG_ACCURACY))      o << " FLAG_ACCURACY";
  if(hasFlag(FLAG_SIZEVAR))       o << " FLAG_SIZEVAR";
  if(hasFlag(FLAG_FROMCFG))       o << " FLAG_FROMCFG";
  if(hasFlag(FLAG_TEMPLATEVAR))   o << " FLAG_TEMPLATEVAR";
  if(hasFlag(FLAG_DOUBLE))        o << " FLAG_DOUBLE";
  if(hasFlag(FLAG_ARRAY))         o << " FLAG_ARRAY(" << _arraySize << ")" ;
  if(hasFlag(FLAG_FORCEPASS))     o << " FLAG_FORCEPASS";
  o << ")";
}


void petabricks::ConfigItem::initDefaults() {
  if(hasFlag(FLAG_DOUBLE)){
    if(_initial == jalib::TunableValue()) _initial = 0.0 ;
    if(_min == jalib::TunableValue())     _min = jalib::minval<double>();
    if(_max == jalib::TunableValue())     _max = jalib::maxval<double>();
  }else{
    if(_initial == jalib::TunableValue()) _initial = 0 ;
    if(_min == jalib::TunableValue())     _min = 0;
    if(_max == jalib::TunableValue())     _max = jalib::maxval<int>();
  }
  JASSERT(_min <= _max)(_name)(_min)(_max)
    .Text("invalid tunable setting");
  JASSERT(_min <= _initial && _initial <=_max )
    (_name)(_initial)(_min)(_max)
    .Text("invalid tunable setting");
}

void petabricks::ConfigItem::createTunableDecls(const std::string& prefix, CodeGenerator& o) const {
  JASSERT(hasFlag(FLAG_FROMCFG));
  if(hasFlag(FLAG_ARRAY)){
    ConfigItem tmp = *this;
    tmp.removeFlag(FLAG_ARRAY);
    for(size_t i=0; i<_arraySize; ++i) {
      tmp.createTunableDecls(prefix+"i"+jalib::XToString(i)+"_", o);
    }
  }else{
    const char* type = hasFlag(FLAG_DOUBLE) ? "DOUBLE" : "";
    if(hasFlag(FLAG_SIZESPECIFIC)){
      o.createTunableArray(category(), prefix+name(), MAX_INPUT_BITS, initial(), min(), max(), hasFlag(FLAG_TUNABLE), type);
    }else{
      o.createTunable(hasFlag(FLAG_TUNABLE), category(), prefix+name(), initial(), min(), max(), type);
    }
  }
}

void petabricks::ConfigItem::assignTunableDecls(const std::string& prefix,
                                                CodeGenerator& o,
                                                const std::string& sizestr,
                                                const std::string& arraystr) const {
  if(hasFlag(FLAG_ARRAY)){
    ConfigItem tmp = *this;
    tmp.removeFlag(FLAG_ARRAY);
    tmp.addFlag(FLAG_FORCEPASS);
    o.write(name()+".resize("+jalib::XToString(_arraySize)+");");
    for(size_t i=0; i<_arraySize; ++i) {
      tmp.assignTunableDecls(prefix+"i"+jalib::XToString(i)+"_", o, sizestr, "["+jalib::XToString(i)+"]");
    }
  }else{
    if(hasFlag(ConfigItem::FLAG_SIZESPECIFIC)){
      o.write(name()+arraystr+" = petabricks::interpolate_sizespecific("+
                                       prefix+name()+","
                                       +sizestr +" ,"+
                                       jalib::XToString(min())+");");
    }else{
      if(shouldPass() && hasFlag(ConfigItem::FLAG_FROMCFG)){
       o.write(name()+arraystr+" = "+prefix+name()+";");
      }
    }
  }
}


