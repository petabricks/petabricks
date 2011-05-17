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
#ifndef PETABRICKSCONFIGITEM_H
#define PETABRICKSCONFIGITEM_H


#include "common/jassert.h"
#include "common/jprintable.h"
#include "common/jrefcounted.h"
#include "common/jtunable.h"
#include "common/srcpos.h"

#include <string>
#include <vector>

namespace petabricks {

class CodeGenerator;
class ConfigItem;
typedef std::vector<ConfigItem> ConfigItems;

//TemplateArg is the same as config item, but ref counted
//TODO: finish renaming all the uses so we can get rid of the typedef
typedef ConfigItem TemplateArg;
typedef jalib::JRef<TemplateArg> TemplateArgPtr;
class TemplateArgList: public std::vector<TemplateArgPtr>, public jalib::JRefCounted, public jalib::SrcPosTaggable {};
typedef jalib::JRef<TemplateArgList> TemplateArgListPtr;

class ConfigItem : public jalib::JPrintable, public jalib::JRefCounted, public jalib::SrcPosTaggable {
public:
  enum FlagT {
    FLAG_TUNABLE       = 1<<0,
    FLAG_USER          = 1<<1,
    FLAG_SIZESPECIFIC  = 1<<2,//value depends on transform_n
    FLAG_ACCURACY      = 1<<3,
    FLAG_SIZEVAR       = 1<<4,//value used in to/from/through
    FLAG_FROMCFG       = 1<<5,
    FLAG_TEMPLATEVAR   = 1<<6,
    FLAG_DOUBLE        = 1<<7,//store as floating point
    FLAG_ARRAY         = 1<<8,
    FLAG_FORCEPASS     = 1<<9
  };

  ConfigItem(int flags, std::string name, jalib::TunableValue initial, jalib::TunableValue min, jalib::TunableValue max);
  ConfigItem(std::string name, jalib::TunableValue min, jalib::TunableValue max);
  
  std::string name() const { return _name; }
  jalib::TunableValue initial() const { return _initial; }
  jalib::TunableValue min() const { return _min; }
  jalib::TunableValue max() const { return _max; }
  int range() const { return _max.i()-_min.i()+1; }


  void initDefaults();

  ///
  /// the category string is based on _flags and put in the .info file
  std::string category() const;

  bool hasFlag(FlagT f) const {
    return (_flags & f) != 0;
  }

  bool shouldPass() const {
    return hasFlag(FLAG_SIZEVAR) || hasFlag(FLAG_SIZESPECIFIC) || hasFlag(FLAG_ARRAY) || hasFlag(FLAG_FORCEPASS);
  }

  void merge(int flags, std::string name, jalib::TunableValue initial, jalib::TunableValue min, jalib::TunableValue max){
    merge(ConfigItem(flags, name, initial, min, max));
  }
  void merge(const ConfigItem& that);
  
  void addFlag(int flag){ _flags|=flag; }
  void removeFlag(int flag){ _flags&=~flag; }

  void print(std::ostream& o) const;

  void setInitial(const jalib::TunableValue& i) { _initial=i; }
  void setMin(const jalib::TunableValue& i)     { _min=i; }
  void setMax(const jalib::TunableValue& i)     { _max=i; }
  void setName(const std::string& i)            { _name=i; }
  
  void setArraySize(size_t i) { _arraySize=i; }

  void createTunableDecls(const std::string& prefix, CodeGenerator& o) const;
  
  void assignTunableDecls(const std::string& prefix,
                          CodeGenerator& o,
                          const std::string& sizestr,
                          const std::string& arraystr="") const;

  std::string elementType() const {
    return hasFlag(FLAG_DOUBLE) ? "double" : "int";
  }
  std::string passType() const {
    if(hasFlag(FLAG_ARRAY))
      return "std::vector<"+elementType()+">&";
    else
      return elementType();
  }
  std::string memberType() const {
    if(hasFlag(FLAG_ARRAY))
      return "std::vector<"+elementType()+">";
    else
      return elementType();
  }
private:
  int                 _flags;
  std::string         _name;
  jalib::TunableValue _initial;
  jalib::TunableValue _min;
  jalib::TunableValue _max;
  size_t              _arraySize;
};

}

#endif

