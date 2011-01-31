/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#include "configitem.h"

#include "common/jconvert.h"
  
petabricks::ConfigItem::ConfigItem(int flags, std::string name, jalib::TunableValue initial, jalib::TunableValue min, jalib::TunableValue max)
    :_flags(flags),
     _name(name),
     _initial(initial),
     _min(min),
     _max(max)
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
  if(hasFlag(FLAG_ARRAY))         o << " FLAG_ARRAY";
  o << ")";
}


void petabricks::ConfigItem::initDefaults() {
  if(_initial == jalib::TunableValue())
    _initial = hasFlag(FLAG_DOUBLE) ? 0.0 : 0;
  if(_min == jalib::TunableValue())
    _min = hasFlag(FLAG_DOUBLE) ? jalib::minval<double>() : 0;
  if(_max == jalib::TunableValue())
    _max = hasFlag(FLAG_DOUBLE) ? jalib::maxval<double>() : jalib::maxval<int>();

}


