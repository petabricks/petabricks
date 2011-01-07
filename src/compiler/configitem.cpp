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
  
petabricks::ConfigItem::ConfigItem(int flags, std::string name, jalib::TunableValue initial, jalib::TunableValue min, jalib::TunableValue max)
    :_flags(flags),
     _name(name),
     _initial(initial),
     _min(min),
     _max(max)
{
  JASSERT(max>=min)(min)(max);
}
  
petabricks::ConfigItem::ConfigItem(std::string name, jalib::TunableValue min, jalib::TunableValue max)
  :_flags(0),
  _name(name),
  _initial(min),
  _min(min),
  _max(max) 
{
  JASSERT(max>=min)(min)(max);
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

void petabricks::ConfigItem::merge(int flags, std::string name, jalib::TunableValue initial, jalib::TunableValue min, jalib::TunableValue max){
  _flags|=flags;
  JASSERT(name==_name);
  _initial=std::max(_initial, initial);
  _min=std::max(_min, min);
  _max=std::min(_max, max);
  JTRACE("merged cfg")(_flags)(_name);
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
  o << ")";
}

