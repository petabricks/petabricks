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
#ifndef PETABRICKSCONFIGITEM_H
#define PETABRICKSCONFIGITEM_H

#include "common/jassert.h"
#include "common/jprintable.h"
#include "common/jrefcounted.h"

#include <string>
#include <vector>

namespace petabricks {

class ConfigItem;
typedef std::vector<ConfigItem> ConfigItems;

//TemplateArg is the same as config item, but ref counted
typedef ConfigItem TemplateArg;
typedef jalib::JRef<TemplateArg> TemplateArgPtr;
class TemplateArgList: public std::vector<TemplateArgPtr>, public jalib::JRefCounted {};
typedef jalib::JRef<TemplateArgList> TemplateArgListPtr;

class ConfigItem : public jalib::JPrintable, public jalib::JRefCounted {
public:
  enum FlagT {
    FLAG_TUNABLE       = 1<<0,
    FLAG_USER          = 1<<1,
    FLAG_SIZESPECIFIC  = 1<<2,//value depends on transform_n
    FLAG_ACCURACY      = 1<<3,
    FLAG_SIZEVAR       = 1<<4,//value used in to/from/through
    FLAG_FROMCFG       = 1<<5 
  };

  ConfigItem(int flags, std::string name, int initial, int min, int max);
  ConfigItem(std::string name, int min, int max);
  
  std::string name     () const { return _name;     }
  int initial  () const { return _initial;  }
  int min      () const { return _min;      }
  int max      () const { return _max;      }
  int range() const { return _max-_min+1; }

  std::string category() const;

  bool hasFlag(FlagT f) const {
    return (_flags & f) != 0;
  }

  bool shouldPass() const { return hasFlag(FLAG_SIZEVAR) || hasFlag(FLAG_SIZESPECIFIC); }

  void merge(int flags, std::string name, int initial, int min, int max);

  void print(std::ostream& o) const;
private:
  int         _flags;
  std::string _name;
  int         _initial;
  int         _min;
  int         _max;
};


}
#endif
