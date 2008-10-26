/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
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
#ifndef JALIBJTUNABLE_H
#define JALIBJTUNABLE_H

#include "jassert.h"

#include <limits>
#include <set>
#include <map>

#define JTUNABLE(name, args...) \
  jalib::JTunable name(#name, args)

namespace jalib {

typedef int TunableValue;
class JTunable;

class JTunableConfiguration : public std::map<JTunable*, TunableValue> {};


class JTunableManager : public std::set<JTunable*> {
public:
  static JTunableManager& instance(){ static JTunableManager t; return t; }

  JTunableConfiguration getCurrentConfiguration() const;
};


class JTunable{
public:
  JTunable( const char* name
          , TunableValue initial
          , TunableValue min=std::numeric_limits<TunableValue>::min()
          , TunableValue max=std::numeric_limits<TunableValue>::max())
    : _name(name), _value(initial), _min(min), _max(max)
  {
    JTunableManager::instance().insert(this);
  }

  ~JTunable() {
    JTunableManager::instance().erase(this);
  }

  operator TunableValue () const { return _value; }


  TunableValue value() const { return _value; }
  void setValue(TunableValue v) { _value=v; }
private:
  std::string  _name;
  TunableValue _value;
  TunableValue _min;
  TunableValue _max;
};



}

#endif
