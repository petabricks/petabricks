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
#include "jprintable.h"

#include <limits>
#include <set>
#include <map>

#define JTUNABLE(name, args...) \
  jalib::JTunable name(#name, args)

namespace jalib {

typedef int TunableValue;
class JTunable;
typedef std::map<std::string, JTunable*> JTunableReverseMap;

/**
 * A snapshot of the state of JTunables
 */
class JTunableConfiguration : public std::map<JTunable*, TunableValue>, public jalib::JPrintable {
public:
  ///
  /// Make a given configuration active
  void makeActive() const;

  ///
  /// Return distance between two configurations
  double distanceTo(const JTunableConfiguration& that) const;


  void print(std::ostream& o) const;
};

class JConfigurationTester {
public:
  virtual ~JConfigurationTester(){}

  ///
  /// Test performance of given configuration
  virtual double test(const JTunableConfiguration& cfg) = 0;
};

/**
 * Keeps pointers to all tunables
 */
class JTunableManager : public std::set<JTunable*> {
public:
  ///
  /// Master manager that contains all possible tunables
  static JTunableManager& instance();

  ///
  /// Create a string->Tunable mapping
  JTunableReverseMap getReverseMap() const;

  ///
  /// Get the current state of all tunables
  JTunableConfiguration getCurrentConfiguration() const;

  ///
  /// Load a configuration from disk
  void load(const std::string& filename) const;

  ///
  /// Save a configuration to disk
  void save(const std::string& filename) const;

  ///
  /// Use simulated annealing to find an good configuration
  void autotune(JConfigurationTester* tester) const;

  void reset() const;
};

/**
 * A parameter that can be tuned at runtime
 */
class JTunable{
public:
  ///
  /// Constructor
  JTunable( const char* name
          , TunableValue initial
          , TunableValue min=std::numeric_limits<TunableValue>::min()
          , TunableValue max=std::numeric_limits<TunableValue>::max())
    : _name(name), _value(initial), _initial(initial), _min(min), _max(max), _isPegged(false)
  {
    JTunableManager::instance().insert(this);
  }

  ///
  /// Destructor
  ~JTunable() {
    JTunableManager::instance().erase(this);
  }

  //set/get _value
  operator TunableValue () const { return _value; }
  TunableValue value() const { return _value; }
  void setValue(TunableValue v) { _value=v; }

  //set/get _isPegged
  bool isPegged() const { return _isPegged; }
  void setPegged(bool v) { _isPegged=v; }

  const std::string& name() const { return _name; }
  TunableValue min() const { return _min; }
  TunableValue max() const { return _max; }

  void verify() {
    JWARNING(_value>=_min && _value<=_max)(_name)(_value)(_min)(_max)
      .Text("invalid tunable value");
    if(! (_value>=_min && _value<=_max))
      _value=_initial;
  }

  TunableValue rangeLength() const { return _max-_min+1; }

  void reset() { _value = _initial; };
private:
  std::string  _name;
  TunableValue _value;
  TunableValue _initial;
  TunableValue _min;
  TunableValue _max;
  bool _isPegged;
};



}

#endif
