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
#ifndef JALIBJTUNABLE_H
#define JALIBJTUNABLE_H

#include "jassert.h"
#include "jconvert.h"
#include "jprintable.h"

#include <limits>
#include <map>
#include <set>
#include <iostream>
#include <vector>

#define JTUNABLE(name, args...) \
  jalib::JTunableInt name(#name, args)

#define JTUNABLESTATIC(name, value) \
  jalib::JTunableIntStatic<value> name(#name)

#define JTUNABLEARRAY(name, n, args...) \
  jalib::JTunableIntArray name(#name, n, args)

#define EXTERNJTUNABLE(name, args...) \
  extern jalib::JTunableInt name

#define EXTERNJTUNABLESTATIC(name, value) \
  extern jalib::JTunableIntStatic<value> name

#define EXTERNJTUNABLEARRAY(name, n, args...) \
  extern jalib::JTunableIntArray name

#define JTUNABLEDOUBLE(name, args...) \
  jalib::JTunableDouble name(#name, args)

#define JTUNABLEDOUBLESTATIC(name, value) \
  jalib::JTunableDoubleStatic<value> name(#name)

#define JTUNABLEDOUBLEARRAY(name, n, args...) \
  jalib::JTunableDoubleArray name(#name, n, args)

#define EXTERNJTUNABLEDOUBLE(name, args...) \
  extern jalib::JTunableDouble name

#define EXTERNJTUNABLEDOUBLESTATIC(name, value) \
  extern jalib::JTunableDoubleStatic<value> name

#define EXTERNJTUNABLEDOUBLEARRAY(name, n, args...) \
  extern jalib::JTunableDoubleArray name


namespace jalib {
class TunableValue {
public:
  TunableValue()         : _type(NONE),   _d(0) {}
  TunableValue(int v)    : _type(INT),    _i(v) {}
  TunableValue(double v) : _type(DOUBLE), _d(v) {}

  int i() const {
#ifdef DEBUG
    JASSERT(_type==INT);
#endif
    return _i;
  }
  double d() const {
#ifdef DEBUG
    JASSERT(_type==DOUBLE);
#endif
    return _d;
  }

  double asD() const {
    if(_type==INT)    return _i;
    if(_type==DOUBLE) return _d;
    UNIMPLEMENTED();
    return 0;
  }
  
  const char* typestr() const {
    if(_type==INT)    return "int";
    if(_type==DOUBLE) return "double";
    return "?";
  }

  bool isInt()    const { return _type == INT; }
  bool isDouble() const { return _type == DOUBLE; }

  friend std::ostream& operator<<(std::ostream& o, const TunableValue& v) {
    if(v._type==NONE)
      return o << "?";
    if(v._type==INT)
      return o << v._i;
    ssize_t oprec = o.precision(35);
    o << v._d;
    o.precision(oprec);
    return o;
  }
  friend bool operator< (const TunableValue& a, const TunableValue& b) { return a.asD() <  b.asD(); } 
  friend bool operator<=(const TunableValue& a, const TunableValue& b) { return a.asD() <= b.asD(); } 
  friend bool operator> (const TunableValue& a, const TunableValue& b) { return a.asD() >  b.asD(); } 
  friend bool operator>=(const TunableValue& a, const TunableValue& b) { return a.asD() >= b.asD(); } 
  friend bool operator!=(const TunableValue& a, const TunableValue& b) { return ! operator==(a,b); } 
  friend bool operator==(const TunableValue& a, const TunableValue& b) {
    if(a._type==NONE && b._type==NONE)
      return true;
    if(a._type==NONE || b._type==NONE)
      return false;
    return a.asD() == b.asD();
  } 

  static TunableValue min(const TunableValue& a, const TunableValue& b) {
    if(a._type==NONE) return b;
    if(b._type==NONE) return a;
    return std::min(a,b);
  }
  static TunableValue max(const TunableValue& a, const TunableValue& b) {
    if(a._type==NONE) return b;
    if(b._type==NONE) return a;
    return std::max(a,b);
  }

private:
  enum TypeT { NONE, INT, DOUBLE };
  TypeT _type;
  union {
    int    _i;
    double _d;
  };
};

class JTunable;
typedef std::map<std::string, JTunable*> JTunableReverseMap;
typedef std::map<std::string, TunableValue> TunableValueMap;

class JTunableModificationMonitor {
public:
  virtual void onTunableModification(JTunable* tunable, TunableValue oldVal, TunableValue newVal) = 0;
  virtual ~JTunableModificationMonitor(){}
};

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

  void addStaticTunable(const char* /*name*/, TunableValue /*val*/){}
  
  ///
  /// Load a configuration from disk, just do the parsing part 
  static TunableValueMap loadRaw(const std::string& filename);
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
          , TunableValue min
          , TunableValue max=jalib::maxval<TunableValue>())
    : _name(name), _value(initial), _initial(initial), _min(min), _max(max)
  {
    JTunableManager::instance().insert(this);
  }

  ///
  /// Constructor
  JTunable( const JTunable& that)
    : _name(that._name), _value(that._value), _initial(that._initial), _min(that._min), _max(that._max)
  {
    JTunableManager::instance().insert(this);
  }

  ///
  /// Destructor
  ~JTunable() {
    JTunableManager::instance().erase(this);
  }

  TunableValue value() const { return _value; }
  void setValue(TunableValue v) { 
    if(theModCallback == NULL){
      _value = v; 
    }else{
      std::swap(_value, v);
      theModCallback->onTunableModification(this, v, _value);
    }
  }
  
  const std::string& name() const { return _name; }
  TunableValue min() const { return _min; }
  TunableValue max() const { return _max; }

  void verify() {
    JWARNING(_value>=_min && _value<=_max)(_name)(_value)(_min)(_max)
      .Text("invalid tunable value");
    if(! (_value>=_min && _value<=_max))
      _value=_initial;
  }

  //TunableValue rangeLength() const { return _max - _min + 1; }

  void reset() { setValue(_initial); };

  static void setModificationCallback(JTunableModificationMonitor* m = NULL){
    theModCallback = m;
  }
protected:
  std::string  _name;
  TunableValue _value;
  TunableValue _initial;
  TunableValue _min;
  TunableValue _max;
  static JTunableModificationMonitor* theModCallback;
};

class JTunableInt : public JTunable {
public:
  ///
  /// Constructor
  JTunableInt( const char* name
          , int initial
          , int min
          , int max=jalib::maxval<int>())
    : JTunable(name, initial, min, max)
  {}

  //set/get _value
  operator int() const { return _value.i(); }

};


class JTunableDouble: public JTunable {
public:
  ///
  /// Constructor
  JTunableDouble( const char* name
          , double initial
          , double min
          , double max=jalib::maxval<double>())
    : JTunable(name, initial, min, max)
  {}

  //set/get _value
  operator double() const { return _value.d(); }

};


//statically set tunable value
template < int _value > 
class JTunableIntStatic {
public:
  JTunableIntStatic( const char* name ){
    JTunableManager::instance().addStaticTunable(name, _value);
  }

  //set/get _value
  inline operator int () const { return _value; }
  inline TunableValue value() const { return _value; }
  void setValue(TunableValue v) { JWARNING(v==_value)(v)(_value).Text("Can't change static JTunable"); }
  
  const std::string& name() const {
    static std::string t = "__static_tunable";
    return t;
  }
  void verify() {}
  void reset() {}
};

//statically set tunable value
class JTunableDoubleStatic {
public:
  JTunableDoubleStatic( const char* name, double value) : _value(value) {
    JTunableManager::instance().addStaticTunable(name, value);
  }

  //set/get _value
  inline operator double () const { return _value; }
  inline TunableValue value() const { return _value; }
  void setValue(TunableValue v) { JWARNING(v==_value)(v)(_value).Text("Can't change static JTunable"); }
  
  const std::string& name() const {
    static std::string t = "__static_tunable";
    return t;
  }
  void verify() {}
  void reset() {}
private:
  double _value;
};


class JTunableIntArray : public std::vector<JTunableInt> {
public: 
  JTunableIntArray( const char* name
               , int n
               , int initial
               , int min=0
               , int max=jalib::maxval<int>())
  {
    JASSERT(n>0)(n);
    reserve(n);
    for(int i=0; i<n; ++i)
      push_back(JTunableInt((std::string(name)+"__"+XToString(i)).c_str(), initial, min, max));
  }
};

class JTunableDoubleArray : public std::vector<JTunableDouble> {
public: 
  JTunableDoubleArray( const char* name
               , int n
               , double initial
               , double min=0
               , double max=jalib::maxval<double>())
  {
    JASSERT(n>0)(n);
    reserve(n);
    for(int i=0; i<n; ++i)
      push_back(JTunableDouble((std::string(name)+"__"+XToString(i)).c_str(), initial, min, max));
  }
};

}

#endif

