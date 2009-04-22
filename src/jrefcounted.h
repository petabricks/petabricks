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
#ifndef JALIBJREFCOUNTED_H
#define JALIBJREFCOUNTED_H

#include "jassert.h"
#include "jprintable.h" //need to compile under ICC
#include "jasm.h"

#include <vector>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

namespace jalib {

/**
 * Basic policy for JRef, normal ref counted objects
 */
template < typename T > struct JRefPolicyShared{
  static void inc(T* o){ if(o!=NULL) o->incRefCount(); }
  static void dec(T* o){ if(o!=NULL) o->decRefCount(); }
  static void use(T*){}
};

/**
 * Basic policy for JRef, copy on write objects
 */
template < typename T > struct JRefPolicyCopied{
  static void inc(T* o){ if(o!=NULL) o->incRefCount(); }
  static void dec(T* o){ if(o!=NULL) o->decRefCount(); }
  static void use(T*& o){
    if(o!=NULL && o->refCount()>1){
      T* t=o->clone();
      inc(t);
      dec(o);
      o=t;
    }
  }
};

/**
 * Policy that does no reference counting 
 */
template < typename T > struct JRefPolicyLeaked {
  static void inc(T* o){}
  static void dec(T* o){}
  static void use(T*){}
};

/**
 * Reference to a JRefCounted
 */
template < typename T, typename Policy = JRefPolicyShared<T> >
class JRef{
public:
  static const JRef& null() { static JRef t; return t; }

  //constructors
  JRef(T* o = NULL)   : _obj(o) { inc(); }
  JRef(const JRef& p) : _obj(p._obj) { inc(); }

  //destructors
  ~JRef() { dec(); }

  //assignment
  JRef& operator= (const JRef& p){
    p.inc(); //must go first for self assignment
    dec();
    _obj = p._obj;
    return *this;
  }

  //accessor
  T* operator->() const {
    check();
    use();
    return _obj;
  }

  //accessor
  T& operator*()  const {
    check();
    use();
    return *_obj;
  }

   T* asPtr()             { use(); return _obj; }
   const T* asPtr() const { use(); return _obj; }

  //is valid?
  operator bool() const {
    return _obj!=NULL;
  }

  operator const T& () const { check(); use(); return *_obj; }
  operator T& ()             { check(); use(); return *_obj; }

  //compare
  friend bool operator == (const JRef& a, const JRef& b) { return a._obj == b._obj; }
  friend bool operator != (const JRef& a, const JRef& b) { return a._obj != b._obj; }
  friend bool operator <= (const JRef& a, const JRef& b) { return a._obj <= b._obj; }
  friend bool operator >= (const JRef& a, const JRef& b) { return a._obj >= b._obj; }
  friend bool operator <  (const JRef& a, const JRef& b) { return a._obj <  b._obj; }
  friend bool operator >  (const JRef& a, const JRef& b) { return a._obj >  b._obj; }

private: //helpers:
  void inc() const { Policy::inc(_obj); }
  void dec() const { Policy::dec(_obj); }
  void use() const { Policy::use(_obj); }
#ifdef DEBUG
  void check() const { JASSERT(_obj!=NULL).Text("Would have dereferenced null pointer."); }
#else
  void check() const {}
#endif
private:
  mutable T* _obj;
};

/**
 * Base class for ref counted objects
 */
class JRefCounted{
protected:
  JRefCounted()                   : _refCount(0) {}
  JRefCounted(const JRefCounted&) : _refCount(0) {}
  virtual ~JRefCounted(){}
public:
  inline void incRefCount() const{
    atomicIncrement(&_refCount);
  }
  inline void decRefCount() const{
    if(atomicDecrementReturn(&_refCount)==0)
      delete this;
  }
  inline long refCount() const{
    return _refCount;
  }
  inline void incRefCountUnsafe() const {
    ++_refCount;
  }
private:
  //PADDING(56);
  mutable jalib::AtomicT _refCount;
  //PADDING(64-sizeof(jalib::AtomicT));
};

/**
 * A pool of references
 */
class JRefPool {
public:
  template < typename T >  T* add(T* t) {
    _pool.push_back(JRef<JRefCounted>(t));
    return t;
  }
  void clear() { return _pool.clear(); }
private:
  std::vector< JRef< JRefCounted > > _pool;
};

}

template < typename T, typename P >
std::ostream& operator<< (std::ostream& o, const jalib::JRef<T, P>& ptr){
  if(ptr.operator bool()){
    const T& t = ptr.operator *();
    o << t;
  }else{
    o << "(null)";
  }
  return o;
}

#endif

