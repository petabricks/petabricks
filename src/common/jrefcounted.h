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
#ifndef JALIBJREFCOUNTED_H
#define JALIBJREFCOUNTED_H

#include "jprintable.h" //need to compile under ICC
#include "jasm.h"

#include <vector>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

namespace jalib {

void _JRefAbort(const char* msg);

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

  T* asPtr() const { use(); return _obj; }

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
  void check() const { if(_obj==NULL) _JRefAbort("Would have dereferenced null pointer."); }
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
#ifdef DEBUG
    if(_refCount<=0) _JRefAbort("negative ref count");
#endif
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

