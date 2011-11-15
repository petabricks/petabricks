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

/**  USAGE EXAMPLE:
 *
 * int a=1,b=2,c=3,d=4;
 *
 * code:
 *     JASSERT(a==b)(a)(b)(c).Text("Error a!=b program will exit");
 * outputs:
 *     ERROR at main.cpp:15 in main
 *     Reason: JASSERT(a=b) failed
 *       a = 1
 *       b = 2
 *       c = 3
 *     Message: Error a!=b program will exit
 *     Terminating...
 *
 *
 * code:
 *     JWARNING(a==b)(a)(b)(d).Text("Warning a!=b program will continue");
 * outputs:
 *     WARNING at main.cpp:15 in main
 *     Reason: JWARNING(a=b) failed
 *       a = 1
 *       b = 2
 *       d = 4
 *     Message: Warning a!=b program will continue
 *
 *
 * code:
 *     JNOTE("Values of abcd (in the form 'a=1') will be printed below this text.")(a)(b)(c)(d);
 * outputs:
 *     JNOTE at main.cpp:15 in main
 *     Reason: Values of abcd (in the form 'a=1') will be printed below this text.
 *       a = 1
 *       b = 2
 *       c = 3
 *       d = 4
 *
 *
 * It has the ability to output any variable understood by std::ostream.
 *
 */

#ifndef JASSERT_H
#define JASSERT_H

#include <errno.h>
#include <iostream>
#include <sstream>
#include <string.h>
#include <unistd.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

namespace jalib{
  class SrcPosTaggable;

  class JAssert{
    public:
      typedef void (*CallbackT)(JAssert&);
      static int onBegin(CallbackT fn);

      ///
      /// print a value of any type
      template < typename T > JAssert& Print ( const T& t );
      ///
      /// print out a string in format "Message: msg"
      JAssert& Text ( const char* msg );
      ///
      /// print out a string in format "Message: msg"
      JAssert& Text ( const std::string& msg ){ return Text(msg.c_str()); }
      ///
      /// constructor: sets members
      JAssert ( bool exitWhenDone );
      ///
      /// destructor: exits program if exitWhenDone is set
      ~JAssert();
      ///
      /// termination point for crazy macros
      JAssert& JASSERT_CONT_A;
      ///
      /// termination point for crazy macros
      JAssert& JASSERT_CONT_B;

      template < typename T > JAssert& operator << ( const T& t )
      { Print ( t ); return *this; }

      JAssert& SetContext( const char* type
                         , const char* reason
                         , const char* file
                         , const char* line
                         , const char* func
                         , const jalib::SrcPosTaggable* srcpos);

      JAssert& VarName(const char* name);
      JAssert& Prefix();
      JAssert& EndLine(){ return Print('\n'); }

      bool IsFatal() const { return _exitWhenDone; }
    private:
      ///
      /// if set true (on construction) call exit() on destruction
      bool _exitWhenDone;

      long _id;
  };

  const char* jassert_basename ( const char* str );
  std::ostream& jassert_output_stream();
  void jassert_safe_print ( const char* );
  void set_log_file ( const std::string& path );
  int jassert_console_fd();

  template < typename T >
  inline JAssert& JAssert::Print ( const T& t )
  {
#ifdef JASSERT_FAST
    jassert_output_stream() << t;
#else
    std::ostringstream ss;
    ss << t;
    jassert_safe_print ( ss.str().c_str() );
#endif
    return *this;
  }

#ifndef JASSERT_FAST
  template <>
  inline JAssert& JAssert::Print( const std::string& t ){
    jassert_safe_print ( t.c_str() );
    return *this;
  }

  template <>
  inline JAssert& JAssert::Print( const char* const& t ){
    jassert_safe_print( t );
    return *this;
  }
#endif


}//jalib
  
#define USE(x) (void)(x)

//helpers:
#define JASSERT_ERRNO          (strerror(errno))
#define JASSERT_INIT()         JASSERT_PRINT("")
#define JASSERT_PRINT(str)     jalib::JAssert(false).Print(str)
#define JASSERT_SET_LOGFILE(p) jalib::set_log_file(p)
#define JASSERT_STDERR_FD      jalib::jassert_console_fd()
#define JASSERT_STDERR         jalib::JAssert(false)
#define JASSERT_CAT(a,b)       a ## b
#define JASSERT_STRINGIFY(x)   JASSERT_STRINGIFY_(x)
#define JASSERT_STRINGIFY_(x)  #x

//detecting context 
#define JASSERT_FUNC __FUNCTION__
#define JASSERT_LINE JASSERT_STRINGIFY(__LINE__)
#define JASSERT_FILE __FILE__
#ifdef JASSERT_USE_SRCPOS
# define JASSERT_SRCPOS (_lexicalSrcPos())
#else
# define JASSERT_SRCPOS NULL
#endif
#define JASSERT_CONTEXT(type,reason) SetContext(type, reason, JASSERT_FILE, JASSERT_LINE, JASSERT_FUNC, JASSERT_SRCPOS)

//glue for variable printing
#define JASSERT_CONT(AB,term) VarName(#term).Print(term).EndLine().JASSERT_CONT_##AB
#define JASSERT_CONT_A(term) JASSERT_CONT(B,term)
#define JASSERT_CONT_B(term) JASSERT_CONT(A,term)

//actual macros follow
#define JASSERT_NOP if(true){}else jalib::JAssert(false).JASSERT_CONT_A

#ifdef DEBUG
#define JTRACE(msg) jalib::JAssert(false).JASSERT_CONTEXT("TRACE",msg).JASSERT_CONT_A
#define JDEBUGWARNING JWARNING
#define JDEBUGASSER JASSERT
#else
#define JTRACE(msg) JASSERT_NOP
#define JDEBUGWARNING(term) JASSERT_NOP
#define JDEBUGASSERT(term) JASSERT_NOP
#endif

#define JNOTE(msg) \
    jalib::JAssert(false).JASSERT_CONTEXT("NOTE",msg).JASSERT_CONT_A
#define JWARNING(term) if((term)){}else \
    jalib::JAssert(false).JASSERT_CONTEXT("WARNING","JWARNING(" #term ") failed").JASSERT_CONT_A
#define JASSERT(term)  if((term)){}else \
    jalib::JAssert(true).JASSERT_CONTEXT("ERROR","JASSERT(" #term ") failed").JASSERT_CONT_A

#ifdef UNSAFE
#undef  JWARNING
#define JWARNING(t) if((t)) JASSERT_NOP
#undef  JASSERT
#define JASSERT(t)  if((t)) JASSERT_NOP
#undef  JNOTE
#define JNOTE(m)  JASSERT_NOP
#endif

#define UNIMPLEMENTED() JASSERT(false).Text("Unimplemented");

#define JASSERT_STATIC(term) extern char JASSERT_CAT(_jassert_static_,__LINE__) [ 1 - 2*( (term)==0 ) ]

#ifdef JASSERT_USE_SRCPOS
#include "srcpos.h"
#endif

#endif

