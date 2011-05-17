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
#ifndef JTIMER_H
#define JTIMER_H


#include "jconvert.h"
#include "jassert.h"
#include "jprintable.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#if defined(USE_GETTIMEOFDAY) && defined(HAVE_SYS_TIME_H)
# include <sys/time.h>
#endif

#include <time.h>

#ifdef TIMING
#define JTIMER(name) static jalib::JTimeRecorder _jtimer_ ## name (#name);
#define JTIMER_START(name) ( _jtimer_ ## name . start() )
#define JTIMER_STOP(name) ( _jtimer_ ## name . stop() )
#define JTIMER_SCOPE(name) static jalib::JTimeRecorder _jtimer_scope_tm_ ## name (#name); \
       jalib::JScopeTimer _jtimer_scope_inst_ ## name (_jtimer_scope_tm_ ## name);
#else
#define JTIMER(name)
#define JTIMER_START(name)
#define JTIMER_STOP(name)
#define JTIMER_SCOPE(name)
#endif


namespace jalib
{

class JTime;
double operator- ( const JTime& a, const JTime& b );

class JTime : public JPrintable
{
public:
  friend double operator- ( const JTime& a, const JTime& b );
  static JTime now();
  static JTime resolution();
  static JTime null() {return JTime();}

  void print(std::ostream& os) const;
  
  long sec() const { return _value.tv_sec; }
  long usec() const {
#ifdef USE_GETTIMEOFDAY
    return _value.tv_usec;
#else
    return _value.tv_nsec / 1e3;
#endif
  }
  long nsec() const {
#ifdef USE_GETTIMEOFDAY
    return _value.tv_usec * 1e3;
#else
    return _value.tv_nsec;
#endif
  }
protected:
  JTime(){}
private:
#ifdef USE_GETTIMEOFDAY
  struct timeval _value;
#else
  struct timespec _value;
#endif
};

class JTimeRecorder
{
  public:
    JTimeRecorder ( const std::string& name );
    void start()
    {
      JWARNING ( !_isStarted ) ( _name );
      _start = JTime::now();
      _isStarted = true;
    }
    void stop()
    {
      JWARNING ( _isStarted ) ( _name );
      if ( !_isStarted ) return;
      _isStarted = false;
      recordTime ( JTime::now() - _start );
    }
  protected:
    void recordTime ( double time );
  private:
    std::string _name;
    bool  _isStarted;
    JTime _start;
};

class JScopeTimer
{
  public:
    JScopeTimer ( JTimeRecorder& tm ) :_tm ( tm ) { _tm.start(); }
    ~JScopeTimer() { _tm.stop(); }
  private:
    JTimeRecorder& _tm;
};

}
#endif
