/***************************************************************************
 *   Copyright (C) 2006 by Jason Ansel                                     *
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
  static JTime now() {return JTime();}
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
  JTime();
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
