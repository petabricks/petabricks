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
#include "jtimer.h"
#include "jassert.h"
#include "jfilesystem.h"

#include <iostream>
#include <fstream>

jalib::JTime jalib::JTime::now() {
  JTime t;
#ifdef USE_GETTIMEOFDAY
  JASSERT ( gettimeofday ( &t._value, NULL ) == 0 );
#else
  JASSERT ( clock_gettime ( CLOCK_MONOTONIC, &t._value ) == 0 );
#endif
  return t;
}

jalib::JTime jalib::JTime::resolution() {
  JTime t;
#ifdef USE_GETTIMEOFDAY
  t._value.tv_sec=0;
  t._value.tv_usec=1;
#else
  JASSERT ( clock_getres( CLOCK_MONOTONIC, &t._value ) == 0 );
#endif
  return t;
}

void jalib::JTime::print(std::ostream& os) const {
  char buf[128];
  snprintf(buf, sizeof buf, "%ld.%09ld", (long)sec(), (long)nsec());
  os << buf;
}

double jalib::operator- ( const jalib::JTime& a, const jalib::JTime& b )
{
  double sec = a._value.tv_sec - b._value.tv_sec;
  sec += ( a.nsec() - b.nsec() ) / (double)1e9;
  if ( sec < 0 ) sec *= -1;
  return sec;
}

jalib::JTimeRecorder::JTimeRecorder ( const std::string& name )
    : _name ( name )
    , _isStarted ( false )
    , _start(JTime::null())
{}

namespace
{
  static const std::string& _testName()
  {
    static const char* env = getenv ( "TESTNAME" );
    static std::string tn = jalib::Filesystem::GetProgramName()
                            + jalib::XToString ( getpid() )
                            + ',' + std::string ( env == NULL ? "unamedtest" : env );
    return tn;
  }

  static void _writeTimerLogLine ( const std::string& name, double time )
  {
    static std::ofstream logfile ( "jtimings.csv", std::ios::out | std::ios::app );
    logfile << _testName() <<  ',' << name << ',' << time << std::endl;
    JASSERT_STDERR << "JTIMER(" <<  name << ") : " << time << '\n';
  }
}

void jalib::JTimeRecorder::recordTime ( double time )
{
  _writeTimerLogLine ( _name,time );
}
