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

#include "jassert.h"
#include "jconvert.h"
#include "jasm.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_EXECINFO_H
# include <execinfo.h>
#else 
# undef HAVE_BACKTRACE
# undef HAVE_BACKTRACE_SYMBOLS
# undef HAVE_BACKTRACE_SYMBOLS_FD
#endif

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fstream>

#undef JASSERT_CONT_A
#undef JASSERT_CONT_B


#if defined(HAVE_CXXABI_H) && defined(HAVE_BACKTRACE_SYMBOLS)
#include <cxxabi.h>

static std::string _cxxdemangle(const char* str){
  int     status;
  const char* b;
  const char* e;
  char   *tmp;

  //find start and end of where to expand
  b=str;
  while(*b!='\0' && *b!='(') ++b;
  e=b;
  while(*e!='\0' && *e!=')' && *e!='+') ++e;

  if(*b=='('){
    ++b;
    std::string mid(b, e);
    tmp = abi::__cxa_demangle(mid.c_str(), 0, 0, &status);
    if(tmp!=NULL){
      //rebuild the string
      mid=tmp;
      free(tmp);
      std::string left(str, b);
      std::string right(e);
      return left+mid+right;
    }else{
      //demangle failed, do nothing
      return str; 
    }
  }
  return str; 
}

#else
static std::string _cxxdemangle(const std::string& str){ return str; }
#endif

/* 
   When updating value of DUP_STDERR_FD, the same value should be updated 
   in mtcp_printf.c. The two consts must always in sync.
*/
static const int DUP_STDERR_FD = 826;
static const int DUP_LOG_FD    = 827;

int jassert_internal::jassert_console_fd()
{
  //make sure stream is open
  jassert_safe_print ( "" );
  return DUP_STDERR_FD;
}

jassert_internal::JAssert& jassert_internal::JAssert::Text ( const char* msg )
{
  Print ( "Message: " );
  Print ( msg );
  Print ( "\n" );
  return *this;
}

jassert_internal::JAssert::JAssert ( bool exitWhenDone )
    : JASSERT_CONT_A ( *this )
    , JASSERT_CONT_B ( *this )
    , _exitWhenDone ( exitWhenDone )
{}

jassert_internal::JAssert::~JAssert()
{
  if ( _exitWhenDone )
  {
#ifdef DEBUG
# ifdef HAVE_BACKTRACE_SYMBOLS
    Print( "Stack trace:\n" );
    void *addresses[10];
    int size = backtrace(addresses, 10);
    char **strings = backtrace_symbols(addresses, size);
    for(int i = 0; i-1 < size; i++){
      Print("   "); Print(i); Print(": "); Print(_cxxdemangle(strings[i+1])); Print("\n");
    }
    free(strings);
# endif
    jalib::Breakpoint();
#endif
    Print ( "Terminating...\n" );
    _exit ( 1 );
  }
}

const char* jassert_internal::jassert_basename ( const char* str )
{
  for ( const char* c = str; c[0] != '\0' && c[1] !='\0' ; ++c )
    if ( c[0]=='/' )
      str=c+1;
  return str;
}

// std::ostream& jassert_internal::jassert_output_stream(){
//     return std::cerr;
// }


static FILE* _fopen_log_safe ( const char* filename, int protectedFd )
{
  //open file
  int tfd = open ( filename, O_WRONLY | O_APPEND | O_CREAT /*| O_SYNC*/, S_IRUSR | S_IWUSR );
  if ( tfd < 0 ) return NULL;
  //change fd to 211
  int nfd = dup2 ( tfd, protectedFd );
  close ( tfd );
  if ( nfd < 0 ) return NULL;
  //promote it to a stream
  return fdopen ( nfd,"w" );
}
static FILE* _fopen_log_safe ( const std::string& s, int protectedFd )
{ 
  return _fopen_log_safe ( s.c_str(), protectedFd ); 
}


static FILE* theLogFile = NULL;

static std::string& theLogFilePath() {static std::string s;return s;};

void jassert_internal::set_log_file ( const std::string& path )
{
  theLogFilePath() = path;
  if ( theLogFile != NULL ) fclose ( theLogFile );
  theLogFile = NULL;
  if ( path.length() > 0 )
  {
    theLogFile = _fopen_log_safe ( path, DUP_LOG_FD );
    if ( theLogFile == NULL )
      theLogFile = _fopen_log_safe ( path + "_2",DUP_LOG_FD );
    if ( theLogFile == NULL )
      theLogFile = _fopen_log_safe ( path + "_3",DUP_LOG_FD );
    if ( theLogFile == NULL )
      theLogFile = _fopen_log_safe ( path + "_4",DUP_LOG_FD );
    if ( theLogFile == NULL )
      theLogFile = _fopen_log_safe ( path + "_5",DUP_LOG_FD );
  }
}

static FILE* _initJassertOutputDevices()
{
#ifdef JASSERT_LOG
  JASSERT_SET_LOGFILE ( "/tmp/jassertlog." + jalib::XToString ( getpid() ) );
#endif

  const char* errpath = getenv ( "JALIB_STDERR_PATH" );

  if ( errpath != NULL )
    return _fopen_log_safe ( errpath, DUP_STDERR_FD );
  else
    return fdopen ( dup2 ( fileno ( stderr ),DUP_STDERR_FD ),"w" );;;
}



void jassert_internal::jassert_safe_print ( const char* str )
{
  static FILE* errconsole = _initJassertOutputDevices();

  fprintf ( errconsole,"%s",str );

  if ( theLogFile != NULL )
  {
    int rv = fprintf ( theLogFile,"%s",str );

    if ( rv < 0 )
    {
      fprintf ( errconsole,"JASSERT: write failed, reopening log file.\n" );
      JASSERT_SET_LOGFILE ( theLogFilePath() );
      if ( theLogFile != NULL )
        fprintf ( theLogFile,"JASSERT: write failed, reopened log file.\n%s",str );
    }
    fflush ( theLogFile );
  }

// #ifdef DEBUG
//     static pid_t logPd = -1;
//     static FILE* log = NULL;
//
//     if(logPd != getpid())
//     {
//         if(log != NULL) fclose(log);
//         logPd = getpid();
//         log = _fopen_log_safe(("/tmp/jassertlog." + jalib::XToString(logPd)).c_str());
//     }
//
//     if(log != NULL)
//     {
//         fprintf(log,"%s",str);
//         fflush(log);
//     }
// #endif
}

