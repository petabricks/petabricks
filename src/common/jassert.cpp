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

#include "jassert.h"
#include "jconvert.h"
#include "jasm.h"

#undef JASSERT_CONT_A
#undef JASSERT_CONT_B

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

#include <fcntl.h>
#include <fstream>
#include <pthread.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>


#if defined(HAVE_CXXABI_H) && defined(HAVE_BACKTRACE_SYMBOLS) && defined(DEBUG)
#include <cxxabi.h>
static const char* _cxxdemangle(const char* i){
  static char buf[1024];
  memset(buf, 0, sizeof(buf));
  const char* end = buf+sizeof(buf)-1;
  char* o = buf;
  char* start = NULL;

  while(*i!=0 && o<end){
    if( (*o++=*i++) == '(' ){
      start = o;   
      break;
    }
  }
  while(*i!=0 && o<end){
    if( *i == ')' || *i == '+' ){
      int status;
      char* tmp = abi::__cxa_demangle(start, 0, 0, &status);
      if(tmp!=NULL){
        o=start;
        for(const char* t=tmp; *t!=0 && o<end;)
          *o++=*t++;  
        memset(o, 0, end-o);
        free(tmp);
      }
      break;
    }
    *o++=*i++;
  }
  while(*i!=0 && o<end){
    *o++=*i++;
  }
  return buf; 
}
#else
#define _cxxdemangle(x) x
#endif

static pthread_mutex_t theMutex = PTHREAD_MUTEX_INITIALIZER;
static jalib::JAssert::CallbackT theBeginCallback = 0;

int jalib::JAssert::onBegin(CallbackT fn){
  theBeginCallback=fn;
  return 0;
}

jalib::JAssert& jalib::JAssert::Text ( const char* msg )
{
  Prefix() << "Message: " << msg;
  EndLine();
  return *this;
}

jalib::JAssert::JAssert ( bool exitWhenDone )
    : JASSERT_CONT_A ( *this )
    , JASSERT_CONT_B ( *this )
    , _exitWhenDone ( exitWhenDone )
{
  pthread_mutex_lock(&theMutex);
  static jalib::AtomicT next=-1;
  _id=jalib::atomicIncrementReturn(&next);
}

jalib::JAssert& jalib::JAssert::SetContext( 
    const char* type,
    const char* reason,
    const char* file,
    const char* line,
    const char* func,
    const jalib::SrcPosTaggable* srcpos)
{
  
#if defined(DEBUG) && defined(HAVE_BACKTRACE_SYMBOLS)
#define MAX_BT_LEN 10
  if ( _exitWhenDone )
  {
    void *addresses[MAX_BT_LEN+1];
    int size = backtrace(addresses, MAX_BT_LEN+1);
    char **strings = backtrace_symbols(addresses, size);
    if(strings!=NULL){
      Prefix() << "Stack trace:";
      EndLine();

      for(int i = 1; i < size; i++){
        if(i<MAX_BT_LEN){
          Prefix() << " " << i << ": " << _cxxdemangle(strings[i]);
          EndLine();
        }else{
          Prefix() << "  ...";
          EndLine();
          break;
        }
      }
      free(strings);
      Prefix();
      EndLine();
    }
  }
#endif

  if(theBeginCallback!=0){
    (*theBeginCallback)(*this);
  }

  Prefix() << "In function " << func << " at " << jassert_basename(file) << ':' << line;
  EndLine();

#ifdef JASSERT_USE_SRCPOS
  if(srcpos!=0){
    Prefix() << "Source " << srcpos->srcPos();
    EndLine();
  }
#endif

 //if(errno!=0){
 //  Prefix() << "errno " << errno << ": " << JASSERT_ERRNO;
 //  EndLine();
 //  errno=0;
 //}

  Prefix() << type << ": " << reason;
  EndLine();
  return *this;
}

jalib::JAssert& jalib::JAssert::Prefix(){
  return *this << '[' << getpid() << '-' << _id << "] ";
}

jalib::JAssert& jalib::JAssert::VarName(const char* n){
  return Prefix() << " " << n << " = ";
}

void jalib::JAssert::dtorExit()
{
  Prefix() << "Terminating...";
  EndLine();
#ifdef DEBUG
  jalib::Breakpoint();
#endif
  _exit ( 1 );
}

void jalib::JAssert::dtorUnlock()
{
  pthread_mutex_unlock(&theMutex);
}

const char* jalib::jassert_basename ( const char* str )
{
  for ( const char* c = str; c[0] != '\0' && c[1] !='\0' ; ++c )
    if ( c[0]=='/' )
      str=c+1;
  return str;
}

#ifndef JASSERT_FAST
static const int DUP_STDERR_FD = 826;
static const int DUP_LOG_FD    = 827;
static FILE* theLogFile = NULL;

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

static std::string& theLogFilePath() {static std::string s;return s;};

void jalib::set_log_file ( const std::string& path )
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

int jalib::jassert_console_fd()
{
  //make sure stream is open
  jassert_safe_print ( "" );
  return DUP_STDERR_FD;
}

void jalib::jassert_safe_print ( const char* str )
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

}
#else
# ifdef JASSERT_LOG
//JASSERT_FAST conflicts with JASSERT_LOG
JASSERT_STATIC(false);
# endif

std::ostream& jalib::jassert_output_stream(){
  static const char* errpath = getenv ( "JALIB_STDERR_PATH" );
  if(errpath !=0){
    static std::ofstream output(errpath);
    if(output.is_open())
      return output;
  }
  return std::cerr;
}
#endif


