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
#include "maximawrapper.h"

#include "common/jassert.h"
#include "common/jsocket.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

static int forkopen(void (*fn)()){
  int fds[2];
  JASSERT(socketpair(AF_UNIX, SOCK_STREAM, 0, fds)==0);
  pid_t pid = fork();
  JASSERT(pid>=0);
  if(pid>0){
    //parent
    close(fds[1]);
    return fds[0];
  }else{
    //child
    JASSERT(dup2(fds[1], 0)==0);
    JASSERT(dup2(fds[1], 1)==1);
    close(fds[0]);
    close(fds[1]);
    (*fn)();
    JASSERT(false);
    return -1;
  }
}

static void launchMaxima(){
  const char* args[]= {MAXIMA_PATH,"-q","--disable-readline", NULL};
  execvp(args[0],(char**)args);
  JASSERT(false)(JASSERT_ERRNO).Text("exec(maxima) failed");
}
#ifdef MAXIMA_LOG
static void launchMaximaWithLogging(){
  class LogForwarder : public jalib::JMultiSocketProgram {
  public:
    void onConnect ( const jalib::JSocket& /*sock*/, const struct sockaddr* /*remoteAddr*/, socklen_t /*remoteLen*/ ){JASSERT(false);}
    void onDisconnect ( jalib::JReaderInterface* /*sock*/ ) { exit(0); };

    void onData ( jalib::JReaderInterface* sock ){
      static int stdinfd  = fileno(stdin);
      static int stdoutfd = fileno(stdout);
      char buf = *sock->buffer();
      log(buf);
      if(sock->socket().sockfd() == stdinfd)
        JASSERT(write(maximaFd, &buf, 1)==1);
      else
        JASSERT(write(stdoutfd, &buf, 1)==1);
    }

    void log(char c){
      static FILE* fd = fopen("maxima.log","w");
      JASSERT(fwrite(&c, 1,1, fd) == 1);
      fflush(fd);
    }

    int maximaFd;
  } forwarder;

  forwarder.maximaFd = forkopen(launchMaxima);
  jalib::JChunkReader maxima(forwarder.maximaFd, 1);
  jalib::JChunkReader petabricks(fileno(stdin),      1);
  forwarder.addDataSocket(&maxima);
  forwarder.addDataSocket(&petabricks);
  forwarder.monitorSockets();
  JASSERT(false);
}
#endif

static const char * const theInitCode = 
  "display2d : false $ "
  "prederror : false $ "
  "load(ineq) $ "
  "1"
;

extern FILE* maximain;
extern petabricks::FormulaListPtr readFormulaFromMaxima();

petabricks::MaximaWrapper& petabricks::MaximaWrapper::instance(){
  static MaximaWrapper inst;
  return inst;
}

petabricks::MaximaWrapper::MaximaWrapper()
  : _fd(-1)
  , _stackDepth(0)
{
#ifdef MAXIMA_LOG
  _fd = forkopen(&launchMaximaWithLogging);
#else
  _fd = forkopen(&launchMaxima);
#endif
  maximain = fdopen(_fd, "rw");
  readFormulaFromMaxima();//initial prompt
#ifdef DEBUG
  sanityCheck();
#endif
  runCommand(theInitCode);
#ifdef DEBUG
  sanityCheck();
#endif
}

petabricks::MaximaWrapper::~MaximaWrapper()
{
  maximain=NULL;
  close(_fd);
}

petabricks::FormulaListPtr petabricks::MaximaWrapper::runCommandRaw(const char* cmd, int len){
  static const char endCommand[] = ";\n";
  JASSERT(write(_fd, cmd, len)==len)(cmd)(JASSERT_ERRNO);
  JASSERT(write(_fd, endCommand, sizeof endCommand-1)==sizeof endCommand-1)(cmd)(JASSERT_ERRNO);
  fsync(_fd);
  petabricks::FormulaListPtr result = readFormulaFromMaxima();
  return result;
}

//run command and perform simple caching
petabricks::FormulaListPtr petabricks::MaximaWrapper::runCommand(const std::string& cmd){
  if(!_pendingAssume.empty()) {
    //we delay assume() commands since they often aren't followed by anything
    ContextT ctx;
    ctx.swap(_pendingAssume);
    for(ContextT::const_iterator i=ctx.begin(); i!=ctx.end(); ++i) {
      runCommandRaw(i->c_str(), i->length());
    }
    clearCache();
  }

  CacheT::const_iterator i = _cache.find(cmd);
  if(i != _cache.end()) {
    return i->second;
  }
  FormulaListPtr rv = runCommandRaw(cmd.c_str(), cmd.length());
#ifdef MAXIMA_CACHING
  _cache[cmd]=rv;
#endif
  return rv;
}


