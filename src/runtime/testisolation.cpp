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
#include "testisolation.h"
#include "dynamicscheduler.h"
#include "gpumanager.h"

#include <limits>
#include <string.h>

#include "petabricksruntime.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_POLL_H
#include <poll.h>
#endif 
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif 
#ifdef HAVE_SYS_PRCTL_H
#include <sys/prctl.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

// annoyingly lapack returns 0 when it aborts, so we use a "secret" rv here
#define SUCCESS_RV 198
#define RUNNING_RV -257
#define CRASH_RV 666

static void _settestprocflags(){
#ifdef HAVE_SYS_PRCTL_H
# ifdef PR_SET_PDEATHSIG //linux 2.1.57
  //automatically kill us when parent dies
  JWARNING(prctl(PR_SET_PDEATHSIG, SIGTERM, 0, 0, 0)==0);
# endif
# ifdef PR_GET_NAME //linux 2.6.11 (PR_SET_NAME 2.6.9)
  //append "(timing)" to our process name
  char buf[128];//spec says 16 is big enough
  memset(buf, 0, sizeof buf);
  prctl(PR_GET_NAME, buf, 0, 0, 0);
  strncpy(buf+strlen(buf), "(timing)", sizeof(buf)-strlen(buf)-1);
  prctl(PR_SET_NAME, buf, 0, 0, 0);
# endif
#endif
}

//keep these 1 char long: (all must be same size)
static const char COOKIE_DONE[] = "D";
static const char COOKIE_DISABLETIMEOUT[] = "E";
static const char COOKIE_RESTARTTIMEOUT[] = "R";
JASSERT_STATIC(sizeof COOKIE_DONE == sizeof COOKIE_DISABLETIMEOUT);
JASSERT_STATIC(sizeof COOKIE_DONE == sizeof COOKIE_RESTARTTIMEOUT);

petabricks::SubprocessTestIsolation::SubprocessTestIsolation(double to) 
  : _pid(-1), _fd(-1), _rv(RUNNING_RV), _timeout(to), _timeoutEnabled(false), _start(jalib::JTime::null())
{
  if(_timeout < std::numeric_limits<double>::max()-TIMEOUT_GRACESEC)
    _timeout += TIMEOUT_GRACESEC;
}

void petabricks::SubprocessTestIsolation::onTunableModification(jalib::JTunable* t, jalib::TunableValue, jalib::TunableValue newVal){ 
  _modifications.push_back(TunableMod(t,newVal)); 
}

petabricks::TestIsolation* theMasterProcess = NULL;
petabricks::TestIsolation* petabricks::SubprocessTestIsolation::masterProcess() {
  return theMasterProcess;
}

bool petabricks::SubprocessTestIsolation::beginTest(int workerThreads, int reexecchild) {
  JASSERT(theMasterProcess==NULL);
  _modifications.clear();
#ifdef HAVE_OPENCL
  GpuManager::shutdown();
#endif
  DynamicScheduler::cpuScheduler().shutdown();
  int fds[2];
  //JASSERT(pipe(fds) == 0);
  if(reexecchild<0)  {
    JASSERT(socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
    JASSERT((_pid=fork()) >=0);
  }else{
    _pid=0;
  }
  if(_pid>0){
    //parent
    _fd=fds[0];
    close(fds[1]);
    _rv = RUNNING_RV;
    _timeoutEnabled = false;
    _start = jalib::JTime::now();
    //JTRACE("parent");
    return false;
  }else{
    //child
    //JTRACE("child")(reexecchild);
    if(reexecchild<0) {
      _fd=fds[1];
      close(fds[0]);
      PetabricksRuntime::reexecTestIsolation(fds[1]);
    }else{
      _fd = reexecchild;
    }
    GpuManager::start();
    _settestprocflags();
    PetabricksRuntime::startWorkerThreads(workerThreads);
    jalib::JTunable::setModificationCallback(this); 
    theMasterProcess=this;
    //JTRACE("child starting");
    restartTimeout();
    return true;
  }
}

void petabricks::SubprocessTestIsolation::disableTimeout() {
  JASSERT(write(_fd, COOKIE_DISABLETIMEOUT, strlen(COOKIE_DISABLETIMEOUT))>0)(JASSERT_ERRNO);
  fsync(_fd);
}

void petabricks::SubprocessTestIsolation::restartTimeout() {
  JASSERT(write(_fd, COOKIE_RESTARTTIMEOUT, strlen(COOKIE_RESTARTTIMEOUT))>0)(JASSERT_ERRNO);
  fsync(_fd);
}

void petabricks::SubprocessTestIsolation::endTest(TestResult& result) {
  JASSERT(_pid==0);
  JASSERT(write(_fd, COOKIE_DONE, strlen(COOKIE_DONE))>0)(JASSERT_ERRNO);
  jalib::JBinarySerializeWriterRaw o("pipe", _fd);
  o.serialize(result.time);
  o.serialize(result.accuracy);
  o & result.hash;
  o.serializeVector(_modifications);
  fflush(NULL);
  fsync(_fd);
  fsync(fileno(stdout));
  fsync(fileno(stderr));
  //GpuManager::shutdown();
  //DynamicScheduler::cpuScheduler().shutdown();
  _exit(SUCCESS_RV);
}

inline static void _settimeout(struct timespec& timeout, double sec){
  if(sec >= std::numeric_limits<long>::max()){
    timeout.tv_sec=std::numeric_limits<long>::max();
    timeout.tv_nsec=0;
  }else{
    timeout.tv_sec = (long)sec;
    timeout.tv_nsec = (long)((sec-(double)timeout.tv_sec)*1.0e9);
  }
}

inline static void _settimeout(int& timeout, double sec){
  if(sec >= std::numeric_limits<int>::max()/1000-1){
    timeout = -1;
  }else{
    timeout = (int)(sec*1000.0 + 0.5);
  }
}


void petabricks::SubprocessTestIsolation::recvResult(TestResult& result) {
  TimeoutT timeout;
  int ready;
  _settimeout(timeout, std::numeric_limits<int>::max());

  struct pollfd fds[1];
  fds[0].fd = _fd;
  fds[0].events = POLLIN;
  fds[0].revents = 0;

  for(;;){
    _settimeout(timeout, timeleft());
    //JTRACE("timeout")(timeout);
    ready = poll(fds, sizeof(fds)/sizeof(struct pollfd), timeout);
    JASSERT(ready>=0)(ready);

    if(ready==0){ //timeout
      killChild();
      break;
    }

    if(handleEvent(result)){
      break;
    }
  }
}
    
void petabricks::SubprocessTestIsolation::recvFirstResult(SubprocessTestIsolation& a, TestResult& aresult,
                                                          SubprocessTestIsolation& b, TestResult& bresult) {
  struct pollfd fds[2];
  struct pollfd* pfds = fds;
  bool adone = false;
  bool bdone = false;
  int nfds = 2;
  fds[0].fd = a._fd;
  fds[0].events = POLLIN;
  fds[0].revents = 0;
  fds[1].fd = b._fd;
  fds[1].events = POLLIN;
  fds[1].revents = 0;

  for(;;){
    int ready = 0;
    double secleft = std::max(0.0, std::max(a.timeleft(), b.timeleft()));
    TimeoutT timeout;
    _settimeout(timeout, secleft);
    ready = poll(pfds, nfds, timeout);
    JASSERT(ready>=0)(ready);

    if(ready==0){ //timeout
      a.killChild();
      b.killChild();
      break;
    }

    try {
      adone = adone || ((fds[0].revents&POLLIN)!=0 && a.handleEvent(aresult));
    } catch(UnknownTestFailure e) {
      adone = true;
    }
    try {
      bdone = bdone || ((fds[1].revents&POLLIN)!=0 && b.handleEvent(bresult));
    } catch(UnknownTestFailure e) {
      bdone = true;
    }
    if(adone) {
      fds[0].revents = 0;
      pfds = fds+1;
      nfds = 1;
      b._timeout = PetabricksRuntime::updateRaceTimeout(aresult, 0);
    }else if(bdone) {
      fds[1].revents = 0;
      pfds = fds;
      nfds = 1;
      a._timeout = PetabricksRuntime::updateRaceTimeout(bresult, 1);
    }
    if(adone && bdone) break;
  }
}

double petabricks::SubprocessTestIsolation::timeleft() const {
  if(!running())
      return 0;
  if(_timeoutEnabled)
    return _timeout - (jalib::JTime::now() - _start) + 0.02;
  return std::numeric_limits<double>::max();
}

bool petabricks::SubprocessTestIsolation::handleEvent(TestResult& result) {
  //receive a control code
  std::string cnt = recvControlCookie();

  //check control code:
  if(cnt==COOKIE_DISABLETIMEOUT){
    _timeoutEnabled = false;
    return false;
  }
  if(cnt==COOKIE_RESTARTTIMEOUT){
    _timeoutEnabled = true;
    _start = jalib::JTime::now();
    return false;
  }
  if(cnt!=COOKIE_DONE || (!running() && rv()!=SUCCESS_RV )){ 
    //unknown control code, most likely assertion failure in child
    killChild();
    throw UnknownTestFailure(rv());
  }

  //read the result
  jalib::JBinarySerializeReaderRaw o("pipe", _fd);
  o.serialize(result.time);
  o.serialize(result.accuracy);
  o & result.hash;
  o.serializeVector(_modifications);

  //reapply tunable modifications to this process
  for(size_t i=0; i<_modifications.size(); ++i)
    _modifications[i].tunable->setValue(_modifications[i].value);
  _modifications.clear();

  //wait for subprocess to exit cleanly
  waitExited();
  if(rv()!=SUCCESS_RV){
    throw UnknownTestFailure(rv());
  }
  return true;
}

std::string petabricks::SubprocessTestIsolation::recvControlCookie() {
  //perform a test read -- we dont expect reads to block because of pselect above
  char buf[sizeof COOKIE_DONE];
  for(ssize_t n=0; n==0;){
    memset(buf, 0, sizeof buf);
    n=recv(_fd, buf, strlen(COOKIE_DONE), MSG_DONTWAIT);
    if(n<0 && errno==EAGAIN)
      n=0;
    testExited();
    if(!running() && rv()!=SUCCESS_RV)
      break;//child failed
  }
  return buf;
}
    
void  petabricks::SubprocessTestIsolation::killChild() {
  if(running()){
    kill(_pid, TIMEOUTKILLSIG);
    waitExited();
  }
}
void  petabricks::SubprocessTestIsolation::waitExited() {
  if(running()){
    if(waitpid(_pid, &_rv, 0)==_pid){
      JASSERT(!running())(_rv);
    }else
      JASSERT(false)(JASSERT_ERRNO).Text("wait failed");
  }
  if(_fd>=0){
    close(_fd);
    _fd=-1;
  }

}
void  petabricks::SubprocessTestIsolation::testExited() {
  if(running()){
    if(waitpid(_pid, &_rv, WNOHANG)==_pid)
      JASSERT(!running())(_rv);
    else
      _rv=RUNNING_RV;//ensure running()
  }
}
bool petabricks::SubprocessTestIsolation::running() const{ 
  return _rv<-256; 
}
int  petabricks::SubprocessTestIsolation::rv(){
  JASSERT(!running());
  if(WIFEXITED(_rv))
    return WEXITSTATUS(_rv);
  return CRASH_RV;
}

bool petabricks::DummyTestIsolation::beginTest(int workerThreads, int reexecchild) {
  DynamicScheduler::cpuScheduler().startWorkerThreads(workerThreads);
  JASSERT(reexecchild<0);
  return true;
}

void petabricks::DummyTestIsolation::endTest(TestResult&) {}

void petabricks::DummyTestIsolation::recvResult(TestResult&) {
  JASSERT(false);
}

