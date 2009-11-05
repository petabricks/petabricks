/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#include "testisolation.h"
#include "dynamicscheduler.h"

#include <limits>
#include <string.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
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
static const char COOKIE[] = "!";
static const char COOKIE_DISABLETIMEOUT[] = "E";
static const char COOKIE_RESTARTTIMEOUT[] = "R";

petabricks::SubprocessTestIsolation::SubprocessTestIsolation(double to) 
  : _pid(-1), _fd(-1), _timeout(to)
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

bool petabricks::SubprocessTestIsolation::beginTest(int workerThreads) {
  JASSERT(theMasterProcess==NULL);
  _modifications.clear();
  DynamicScheduler::cpuScheduler().shutdown();
  int fds[2];
  //JASSERT(pipe(fds) == 0);
  JASSERT(socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0);
  JASSERT((_pid=fork()) >=0);
  if(_pid>0){
    //parent
    _fd=fds[0];
    close(fds[1]);
    return false;
  }else{
    //child
    _fd=fds[1];
    close(fds[0]);
    DynamicScheduler::cpuScheduler().startWorkerThreads(workerThreads);
    _settestprocflags();
    jalib::JTunable::setModificationCallback(this); 
    theMasterProcess=this;
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

void petabricks::SubprocessTestIsolation::endTest(double time, double accuracy) {
  JASSERT(_pid==0);
  JASSERT(write(_fd, COOKIE, strlen(COOKIE))>0)(JASSERT_ERRNO);
  jalib::JBinarySerializeWriterRaw o("pipe", _fd);
  o.serialize(time);
  o.serialize(accuracy);
  o.serializeVector(_modifications);
  fflush(NULL);
  fsync(_fd);
  fsync(fileno(stdout));
  fsync(fileno(stderr));
  //DynamicScheduler::cpuScheduler().shutdown();
  _exit(0);
}

inline static void _settimespec(struct timespec& timeout, double sec){
  if(sec>=std::numeric_limits<long>::max()){
    timeout.tv_sec=std::numeric_limits<long>::max();
    timeout.tv_nsec=0;
  }else{
    timeout.tv_sec = (long)sec;
    timeout.tv_nsec = (long)((sec-(double)timeout.tv_sec)*1.0e9);
  }
}

void petabricks::SubprocessTestIsolation::recvResult(double& time, double& accuracy) {
  time = jalib::maxval<double>();
  accuracy = jalib::minval<double>();
  int rv=-257;//special value means still running
  struct timespec timeout;
  fd_set rfds;
  FD_ZERO(&rfds);
  _settimespec(timeout, _timeout);

  for(;;){
    FD_SET(_fd, &rfds);
    int s=pselect(_fd+1, &rfds, NULL, NULL, &timeout, NULL);
    JASSERT(s>=0)(s);

    if(s==0){
      //timeout... kill the subprocess
      if(rv<-256){
        kill(_pid, TIMEOUTKILLSIG);
        JASSERT(waitpid(_pid,&rv,0)==_pid);
      }
      break;
    }

    //receive a control code
    std::string cnt = recvControlCookie(rv);

    //special control codes:
    if(cnt==COOKIE_DISABLETIMEOUT){
      _settimespec(timeout, std::numeric_limits<double>::max());
      continue;
    }
    if(cnt==COOKIE_RESTARTTIMEOUT){
      _settimespec(timeout, _timeout);
      continue;
    }

    JASSERT(cnt==COOKIE)(cnt)(rv).Text("subprocess test failed");

    //read the result
    jalib::JBinarySerializeReaderRaw o("pipe", _fd);
    o.serialize(time);
    o.serialize(accuracy);
    o.serializeVector(_modifications);

    //reapply tunable modifications to this process
    for(size_t i=0; i<_modifications.size(); ++i)
      _modifications[i].tunable->setValue(_modifications[i].value);
    _modifications.clear();

    //wait for subprocess to exit cleanly
    if(rv<-256){
      JASSERT(waitpid(_pid,&rv,0)==_pid);
    }
    JASSERT(rv==0)(rv).Text("test subprocess failed");
    break;
  }
  close(_fd);
}

std::string petabricks::SubprocessTestIsolation::recvControlCookie(int& rv) {
  JASSERT(sizeof COOKIE == sizeof COOKIE_DISABLETIMEOUT);
  JASSERT(sizeof COOKIE == sizeof COOKIE_RESTARTTIMEOUT);
  //perform a test read -- we dont expect reads to block because of pselect above
  char buf[sizeof COOKIE];
  for(ssize_t n=0; n==0;){
    memset(buf, 0, sizeof buf);
    n=recv(_fd, buf, strlen(COOKIE), MSG_DONTWAIT);
    if(n<0 && errno==EAGAIN)
      n=0;
    if(rv<-256 && waitpid(_pid, &rv, WNOHANG)==_pid && rv!=0)
      break;//child failed
  }
  return buf;
}

bool petabricks::DummyTestIsolation::beginTest(int workerThreads) {
  DynamicScheduler::cpuScheduler().startWorkerThreads(workerThreads);
  return true;
}

void petabricks::DummyTestIsolation::endTest(double /*time*/, double /*accuracy*/) {}

void petabricks::DummyTestIsolation::recvResult(double& /*time*/, double& /*accuracy*/) {
  JASSERT(false);
}

