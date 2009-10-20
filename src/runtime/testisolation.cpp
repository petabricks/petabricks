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

static const char COOKIE[] = "!";

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

void petabricks::SubprocessTestIsolation::endTest(double result) {
  JASSERT(_pid==0);
  JASSERT(write(_fd, COOKIE, strlen(COOKIE))>0)(JASSERT_ERRNO);
  jalib::JBinarySerializeWriterRaw o("pipe", _fd);
  o.serialize(result);
  o.serializeVector(_modifications);
  DynamicScheduler::cpuScheduler().shutdown();
  _exit(0);
}

double petabricks::SubprocessTestIsolation::recvResult() {
  int rv=-1;
  double result = std::numeric_limits<double>::max();
  //wait for _timeout
  JASSERT(_pid>0);
  fd_set rfds;
  FD_ZERO(&rfds);
  FD_SET(_fd, &rfds);
  struct timespec timeout;
  timeout.tv_sec = (long) _timeout;
  timeout.tv_nsec = (long)((_timeout-(double)timeout.tv_sec)*1.0e9);
  if(_timeout>=std::numeric_limits<long>::max()){
    timeout.tv_sec=std::numeric_limits<long>::max();
    timeout.tv_nsec=0;
  }
  int s=pselect(_fd+1, &rfds, NULL, NULL, &timeout, NULL);
  JASSERT(s>=0)(s);

  if(s==0){
    //timeout
    if(rv==-1){
      kill(_pid, SIGTERM);//error not checked
      JASSERT(waitpid(_pid, &rv, NULL)==_pid);
    }
  }else{
    {
      //perform a test read -- we dont expect reads to block because of pselect above
      char buf[sizeof COOKIE];
      memset(buf, 0, sizeof buf);
      for(ssize_t n=0; n==0;){
        n=recv(_fd, buf, strlen(COOKIE), MSG_DONTWAIT);
        if(n<0 && errno==EAGAIN)
          n=0;
        if(rv==-1 && waitpid(_pid, &rv, WNOHANG)==_pid && rv!=0)
          break;
      }
      JASSERT(strncmp(COOKIE, buf, sizeof COOKIE)==0).Text("subprocess failed");
    }
    //read the result
    jalib::JBinarySerializeReaderRaw o("pipe", _fd);
    o.serialize(result);
    o.serializeVector(_modifications);
    for(size_t i=0; i<_modifications.size(); ++i)
      _modifications[i].tunable->setValue(_modifications[i].value);
    _modifications.clear();
    if(rv==-1){
      JASSERT(waitpid(_pid,&rv,0)==_pid);
    }
    JASSERT(rv==0)(rv).Text("test subprocess failed");
  }
  close(_fd);
  return result;
}

bool petabricks::DummyTestIsolation::beginTest(int workerThreads) {
  DynamicScheduler::cpuScheduler().startWorkerThreads(workerThreads);
  return true;
}

void petabricks::DummyTestIsolation::endTest(double /*result*/) {}

double petabricks::DummyTestIsolation::recvResult() {
  JASSERT(false);
  return -1;
}

