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
#include <signal.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

petabricks::SubprocessTestIsolation::SubprocessTestIsolation(double to) 
  : _pid(-1), _fd(-1), _timeout(to)
{
  if(_timeout < std::numeric_limits<double>::max()-TIMEOUT_GRACESEC)
    _timeout += TIMEOUT_GRACESEC;
}

void petabricks::SubprocessTestIsolation::onTunableModification(jalib::JTunable* t, jalib::TunableValue, jalib::TunableValue newVal){ 
  _modifications.push_back(TunableMod(t,newVal)); 
}

bool petabricks::SubprocessTestIsolation::beginTest(int workerThreads) {
  _modifications.clear();
  DynamicScheduler::cpuScheduler().shutdown();
  int fds[2];
  JASSERT(pipe(fds) == 0);
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
    jalib::JTunable::setModificationCallback(this); 
    return true;
  }
}

void petabricks::SubprocessTestIsolation::endTest(double result) {
  JASSERT(_pid==0);
  jalib::JBinarySerializeWriterRaw o("pipe", _fd);
  o.serialize(result);
  o.serializeVector(_modifications);
  DynamicScheduler::cpuScheduler().shutdown();
  _exit(0);
}

double petabricks::SubprocessTestIsolation::recvResult() {
  //wait for _timeout
  JASSERT(_pid>0);
  fd_set fds;
  FD_ZERO(&fds);
  FD_SET(_fd, &fds);
  struct timespec timeout;
  timeout.tv_sec = (long) _timeout;
  timeout.tv_nsec = (long)((_timeout-(double)timeout.tv_sec)*1.0e9);
  if(_timeout>=std::numeric_limits<long>::max()){
    timeout.tv_sec=std::numeric_limits<long>::max();
    timeout.tv_nsec=0;
  }
  int s=pselect(_fd+1, &fds, NULL, NULL, &timeout, NULL);
  JASSERT(s>=0)(s);

  if(s==0){
    //timeout
    kill(_pid, SIGTERM);//error not checked
    JASSERT(waitpid(_pid, NULL, NULL)==_pid);
    close(_fd);
    return std::numeric_limits<double>::max();
  }else{
    //read the result
    double result = -1;
    jalib::JBinarySerializeReaderRaw o("pipe", _fd);
    o.serialize(result);
    o.serializeVector(_modifications);
    for(size_t i=0; i<_modifications.size(); ++i)
      _modifications[i].tunable->setValue(_modifications[i].value);
    _modifications.clear();
    int rv=-1;
    JASSERT(waitpid(_pid,&rv,0)==_pid);
    JASSERT(rv==0)(rv).Text("test subprocess failed");
    close(_fd);
    return result;
  }
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

