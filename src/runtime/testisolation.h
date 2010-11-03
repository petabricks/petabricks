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
#ifndef PETABRICKSTESTISOLATION_H
#define PETABRICKSTESTISOLATION_H

#include "common/hash.h"
#include "common/jconvert.h"
#include "common/jserialize.h"
#include "common/jtunable.h"

#include <vector>
#include <unistd.h>
#include <stdio.h>

namespace petabricks {

  struct TestResult {
    double time;
    double accuracy;
    jalib::Hash hash;
    TestResult() 
      : time(jalib::maxval<double>()), accuracy(jalib::minval<double>()) 
    {}
  };

  class TestIsolation {
  public:
    virtual ~TestIsolation(){}
    virtual bool beginTest(int workerThreads)=0;
    virtual void endTest(TestResult& result)=0;
    virtual void recvResult(TestResult&)=0;
    virtual void disableTimeout(){}
    virtual void restartTimeout(){}

    // thrown when we get no response from child process
    struct UnknownTestFailure {
      UnknownTestFailure(int r):rv(r){};
      int rv; 
    };
  };

  /**
   * Forks a subprocess to run timing tests in
   */
  class SubprocessTestIsolation : public TestIsolation
                                , public jalib::JTunableModificationMonitor
  {
    //typedef struct timespec TimeoutT;
    typedef int TimeoutT;
    struct TunableMod {
      jalib::JTunable* tunable;
      jalib::TunableValue value;
      TunableMod(jalib::JTunable* tt=0, jalib::TunableValue vv=0):tunable(tt),value(vv){}
    };
  public:
    static TestIsolation* masterProcess();

    SubprocessTestIsolation(double to);

    void onTunableModification(jalib::JTunable* t, jalib::TunableValue, jalib::TunableValue newVal);

    bool beginTest(int workerThreads);
    void endTest(TestResult&);
    void recvResult(TestResult&);
    void disableTimeout();
    void restartTimeout();
    
    static void recvFirstResult(SubprocessTestIsolation& a, TestResult& aresult,
                                SubprocessTestIsolation& b, TestResult& bresult);
  protected:
    std::string recvControlCookie();
    void killChild();
    void waitExited();
    void testExited();
    bool running();
    int rv();
    bool handleEvent(TestResult& result, TimeoutT& timeout);
  private:
    pid_t _pid;
    int _fd;
    int _rv;
    std::vector<TunableMod>  _modifications;
    double _timeout;
  };
  
  /**
   * Runs the timing test in the current process
   */
  class DummyTestIsolation : public TestIsolation
  {
  public:
    bool beginTest(int workerThreads);
    void endTest(TestResult& result);
    void recvResult(TestResult& result);
  };

}

#endif
