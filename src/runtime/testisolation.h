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

#include "common/jtunable.h"
#include "common/jserialize.h"

#include <vector>
#include <unistd.h>

namespace petabricks {
  class TestIsolation {
  public:
    virtual ~TestIsolation(){}
    virtual bool beginTest(int workerThreads)=0;
    virtual void endTest(double time, double accuracy)=0;
    virtual void recvResult(double& time, double& accuracy)=0;
    virtual void disableTimeout(){}
  };

  /**
   * Forks a subprocess to run timing tests in
   */
  class SubprocessTestIsolation : public TestIsolation
                                , public jalib::JTunableModificationMonitor
  {
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
    void endTest(double time, double accuracy);
    void recvResult(double& time, double& accuracy);
    void disableTimeout();
  private:
    pid_t _pid;
    int _fd;
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
    void endTest(double time, double accuracy);
    void recvResult(double& time, double& accuracy);
  };

}

#endif
