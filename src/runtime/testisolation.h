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
#ifndef PETABRICKSTESTISOLATION_H
#define PETABRICKSTESTISOLATION_H

#include "common/hash.h"
#include "common/jconvert.h"
#include "common/jserialize.h"
#include "common/jtunable.h"
#include "common/jtimer.h"

#include <vector>
#include <unistd.h>
#include <stdio.h>

namespace petabricks {

  struct TestResult {
    double time;
    double accuracy;
    jalib::Hash hash;
    bool crashed;
    TestResult() 
      : time(jalib::maxval<double>())
      , accuracy(jalib::minval<double>())
      , crashed(false)
    {}
    void writexml(std::ostream& o, const char* label) {
      o << "<testresult"
        << " label=\""    << label    << "\""
        << " timing=\""   << time     << "\""
        << " accuracy=\"" << accuracy << "\""
        << " crashed=\""  << crashed << "\""
        << " hash=\""     << hash     << "\" />";
    }
  };

  class TestIsolation {
  public:
    virtual ~TestIsolation(){}
    virtual bool beginTest(int workerThreads, int reexecchild)=0;
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

    bool beginTest(int workerThreads, int reexecchild);
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
    bool running() const;
    int rv();
    bool handleEvent(TestResult& result);
    double timeleft() const;

  private:
    pid_t _pid;
    int _fd;
    int _rv;
    std::vector<TunableMod> _modifications;
    double _timeout;
    bool _timeoutEnabled;
    jalib::JTime _start;
  };

  
  /**
   * Runs the timing test in the current process
   */
  class DummyTestIsolation : public TestIsolation
  {
  public:
    bool beginTest(int workerThreads, int reexecchild);
    void endTest(TestResult& result);
    void recvResult(TestResult& result);
  };

}

#endif
