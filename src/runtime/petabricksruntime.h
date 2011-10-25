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
#ifndef PETABRICKSPETABRICKSRUNTIME_H
#define PETABRICKSPETABRICKSRUNTIME_H

#include "common/jrefcounted.h"
#include "common/jtunable.h"
#include "common/hash.h"

#include <float.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <string>
#include <vector>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

namespace petabricks {
class TestIsolation;
class Autotuner;
class TestResult;
typedef jalib::JRef<Autotuner> AutotunerPtr;
typedef std::vector<AutotunerPtr> AutotunerList;

class TunableListT : public std::vector<jalib::JTunable*> {
public:
  /*
  void resetMinAll(int offset){
    for(iterator i=begin(); i!=end(); ++i)
      (*i)->setValue((*i)->min()+offset); 
  }
  bool incrementAll(){
    for(iterator i=begin(); i!=end(); ++i){
      if((*i)->value() < (*i)->max())
        (*i)->setValue((*i)->value()+1); 
      else{
        return false;
      }
    }
    return true;
  }
  */
};

int petabricksMain(int argc, const char** argv);

class DynamicScheduler;

class PetabricksRuntime{
public:
  /**
   * Interface used to construct a main routine in generated code
   */
  typedef class __usr_main_interface{
  public:
    typedef const std::vector<std::string>& ArgListT;

    ///
    /// destructor
    virtual ~__usr_main_interface(){}

    ///
    /// get a usage string
    virtual std::string helpString() = 0;

    ///
    /// Read inputs from disk
    virtual void readInputs(ArgListT) = 0;
    virtual void readOutputs(ArgListT) = 0;

    ///
    /// Perform the computation
    virtual void compute() = 0;

    ///
    /// Write inputs to disk
    virtual void writeInputs(ArgListT) = 0;
    virtual void writeOutputs(ArgListT) = 0;

    ///
    /// initialize with random inputs
    virtual void randomizeInputs() = 0;
    virtual void randomizeOutputs() = 0;
    void randomize(){
      randomizeInputs();
      randomizeOutputs();
    }

    virtual void deallocate() = 0;
    virtual void reallocate(int size) = 0;

    virtual const char* name() = 0;

    virtual int numInputs() = 0;
    virtual int numOutputs() = 0;
    int numArgs() { return numInputs()+numOutputs(); }

    virtual MATRIX_ELEMENT_T accuracy() = 0;
    virtual MATRIX_ELEMENT_T accuracyTarget() = 0;
    virtual TunableListT accuracyVariables(int n) = 0;

    bool isVariableAccuracy(){
      return accuracyTarget() != jalib::minval<MATRIX_ELEMENT_T>();
    }
    
    virtual void hash(jalib::HashGenerator&) = 0;

    virtual __usr_main_interface* nextTemplateMain() = 0;
  } Main;

  ///
  /// Construct the runtime, parse any runtime-specific args
  PetabricksRuntime(int argc, const char** argv, Main*);

  ///
  /// Destruct the runtime, saving config to disk
  ~PetabricksRuntime();

  ///
  /// Run the given main routine
  int runMain();

  void runNormal();

  void runGraphMode();
  void runGraphTemplate();

  void runGraphParamMode(const std::string& param);

  void runGraphParallelMode();

  double optimizeParameter(const std::string& param);
  double optimizeParameter(jalib::JTunable& param, int min, int max, int step=-1);

  void runAutotuneMode();
  void runAutotuneLoop(const AutotunerList& tuners);

  double runTrial(TestIsolation&, bool train);
  double runTrialNoSmoothing(TestIsolation&, bool train);
  double runTrial(double thresh, bool train);

  static bool isTrainingRun();
  //static void setIsTrainingRun(bool b);

  void setSize(int n){_randSize=n;};
  int curSize() const { return _randSize;};

  ///
  /// Semi-gracefully end the current execution (means invalid config)
  static void abort();

  ///
  /// Indicate that the current configuration has not been trained
  /// Usually equivalent to abort()
  static void untrained();

  static void saveConfig();

  void spawnDistributedNodes(int argc, const char** argv);
  void distributedSlaveLoop();

  double trainAndComputeWrapper(TestIsolation&, int n);
  double raceConfigs(int n, const std::vector<std::string>* files = NULL, int retries=-1);
  double computeWrapper(TestIsolation&, int n=-1, int retries=-1, const std::vector<std::string>* files = NULL);
  void computeWrapperSubproc( TestIsolation&
                            , int n
                            , TestResult& result
                            , const std::vector<std::string>* files);
  void loadTestInput(int n, const std::vector<std::string>* files);
  
  
  static void startWorkerThreads(int worker_threads);


  void variableAccuracyTrainingLoop(TestIsolation& ti);
  int variableAccuracyTrainingLoopInner(TestIsolation& ti);


  void iogenCreate(const std::vector<std::string>& files);
  void iogenRun(const std::vector<std::string>& files);
  std::vector<std::string> iogenFiles(const std::string& pfx);

  class ComputeRetryException {};

  static double rand01();
  static int randInt(int min=0, int max=RAND_MAX){
    if(min==max) return min;
    return (lrand48()%(max-min)) + min;
  }
  static double randDouble(double min=0, double max=std::numeric_limits<int>::max()){
    if(min==max) return min;
    return (rand01()*(max-min)) + min;
  }
  static double randNormal(double mean, double sigma){
    //formaula taken from boost
    //TODO: link with and call boost directly
    double r1 = rand01();
    double r2 = rand01();
    double pi =  3.14159265358979323846;
    return sqrt(-2.0 * log(1.0-r2)) * cos(2.0*pi*r1) * sigma + mean;
  }


  static double updateRaceTimeout(TestResult& result, int winnerid);
      
  
  static void reexecTestIsolation(int fd);
protected:
  void reallocate() { _main->reallocate(_randSize); }

private:
  Main* _main;
  std::string _mainName;
  int _randSize;
  int _rv;//return val
};

}

#endif
