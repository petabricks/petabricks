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
#ifndef PETABRICKSPETABRICKSRUNTIME_H
#define PETABRICKSPETABRICKSRUNTIME_H

#include "common/jrefcounted.h"
#include "common/jtunable.h"

#include <float.h>
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
typedef jalib::JRef<Autotuner> AutotunerPtr;
typedef std::vector<AutotunerPtr> AutotunerList;

typedef std::vector<jalib::JTunable*> TunableListT;

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
    virtual void read(ArgListT) = 0;

    ///
    /// Perform the computation
    virtual void compute() = 0;

    ///
    /// Write inputs to disk
    virtual void write(ArgListT) = 0;

    ///
    /// initialize with random inputs
    virtual void randomInputs(int size) = 0;


    virtual const char* name() = 0;

    virtual int numInputs() = 0;
    virtual int numOutputs() = 0;
    int numArgs() { return numInputs()+numOutputs(); }

    virtual MATRIX_ELEMENT_T accuracy() = 0;
    virtual MATRIX_ELEMENT_T accuracyTarget() = 0;
    virtual TunableListT accuracyVariables(int n) = 0;

    bool isVariableAccuracy(){
      return accuracyTarget() != std::numeric_limits<MATRIX_ELEMENT_T>::min();
    }

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

  void runGraphParamMode(const std::string& param);

  void runGraphParallelMode();

  double optimizeParameter(const std::string& param);
  double optimizeParameter(jalib::JTunable& param, int min, int max, int step=-1);

  void runAutotuneMode();
  void runAutotuneLoop(const AutotunerList& tuners);
  void runMultigridAutotuneMode();

  double runTrial(TestIsolation&);
  double runTrial(double thresh);

  static bool isTrainingRun();
  //static void setIsTrainingRun(bool b);

  void setSize(int n){_randSize=n;};
  int curSize() const { return _randSize;};

  static void abort();

  static void saveConfig();

  double computeWrapper(TestIsolation&);
  double trainAndComputeWrapper(TestIsolation&);
  
  
  void variableAccuracyTrainingLoop(TestIsolation& ti);

  class ComputeRetryException {};

  static int randInt(int min=0, int max=RAND_MAX){
    return (lrand48()%(max-min)) + min;
  }
  static double randDouble(double min=0, double max=std::numeric_limits<int>::max()){
    return (drand48()*(max-min)) + min;
  }

private:
  Main* _main;
  std::string _mainName;
  int _randSize;
  int _rv;//return val
};

}

#endif
