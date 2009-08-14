/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include "petabricksruntime.h"

#include "autotuner.h"
#include "dynamicscheduler.h"
#include "dynamictask.h"

#include "common/jargs.h"
#include "common/jfilesystem.h"
#include "common/jtimer.h"
#include "common/jtunable.h"

#include <algorithm>
#include <limits>
#include <math.h>

//these must be declared in the user code
petabricks::PetabricksRuntime::Main* petabricksMainTransform();
petabricks::PetabricksRuntime::Main* petabricksFindTransform(const std::string& name);

static bool _isTrainingRun = false;
static bool _needTraingingRun = false;

static std::string CONFIG_FILENAME;
static int GRAPH_MIN=1;
static int GRAPH_MAX=5000;
static int GRAPH_MAX_SEC=std::numeric_limits<int>::max();
static int GRAPH_STEP=50;
static int GRAPH_TRIALS=1;
static int GRAPH_SMOOTHING=0;
static int SEARCH_BRANCH_FACTOR=8;
static bool MULTIGRID_FLAG=false;
static bool FULL_MULTIGRID_FLAG=false;
static bool DUMPTIMING=false;
static bool ACCURACY=false;
static bool FORCEOUTPUT=false;
std::vector<std::string> txArgs;

static enum {
  MODE_RUN_IO,
  MODE_RUN_RANDOM,
  MODE_GRAPH_INPUTSIZE,
  MODE_GRAPH_PARAM,
  MODE_GRAPH_THREADS,
  MODE_AUTOTUNE_GENETIC,
  MODE_AUTOTUNE_PARAM,
  MODE_ABORT,
  MODE_HELP
} MODE = MODE_RUN_IO;

std::vector<std::string> autotuneParams;
std::vector<std::string> cutoffparams;
std::string graphParam;

static struct {
  int count;
  double total;
  double total_accuracy;
  double min;
  double max;
} timing = {0, 0.0, 0.0, std::numeric_limits<double>::max(), std::numeric_limits<double>::min() };

JTUNABLE(worker_threads,   8, MIN_NUM_WORKERS, MAX_NUM_WORKERS);

namespace{//file local
  typedef jalib::JTunableManager TunableManager;
}

petabricks::PetabricksRuntime::PetabricksRuntime(int argc, const char** argv, Main* m)
  : _main(m)
  , _randSize(-1)
  , _rv(0)
{
  jalib::JArgs args(argc, argv);

  if(args.needHelp())
    std::cerr << "GENERAL OPTIONS:" << std::endl;

  //aliases
  args.alias("cfg", "config");
  args.alias("random", "n");
  args.alias("tx", "transform");
  args.alias("graph", "graph-input");
  
  //load config from disk
  CONFIG_FILENAME = jalib::Filesystem::GetProgramPath() + ".cfg";
  args.param("config", CONFIG_FILENAME).help("filename of the program configuration");
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0 && jalib::Filesystem::FileExists(CONFIG_FILENAME))
    tm.load(CONFIG_FILENAME);

  //set the main transform
  if(args.param("transform", _mainName).help("specify an alternate main transform to run")){
    _main = m = petabricksFindTransform(_mainName);
    JASSERT(m!=NULL)(_mainName).Text("unknown transform");
  }else{
    JASSERT(m!=NULL);
    _mainName = m->name();
  }
  
  //seed the random number generator 
  if(! args.param("fixedrandom").help("don't seed the random number generator")){
    struct timeval currentTimeVal;
    gettimeofday(&currentTimeVal, NULL);
    srand48(currentTimeVal.tv_usec);
  }
  
  args.param("accuracy",  ACCURACY).help("print out accuracy of answer");
  args.param("time",      DUMPTIMING).help("print timing results in xml format");
  args.param("force-output", FORCEOUTPUT).help("also write copies of outputs to stdout");

  if(args.needHelp())
    std::cerr << std::endl << "ALTERNATE EXECUTION MODES:" << std::endl;

  if(args.param("n", _randSize).help("generate a random input of the given size")){
    MODE=MODE_RUN_RANDOM;//default mode
  }else{
    MODE=MODE_RUN_IO;//default mode
  }

  //figure out what mode we are in
  if(args.param("autotune", autotuneParams).help("run the genetic autotuner at a given choice cite")){
    MODE=MODE_AUTOTUNE_GENETIC;
  } else if(args.param("optimize", graphParam).help("autotune a single given config parameter")){
    MODE=MODE_AUTOTUNE_PARAM;
  } else if(args.param("graph-input").help("graph run time with changing input size")){
    MODE=MODE_GRAPH_INPUTSIZE;
  } else if(args.param("graph-param", graphParam).help("graph run time with changing parameter value")){
    if(graphParam == worker_threads.name())
      MODE=MODE_GRAPH_THREADS;
    else
      MODE=MODE_GRAPH_PARAM;
  }else if(args.param("graph-parallel").help("graph run time with changing number of threads")){
    MODE=MODE_GRAPH_THREADS;
  }
  
  //flags that cause aborts
  if(args.param("reset").help("reset the config file to the default state and exit")){
    jalib::JTunableManager::instance().reset();
    MODE=MODE_ABORT;
  }
  if(args.param("name").help("print out the name of the main transform and exit")){
    std::cout << _mainName << std::endl;
    MODE=MODE_ABORT;
  }
  if(args.needHelp()){
    MODE=MODE_HELP;
  }
  
  if(args.needHelp())
    std::cerr << std::endl << "OPTIONS FOR ALTERNATE EXECUTION MODES:" << std::endl;
  
  args.param("cutoffparam", cutoffparams).help("additional cutoff tunables for use in --autotune mode");
  args.param("min",       GRAPH_MIN).help("minimum input size for graph/autotuning");
  args.param("max",       GRAPH_MAX).help("maximum input size for graph/autotuning");
  args.param("step",      GRAPH_STEP).help("step size for graph/autotuning");
  args.param("trials",    GRAPH_TRIALS).help("number of times to run each data point in graph/autotuning (averaged)");
  args.param("smoothing", GRAPH_SMOOTHING).help("smooth graphs by also running smaller/larger input sizes");
  args.param("max-sec",   GRAPH_MAX_SEC).help("stop graphs/autotuning after algorithm runs too slow");

  args.finishParsing(txArgs);
  
  //startup requested number of threads
  if(MODE!=MODE_GRAPH_THREADS && MODE!=MODE_ABORT){
    JASSERT(worker_threads>=1)(worker_threads);
    DynamicScheduler::instance().startWorkerThreads(worker_threads);
  }

  if(MODE==MODE_HELP){
    //normal mode with no options will print usage
    txArgs.clear();
    MODE=MODE_RUN_IO;
    std::cerr << std::endl;
  }

  JASSERT(MODE==MODE_RUN_IO||txArgs.size()==0)(txArgs.size())
    .Text("too many arguments");
}

petabricks::PetabricksRuntime::~PetabricksRuntime()
{
  saveConfig();
  DynamicScheduler::instance().shutdown();
}

void petabricks::PetabricksRuntime::saveConfig()
{
  //save config to disk
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0){
    tm.save(CONFIG_FILENAME);
  }
}


int petabricks::PetabricksRuntime::runMain(int argc, const char** argv){
  switch(MODE){
    case MODE_RUN_IO:
      runNormal();
      break;
    case MODE_RUN_RANDOM:
      runTrial();
      break;
    case MODE_GRAPH_INPUTSIZE:
      runGraphMode();
      break;
    case MODE_GRAPH_PARAM:
      runGraphParamMode(graphParam);
      break;
    case MODE_GRAPH_THREADS:
      runGraphParallelMode();
      break;
    case MODE_AUTOTUNE_GENETIC:
      runAutotuneMode(autotuneParams, cutoffparams);
      break;
    case MODE_AUTOTUNE_PARAM:
      optimizeParameter(graphParam);
      break;
    case MODE_ABORT:
      break;
  }
  

  if(FORCEOUTPUT && _rv==0){
    JTIMER_SCOPE(forceoutput);
    std::vector<std::string> tmp;
    for(int i=_main->numInputs(); i-->0;)
      tmp.push_back("/dev/null");
    for(int i=_main->numOutputs(); i-->0;)
      tmp.push_back("-");
    _main->write(tmp);
  }

  if(DUMPTIMING | ACCURACY){
    std::cout << "<timing";
    if(ACCURACY) {
      std::cout << " accuracy=\"" << (timing.total_accuracy/timing.count) << "\"";
    }
    std::cout << " count=\"" << timing.count<< "\"";
    std::cout << " average=\"" << (timing.total/timing.count) << "\"";
    std::cout << " total=\"" << timing.total << "\"";
    std::cout << " min=\"" << timing.min<< "\"";
    std::cout << " max=\"" << timing.max<< "\"";
    std::cout << " />\n" << std::flush;
  }

  return _rv;
}

void petabricks::PetabricksRuntime::runNormal(){
  Main& main = *_main;
  if(main.numArgs() != (int)txArgs.size()){
    std::cerr << "USAGE: " << main.name() << " [GENERAL OPTIONS] " << main.helpString() << std::endl;
    std::cerr << "USAGE: " << main.name() << " ALTERNATE_EXECUTION_MODE [OPTIONS]" << std::endl;
    std::cerr << "run `" << main.name() << " --help` for options" << std::endl;
    _rv = 1;
  }else{
    main.read(txArgs);
    try{
      computeWrapper();
    }catch(petabricks::DynamicScheduler::AbortException e){
      DynamicScheduler::instance().abortEnd();
      JASSERT(false).Text("PetabricksRuntime::abort() called");
    }catch(ComputeRetryException e){
      UNIMPLEMENTED();
    }
    main.write(txArgs);
  }
}
  
void petabricks::PetabricksRuntime::runAutotuneMode(const std::vector<std::string>& params, const std::vector<std::string>& extraCutoffs){
  AutotunerList tuners;
  for(size_t i=0; i<params.size(); ++i)
    tuners.push_back(new Autotuner(*this, _main, params[i], extraCutoffs));
  runAutotuneLoop(tuners);
}
  
  
void petabricks::PetabricksRuntime::runAutotune2Mode(const std::string& param){
  AutotunerList tuners;
  for(Main* m = _main; m!=NULL; m=m->nextTemplateMain())
    tuners.push_back(new Autotuner(*this, m, std::string(m->name())+"_"+param, std::vector<std::string>()));
  runAutotuneLoop(tuners);
}

void petabricks::PetabricksRuntime::runAutotuneLoop(const AutotunerList& tuners){
  Main* old = _main;
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n*=2){
    setSize(n+1);
    for(size_t i=0; i<tuners.size(); ++i){
      _main = tuners[i]->main();
      tuners[i]->trainOnce();
    }
    saveConfig();
  }
  _main=old;
}

void petabricks::PetabricksRuntime::runGraphMode(){
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n+=GRAPH_STEP){
    _randSize = (MULTIGRID_FLAG ? (1 << n) + 1 : n);
    double avg = runTrial();
    printf("%d %.6f\n", _randSize, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

void petabricks::PetabricksRuntime::runGraphParamMode(const std::string& param){
  jalib::JTunable* tunable = jalib::JTunableManager::instance().getReverseMap()[param];
  JASSERT(tunable!=0)(param).Text("parameter not found");
  GRAPH_MIN = std::max(GRAPH_MIN, tunable->min());
  GRAPH_MAX = std::min(GRAPH_MAX, tunable->max());
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n+=GRAPH_STEP){
    tunable->setValue(n);
    double avg = runTrial();
    printf("%d %.6lf\n", n, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

void petabricks::PetabricksRuntime::runGraphParallelMode() {
  GRAPH_MIN = std::max(GRAPH_MIN, worker_threads.min());
  GRAPH_MAX = std::min(GRAPH_MAX, worker_threads.max());
  for(int n = GRAPH_MIN; n <= GRAPH_MAX; n+= GRAPH_STEP) {
    worker_threads.setValue(n);
    DynamicScheduler::instance().startWorkerThreads(worker_threads);
    double avg = runTrial();
    printf("%d %.6lf\n", n, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

double petabricks::PetabricksRuntime::runTrial(double thresh){
  JASSERT(_randSize>0)(_randSize).Text("'--n=NUMBER' is required");
#ifdef GRACEFUL_ABORT
  try{
#endif
    std::vector<double> rslts;
    rslts.reserve(2*GRAPH_SMOOTHING+1);
    for( int n =  _randSize-GRAPH_SMOOTHING
       ; n <= _randSize+GRAPH_SMOOTHING
       ; ++n)
    {
      _main->randomInputs(n);
      double t = trainAndComputeWrapper(thresh); // first trial can train
      for(int z=0;z<GRAPH_TRIALS-1; ++z){
        _main->randomInputs(n);
        t += computeWrapper(thresh); // rest of trials cant train
      }
      double avg = t/GRAPH_TRIALS;
      rslts.push_back(avg);
    }
    std::sort(rslts.begin(), rslts.end());
    return rslts[GRAPH_SMOOTHING];
#ifdef GRACEFUL_ABORT
  }catch(petabricks::DynamicScheduler::AbortException e){
    DynamicScheduler::instance().abortEnd();
    return std::numeric_limits<double>::max();
  }
#endif
}

double petabricks::PetabricksRuntime::trainAndComputeWrapper(double thresh){
  if(_main->isVariableAccuracy()){
    variableAccuracyTrainingLoop();
  }

  _isTrainingRun = true;
  _needTraingingRun = false;
  double t;
  try {
    t=computeWrapper(thresh);
  }catch(ComputeRetryException e) {
    _isTrainingRun = false;
    _needTraingingRun = false;
    t=computeWrapper(thresh);
  }
  _isTrainingRun = false;
  _needTraingingRun = false;
  return t;
}

double petabricks::PetabricksRuntime::computeWrapper(double thresh){
#ifdef GRACEFUL_ABORT
//// Set up a time out so we don't waste time running things that are
//// slower than what we have seen already.
//DynamicScheduler::instance().resetAbortFlag();
//if (thresh < std::numeric_limits<unsigned int>::max() - 1) {
//  alarm((unsigned int) thresh + 1);
//}
#endif

  jalib::JTime begin=jalib::JTime::Now();
  _main->compute();
  jalib::JTime end=jalib::JTime::Now();

#ifdef GRACEFUL_ABORT
//// Disable previous alarm
//if (thresh < std::numeric_limits<unsigned int>::max() - 1) {
//  alarm(0);
//}
#endif
  
  if(_needTraingingRun && _isTrainingRun){
    _isTrainingRun=false;
    throw ComputeRetryException();
  }

  double v=end-begin;
  if(DUMPTIMING || ACCURACY){
    double acc = 0;
    if(ACCURACY){
      JTIMER_SCOPE(accuracycompute);
      acc = _main->accuracy();
    }
    timing.count++;
    timing.total+=v;
    timing.total_accuracy+=acc;
    timing.min=std::min(timing.min,v);
    timing.max=std::max(timing.max,v);
  }
  return v;
}
  
void petabricks::PetabricksRuntime::variableAccuracyTrainingLoop(){
  typedef MATRIX_ELEMENT_T ElementT;
  ElementT cur = std::numeric_limits<ElementT>::min();
  ElementT last = std::numeric_limits<ElementT>::min();
  ElementT target = _main->accuracyTarget();
  TunableListT tunables = _main->accuracyVariables(_randSize);
  //reset tunables to min+1
  for(TunableListT::iterator i=tunables.begin(); i!=tunables.end(); ++i)
    (*i)->setValue((*i)->min()+1); 

  for(int i=0; true;++i){
    _main->compute();
    cur = _main->accuracy();
    if(cur >= target){
      JTRACE("training goal reached")(i)(cur)(target);
      break;
    }
    if(cur <= last){
      JASSERT(false)(i)(cur)(target).Text("training goal failed, no progress");
      break;
    }
    //increment tuning var
    for(TunableListT::iterator i=tunables.begin(); i!=tunables.end(); ++i){
      (*i)->setValue((*i)->value()+1); 
      (*i)->verify();
    }
    JTRACE("training")(cur);
    last=cur;
  }
}


double petabricks::PetabricksRuntime::optimizeParameter(const std::string& param){
  jalib::JTunable* tunable = jalib::JTunableManager::instance().getReverseMap()[param];
  JASSERT(tunable!=0)(param).Text("parameter not found");
  return optimizeParameter(*tunable, GRAPH_MIN, GRAPH_MAX, (GRAPH_MAX-GRAPH_MIN)/SEARCH_BRANCH_FACTOR);
}

double petabricks::PetabricksRuntime::optimizeParameter(jalib::JTunable& tunable, int min, int max, int step){
  if(max<=min) return -1;
  if(step<0){
    step=(max-min)/SEARCH_BRANCH_FACTOR;
  }
  if(step<=0) step = 1;
  int best=max;
  double bestVal = std::numeric_limits<double>::max();

  //scan the search space
  for(int n=min; n<max+step; n+=step){
    tunable.setValue(n);
    double avg = runTrial();
//     printf("%d %.6lf\n", n, avg);
    if(avg<=bestVal){
      bestVal=avg;
      best=n;
    }
  }

  if(step>1){
    int newStep = step/SEARCH_BRANCH_FACTOR;
    int newMin = best-step;
    int newMax = best+step;
    if(newMin<min) newMin = min;
    if(newMax>max) newMax = max;
    return optimizeParameter(tunable, newMin, newMax, newStep);
  }else{
    tunable.setValue(best);
    return bestVal;
  }
}

bool petabricks::PetabricksRuntime::isTrainingRun(){
  _needTraingingRun = true;
  return _isTrainingRun;
}

void petabricks::PetabricksRuntime::setIsTrainingRun(bool b){
  _isTrainingRun=b;
}

void petabricks::PetabricksRuntime::abort(){
#ifdef GRACEFUL_ABORT
  DynamicScheduler::instance().abortBegin();
#else
  JASSERT(false).Text("PetabricksRuntime::abort() called");
#endif
}
void petabricks::PetabricksRuntime::runMultigridAutotuneMode(){
  std::string s1 = "Poisson2D_Inner_Prec1_1";
  std::string s2 = "Poisson2D_Inner_Prec2_1";
  std::string s3 = "Poisson2D_Inner_Prec3_1";
  std::string s4 = "Poisson2D_Inner_Prec4_1";
  std::string s5 = "Poisson2D_Inner_Prec5_1";
  Autotuner at1(*this, _main, s1, std::vector<std::string>());
  Autotuner at2(*this, _main, s2, std::vector<std::string>());
  Autotuner at3(*this, _main, s3, std::vector<std::string>());
  Autotuner at4(*this, _main, s4, std::vector<std::string>());
  Autotuner at5(*this, _main, s5, std::vector<std::string>());

  jalib::JTunableReverseMap m = jalib::JTunableManager::instance().getReverseMap();
  jalib::JTunable* prec_case = m["prec_case"];
  JASSERT(prec_case != 0);

  jalib::JTunable* temp;
  for (int level = 0; level < 30; level++) {
    temp = m["levelTrained__" + jalib::XToString(level)];
    temp->setValue(0);
  }

  Autotuner *at6 = 0, *at7 = 0, *at8 = 0, *at9 = 0, *at10 = 0;
  jalib::JTunable* run_fullmg_flag = NULL;
  if (FULL_MULTIGRID_FLAG) {
    std::string s6 = "FullPoisson2D_Inner_Prec1_1";
    std::string s7 = "FullPoisson2D_Inner_Prec2_1";
    std::string s8 = "FullPoisson2D_Inner_Prec3_1";
    std::string s9 = "FullPoisson2D_Inner_Prec4_1";
    std::string s10 = "FullPoisson2D_Inner_Prec5_1";
    at6 = new Autotuner(*this, _main, s6, std::vector<std::string>());
    at7 = new Autotuner(*this, _main, s7, std::vector<std::string>());
    at8 = new Autotuner(*this, _main, s8, std::vector<std::string>());
    at9 = new Autotuner(*this, _main, s9, std::vector<std::string>());
    at10 = new Autotuner(*this, _main,s10,std::vector<std::string>());

    run_fullmg_flag = m["run_fullmg_flag"];
    JASSERT(run_fullmg_flag != 0);
  }

  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n*=2){
    setSize(n + 1);

    if (FULL_MULTIGRID_FLAG) {
      run_fullmg_flag->setValue(0);
    }

    prec_case->setValue(1);
    at1.trainOnce();
    prec_case->setValue(2);
    at2.trainOnce();
    prec_case->setValue(3);
    at3.trainOnce();
    prec_case->setValue(4);
    at4.trainOnce();
    prec_case->setValue(5);
    at5.trainOnce();

    if (FULL_MULTIGRID_FLAG) {
      run_fullmg_flag->setValue(1);

      prec_case->setValue(1);
      at6->trainOnce();
      prec_case->setValue(2);
      at7->trainOnce();
      prec_case->setValue(3);
      at8->trainOnce();
      prec_case->setValue(4);
      at9->trainOnce();
      prec_case->setValue(5);
      at10->trainOnce();
    }

    saveConfig();
  }

}

int petabricks::petabricksMain(int argc, const char** argv){
  PetabricksRuntime runtime(argc, argv, petabricksMainTransform());
  return runtime.runMain(argc,argv);
}



