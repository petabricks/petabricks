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
#include "petabricksruntime.h"

#include "autotuner.h"
#include "dynamicscheduler.h"
#include "dynamictask.h"
#include "testisolation.h"

#include "common/jargs.h"
#include "common/jfilesystem.h"
#include "common/jtimer.h"
#include "common/jtunable.h"

#include <algorithm>
#include <limits>
#include <iostream>
#include <math.h>

//these must be declared in the user code
petabricks::PetabricksRuntime::Main* petabricksMainTransform();
petabricks::PetabricksRuntime::Main* petabricksFindTransform(const std::string& name);

static bool _isRunning = false;
static bool _isTrainingRun = false;
static bool _needTraingingRun = false;

static std::string CONFIG_FILENAME;
static int GRAPH_MIN=1;
static int GRAPH_MAX=4096;
static double GRAPH_MAX_SEC=std::numeric_limits<double>::max();
static int  GRAPH_STEP=1;
static bool GRAPH_EXP=false;
static int  GRAPH_TRIALS=1;
static int  GRAPH_SMOOTHING=0;
static int  SEARCH_BRANCH_FACTOR=8;
static bool DUMPTIMING=false;
static bool ACCURACY=false;
static bool FORCEOUTPUT=false;
static bool ACCTRAIN=false;
static bool ISOLATION=true;
static int OFFSET=0;
std::vector<std::string> txArgs;
static std::string ATLOG;


static int _incN(int n){
  if(GRAPH_EXP){
    JASSERT(GRAPH_STEP>1)(GRAPH_STEP);
    return GRAPH_STEP*n;
  }else{
    return GRAPH_STEP+n;
  }
}

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

std::string autotunetx;
std::vector<std::string> autotunesites;
std::vector<std::string> autotunecutoffs;
std::string graphParam;

std::vector<double> theTimings;
std::vector<double> theAccuracies;

JTUNABLE(worker_threads,   8, MIN_NUM_WORKERS, MAX_NUM_WORKERS);

namespace{//file local
  typedef jalib::JTunableManager TunableManager;

  void _dumpStats(std::ostream& o, const std::vector<double>& _data){
    if(_data.empty()) return;
    std::vector<double> data = _data;
    std::sort(data.begin(), data.end());

    double sum = 0;
    for(size_t i=0; i<data.size(); ++i)
      sum += data[i];
    double mean = sum / (double) data.size();

    double variance = 0;
    for(size_t i=0; i<data.size(); ++i)
      variance += (data[i]-mean) * (data[i]-mean);
    variance /= (double) data.size();

    double mid = (data.size()-1.0)/2.0;
    double median = (data[(size_t)ceil(mid)] + data[(size_t)floor(mid)]) / 2.0;

    o.precision(15);
    o << " count=\""    << data.size()         << '"'
      << " average=\""  << mean                << '"'
      << " total=\""    << sum                 << '"'
      << " min=\""      << data[0]             << '"'
      << " max=\""      << data[data.size()-1] << '"'
      << " median=\""   << median              << '"'
      << " variance=\"" << variance            << '"'
      << " stddev=\""   << sqrt(variance)      << '"';
  }
}


petabricks::PetabricksRuntime::PetabricksRuntime(int argc, const char** argv, Main* m)
  : _main(m)
  , _randSize(-1)
  , _rv(0)
{
  if(m) _mainName = m->name();
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
  }
  
  //seed the random number generator 
  if(! args.param("fixedrandom").help("don't seed the random number generator")){
    srand48(jalib::JTime::now().usec());
  }
  
  args.param("accuracy",  ACCURACY).help("print out accuracy of answer");
  args.param("time",      DUMPTIMING).help("print timing results in xml format");
  args.param("force-output", FORCEOUTPUT).help("also write copies of outputs to stdout");
  
  args.param("threads", worker_threads).help("number of threads to use");

  if(args.needHelp())
    std::cerr << std::endl << "ALTERNATE EXECUTION MODES:" << std::endl;

  if(args.param("n", _randSize).help("generate a random input of the given size")){
    MODE=MODE_RUN_RANDOM;//default mode
  }else{
    MODE=MODE_RUN_IO;//default mode
  }

  //figure out what mode we are in
  if(args.param("autotune").help("run the genetic autotuner at a given choice site")){
    MODE=MODE_AUTOTUNE_GENETIC;
    GRAPH_EXP=true;
    GRAPH_STEP=2;
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
  if(args.param("resolution").help("print out the timer resolution an exit")){
    std::cout << jalib::JTime::resolution() << std::endl;
    MODE=MODE_ABORT;
  }
  if(args.needHelp()){
    MODE=MODE_HELP;
  }
  
  if(args.needHelp())
    std::cerr << std::endl << "OPTIONS FOR ALTERNATE EXECUTION MODES:" << std::endl;
  
  args.param("autotune-transform", autotunetx).help("transform name to tune in --autotune mode");
  args.param("autotune-site",      autotunesites).help("choice sites to tune in --autotune mode");
  args.param("autotune-tunable",   autotunecutoffs).help("additional cutoff tunables to tune in --autotune mode");
  args.param("autotune-log",   ATLOG).help("log autotuner actions to given filename prefix");
  args.param("acctrain",  ACCTRAIN).help("retrain for accuracy requirements in -n mode");
  args.param("min",       GRAPH_MIN).help("minimum input size for graph/autotuning");
  args.param("max",       GRAPH_MAX).help("maximum input size for graph/autotuning");
  args.param("step",      GRAPH_STEP).help("step size for graph/autotuning");
  args.param("exp",       GRAPH_EXP).help("grow input size exponentially in graph/autotuning mode");
  args.param("trials",    GRAPH_TRIALS).help("number of times to run each data point in graph/autotuning (averaged)");
  args.param("smoothing", GRAPH_SMOOTHING).help("smooth graphs by also running smaller/larger input sizes");
  args.param("offset",    OFFSET).help("size to add to N for each trial");
  args.param("max-sec",   GRAPH_MAX_SEC).help("stop graphs/autotuning after algorithm runs too slow");
  args.param("isolation", ISOLATION).help("don't run timing tests in a forked subprocess");

  args.finishParsing(txArgs);
  
  //startup requested number of threads
  if(MODE!=MODE_GRAPH_THREADS && MODE!=MODE_ABORT){
    JASSERT(worker_threads>=1)(worker_threads);
    DynamicScheduler::cpuScheduler().startWorkerThreads(worker_threads);
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
  DynamicScheduler::cpuScheduler().shutdown();
}

void petabricks::PetabricksRuntime::saveConfig()
{
  //save config to disk
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0){
    tm.save(CONFIG_FILENAME);
  }
}


int petabricks::PetabricksRuntime::runMain(){
  switch(MODE){
    case MODE_RUN_IO:
      runNormal();
      break;
    case MODE_RUN_RANDOM:
      runTrial(GRAPH_MAX_SEC, ACCTRAIN);
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
      runAutotuneMode();
      break;
    case MODE_AUTOTUNE_PARAM:
      optimizeParameter(graphParam);
      break;
    case MODE_ABORT:
    case MODE_HELP:
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

  if(ACCURACY || DUMPTIMING) std::cout << "<root>\n  <stats>\n";
  if(ACCURACY){
    std::cout << "    <accuracy";
    _dumpStats(std::cout, theAccuracies);
    std::cout << " />\n";
  }
  if(DUMPTIMING){
    std::cout << "    <timing";
    _dumpStats(std::cout, theTimings);
    std::cout << " />\n";
  }
  if(ACCURACY || DUMPTIMING) std::cout << "  </stats>\n</root>\n" << std::flush;

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
    double t = jalib::maxval<double>();
    main.read(txArgs);
    try{
      DummyTestIsolation ti;
      t = computeWrapper(ti);
    }catch(...){
      UNIMPLEMENTED();
    }
    if(t>=0 && t<jalib::maxval<double>()/2.0){
      main.write(txArgs);
    }else{
      _rv=66;
    }
  }
}

static const char* _uniquifyatlogname() {
  static int i = 0;
  static std::string buf;
  buf = ATLOG+"."+jalib::XToString(i++)+".log";
  return buf.c_str();
}
  
void petabricks::PetabricksRuntime::runAutotuneMode(){
  AutotunerList tuners;
  Main* tx = _main;
  bool inContext = true;
  if(!autotunetx.empty()){
    tx = petabricksFindTransform(autotunetx);
    JASSERT(tx!=NULL)(autotunetx).Text("unknown transform name");
    if(tx != _main) inContext=false;
  }
  if(autotunesites.empty()){
    autotunesites.push_back("0");
  }
  for(; tx!=NULL; tx=tx->nextTemplateMain()){
    for( std::vector<std::string>::const_iterator site=autotunesites.begin()
       ; site!=autotunesites.end()
       ; ++site)
    {
      Main* ctx=_main;
      if(inContext || tx->isVariableAccuracy())
        ctx = tx;

      std::string pfx = std::string(tx->name())+"_"+(*site);

      if(*site == "-1") pfx = "";

      if(ATLOG.empty())
        tuners.push_back(new Autotuner(*this, ctx, pfx, autotunecutoffs));
      else
        tuners.push_back(new Autotuner(*this, ctx, pfx, autotunecutoffs, _uniquifyatlogname()));
    }
  }

  runAutotuneLoop(tuners);
}
void petabricks::PetabricksRuntime::runAutotuneLoop(const AutotunerList& tuners){
  Main* old = _main;
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n=_incN(n)){
    setSize(n);
    bool overtime=false;
    for(size_t i=0; i<tuners.size(); ++i){
      _main = tuners[i]->main();
      overtime |= ! tuners[i]->trainOnce(GRAPH_MAX_SEC);
    }
    saveConfig();
    if(overtime) 
      break;
  }
  _main=old;
}

void petabricks::PetabricksRuntime::runGraphMode(){
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n=_incN(n)){
    setSize(n);
    double avg = runTrial(GRAPH_MAX_SEC, ACCTRAIN);
    if(avg<std::numeric_limits<double>::max())
      printf("%d %.6f\n", _randSize, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

void petabricks::PetabricksRuntime::runGraphParamMode(const std::string& param){
  jalib::JTunable* tunable = jalib::JTunableManager::instance().getReverseMap()[param];
  JASSERT(tunable!=0)(param).Text("parameter not found");
  GRAPH_MIN = std::max(GRAPH_MIN, tunable->min());
  GRAPH_MAX = std::min(GRAPH_MAX, tunable->max());
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n=_incN(n)){
    tunable->setValue(n);
    double avg = runTrial(GRAPH_MAX_SEC, ACCTRAIN);
    if(avg<std::numeric_limits<double>::max())
      printf("%d %.6lf\n", n, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

void petabricks::PetabricksRuntime::runGraphParallelMode() {
  GRAPH_MIN = std::max(GRAPH_MIN, worker_threads.min());
  GRAPH_MAX = std::min(GRAPH_MAX, worker_threads.max());
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n=_incN(n)){
    worker_threads.setValue(n);
    DynamicScheduler::cpuScheduler().startWorkerThreads(worker_threads);
    double avg = runTrial(GRAPH_MAX_SEC, ACCTRAIN);
    if(avg<std::numeric_limits<double>::max())
      printf("%d %.6lf\n", n, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}
double petabricks::PetabricksRuntime::runTrial(double thresh, bool train){
  SubprocessTestIsolation sti(thresh);
  static DummyTestIsolation dti;
  TestIsolation* ti;
  if(ISOLATION) ti = &sti;
  else          ti = &dti;
  return runTrial(*ti, train);
}

double petabricks::PetabricksRuntime::runTrial(TestIsolation& ti, bool train){
  JASSERT(_randSize>0)(_randSize).Text("'--n=NUMBER' is required");
  int origN = _randSize;
  std::vector<double> rslts;
  rslts.reserve(2*GRAPH_SMOOTHING+1);
  //JTRACE("runtrial")(_randSize)(GRAPH_SMOOTHING)(OFFSET);
  for( int n =  origN-GRAPH_SMOOTHING+OFFSET
     ; n <= origN+GRAPH_SMOOTHING+OFFSET
     ; ++n)
  {
    _randSize=n;
    double t = 0;
    for(int z=0;z<GRAPH_TRIALS; ++z){
      _main->reallocate(n);
      _main->randomize();
      if(z==0 && train){
        t += trainAndComputeWrapper(ti); // first trial can train
      }else{
        t += computeWrapper(ti); // rest of trials cant train
      }
    }
    rslts.push_back(t/GRAPH_TRIALS);//record average
  }
  std::sort(rslts.begin(), rslts.end());
  _randSize=origN;
  return rslts[GRAPH_SMOOTHING];
}

double petabricks::PetabricksRuntime::trainAndComputeWrapper(TestIsolation& ti){
  try {
    _isTrainingRun = true;
    double t=computeWrapper(ti);
    _isTrainingRun = false;
    return t;
  }catch(ComputeRetryException e) {
    _isTrainingRun = false;
    return computeWrapper(ti);
  }
}

double petabricks::PetabricksRuntime::computeWrapper(TestIsolation& ti){
  double v, acc=jalib::maxval<double>();
  if(ti.beginTest(worker_threads)){
    try {
      if(_isTrainingRun && _main->isVariableAccuracy()){
        variableAccuracyTrainingLoop(ti);
      }
      _needTraingingRun = false;//reset flag set by isTrainingRun()
      _isRunning = true;
      jalib::JTime begin=jalib::JTime::now();
      _main->compute();
      jalib::JTime end=jalib::JTime::now();
      _isRunning = false;
      
      if(_needTraingingRun && _isTrainingRun){
        v=-1;
      }else{
        v=end-begin;
      }
      if(ACCURACY){
        ti.disableTimeout();
        acc=_main->accuracy();
      }
    } catch(petabricks::DynamicScheduler::AbortException) {
      v=jalib::maxval<double>();
      _isRunning = false;
    } catch(...) {
      UNIMPLEMENTED();
      throw;
    }
    ti.endTest(v,acc);
  }else{
    ti.recvResult(v,acc);
  }
    
  if(v<0)        throw ComputeRetryException();
  if(DUMPTIMING) theTimings.push_back(v);
  if(ACCURACY)   theAccuracies.push_back(acc);
  return v;
}
  
void petabricks::PetabricksRuntime::variableAccuracyTrainingLoop(TestIsolation& ti){
  typedef MATRIX_ELEMENT_T ElementT;
  ElementT best   = jalib::minval<ElementT>();
  ElementT target = _main->accuracyTarget();
  TunableListT tunables = _main->accuracyVariables(_randSize);
  //reset tunables to min+1
  tunables.resetMinAll(1);
  int triesLeft = 0;

  for(int i=0; true;++i){
    reallocate();
    _isRunning = true;
    ti.restartTimeout();
    _main->compute();
    ti.disableTimeout();
    _isRunning = false;
    ElementT cur = _main->accuracy();
    if(cur >= target){
      JTRACE("training goal reached")(i)(cur)(target);
      return;
    }
    if(cur>best){
      //improvement
      best=cur;
      triesLeft=ACCIMPROVETRIES;
    }else if(--triesLeft <= 0){
      //no improvement for ACCIMPROVETRIES
      JTRACE("training goal failed, no progress")(i)(cur)(best)(target);
      break;
    }
    //increment tuning var
    if(!tunables.incrementAll()){
      JTRACE("training goal failed, max iterations")(cur)(target);
      break; 
    }
  }
  //failure if we reach here, reset tunables and abort
  tunables.resetMinAll(0);
  reallocate();
  abort();
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
    double avg = runTrial(bestVal, ACCTRAIN);
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

void petabricks::PetabricksRuntime::abort(){
  TestIsolation* master = SubprocessTestIsolation::masterProcess();
  if(master!=NULL){
    master->endTest(jalib::maxval<double>(), jalib::minval<double>());//should abort us
    UNIMPLEMENTED();
  }else{
    DynamicScheduler::cpuScheduler().abort();
  }
}

void petabricks::PetabricksRuntime::untrained(){
  if(_isRunning){
    std::cerr << "ERROR: untrained, accuracy is unknown" << std::endl;
    PetabricksRuntime::abort();
  }
}

int petabricks::petabricksMain(int argc, const char** argv){
  PetabricksRuntime runtime(argc, argv, petabricksMainTransform());
  return runtime.runMain();
}



