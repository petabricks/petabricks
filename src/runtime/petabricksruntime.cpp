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
#include "petabricksruntime.h"

#include "dynamicscheduler.h"
#include "dynamictask.h"
#include "gpudynamictask.h"
#include "gpumanager.h"
#include "petabricks.h"
#include "remotehost.h"
#include "testisolation.h"

#include "common/jargs.h"
#include "common/jfilesystem.h"
#include "common/jtimer.h"
#include "common/jtunable.h"
#include "common/openclutil.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <math.h>
#include <pthread.h>

#include <sys/time.h>
#include <sys/resource.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#if !defined(HAVE_THREADLOCAL)
# undef HAVE_BOOST_RANDOM_HPP
#endif


#ifdef HAVE_BOOST_RANDOM_HPP
# include <boost/random.hpp>
#endif

#ifdef __APPLE__ //Darwin has no __thread support. Lazy for now, just disabling...
#undef HAVE_BOOST_RANDOM_HPP
#endif //__APPLE__

static bool _isRunning = false;
static bool _isTrainingRun = false;
static bool _needTraingingRun = false;

static std::string CONFIG_FILENAME;
static std::string CONFIG_FILENAME_ALT;
static int GRAPH_MIN=1;
static int GRAPH_MAX=4096;
static double GRAPH_MAX_SEC=jalib::maxval<double>();
static double RACE_SPLIT_RATIO=0.5;
static int  GRAPH_STEP=1;
static bool GRAPH_EXP=false;
static int GRAPH_TRIALS=1;
static int GRAPH_TRIALS_MAX=jalib::maxval<int>();
static double GRAPH_TRIALS_SEC=0;
static int GRAPH_SMOOTHING=0;
static int RETRIES=0;
static int SEARCH_BRANCH_FACTOR=8;
static int ACCTRIALS=3;
static bool DUMPTIMING=false;
static bool ACCURACY=false;
static bool FORCEOUTPUT=false;
static bool ACCTRAIN=false;
static bool ISOLATION=true;
static bool HASH=false;
static bool FIXEDRANDOM=false;
static int OFFSET=0;
static int ACCIMPROVETRIES=3;
std::vector<std::string> txArgs;
static std::string ATLOG;
static jalib::Hash theLastHash;
static std::string IOGEN_PFX="tmp_";
static int IOGEN_N=-1;
static double RACE_MULTIPLIER=1;
static double RACE_MULTIPLIER_LOWACC=1;
static double RACE_ACCURACY_TARGET=jalib::minval<double>();

static bool TI_REEXEC = true;
static int REEXECCHILD=-1;

#ifdef REGIONMATRIX_TEST
static std::string HOSTS_FILE="hosts.example";
#else
static std::string HOSTS_FILE="";
#endif
static std::string SLAVE_HOST="";
static int SLAVE_PORT = -1;

#ifdef HAVE_BOOST_RANDOM_HPP
static boost::lagged_fibonacci607& myRandomGen(){
  //ouch... lagged_fibonacci is NOT THREAD SAFE
  static __thread char lf_buf[sizeof(boost::lagged_fibonacci607)];
  static __thread boost::lagged_fibonacci607* lf = NULL;
  if(lf==NULL){
    lf=new (lf_buf) boost::lagged_fibonacci607();
    if(!FIXEDRANDOM){
      lf->seed(jalib::JTime::now().usec());
    }
  }
  return *lf;
}
#endif //HAVE_BOOST_RANDOM_HPP

static void _seedRandom(){
  srand48(jalib::JTime::now().usec());
}

double petabricks::PetabricksRuntime::rand01(){
#ifdef HAVE_BOOST_RANDOM_HPP
  double d = myRandomGen()();
#else
  double d = drand48();
#endif
  return d;
}


static int _incN(int n){
  if(GRAPH_EXP){
    JASSERT(GRAPH_STEP>1)(GRAPH_STEP);
    return GRAPH_STEP*n;
  }else{
    return GRAPH_STEP+n;
  }
}

static int theArgc = 0;
static const char** theArgv = NULL;


static enum {
  MODE_RUN_IO,
  MODE_RUN_RANDOM,
  MODE_IOGEN_CREATE,
  MODE_IOGEN_RUN,
  MODE_GRAPH_INPUTSIZE,
  MODE_GRAPH_PARAM,
  MODE_GRAPH_THREADS,
  MODE_GRAPH_TEMPLATE,
  MODE_RACE_CONFIGS,
  MODE_AUTOTUNE_PARAM,
  MODE_DISTRIBUTED_SLAVE,
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

  double _mean(const std::vector<double>& data){
    double sum = 0;
    for(size_t i=0; i<data.size(); ++i)
      sum += data[i];
    return sum / (double) data.size();
  }

  void _dumpStats(std::ostream& o, const std::vector<double>& _data){
    if(_data.empty()) return;
    std::vector<double> data = _data;
    std::sort(data.begin(), data.end());

    double mean = _mean(data);

    double variance = 0;
    for(size_t i=0; i<data.size(); ++i)
      variance += (data[i]-mean) * (data[i]-mean);
    variance /= (double) data.size();

    double mid = (data.size()-1.0)/2.0;
    double median = (data[(size_t)ceil(mid)] + data[(size_t)floor(mid)]) / 2.0;

    o.precision(15);
    o << " count=\""    << data.size()         << '"'
      << " average=\""  << mean                << '"'
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
  JTIMER_SCOPE(init);
  if(m) _mainName = m->name();
  jalib::JArgs args(argc, argv);

  theArgc = argc;
  theArgv = argv;

  if(args.needHelp())
    std::cerr << "GENERAL OPTIONS:" << std::endl;

  //aliases
  args.alias("cfg", "config");
  args.alias("random", "n");
  args.alias("tx", "transform");
  args.alias("graph", "graph-input");
  args.alias("graph-accuracy", "graph-template");

  //load config from disk
  CONFIG_FILENAME = jalib::Filesystem::GetProgramPath() + ".cfg";
  args.param("config",  CONFIG_FILENAME).help("filename of the program configuration");
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
  args.param("fixedrandom", FIXEDRANDOM).help("don't seed the random number generator");

  if(!FIXEDRANDOM)
    _seedRandom();

  args.param("accuracy",     ACCURACY).help("print out accuracy of answer");
  args.param("time",         DUMPTIMING).help("print timing results in xml format");
  args.param("hash",         HASH).help("print hash of output");
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
  if(args.param("optimize", graphParam).help("autotune a single given config parameter")){
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
  }else if(args.param("graph-template").help("graph run time for each template instance")){
    MODE=MODE_GRAPH_TEMPLATE;
  }else if(args.param("iogen-create", IOGEN_PFX).help("generate a set of random inputs with the given prefix")){
    JASSERT(_randSize>=0).Text("-n=... required");
    MODE=MODE_IOGEN_CREATE;
  }else if(args.param("iogen-run", IOGEN_PFX).help("run a set of random inputs generated with --iogen-create")){
    JASSERT(_randSize<0).Text("-n=... conflicts with --iogen-run");
    MODE=MODE_IOGEN_RUN;
  }else if(args.param("race-with", CONFIG_FILENAME_ALT).help("alternate program configuration")) {
    JASSERT(_randSize>=0).Text("-n=... required");
    JASSERT(jalib::Filesystem::FileExists(CONFIG_FILENAME));
    JASSERT(jalib::Filesystem::FileExists(CONFIG_FILENAME_ALT) || CONFIG_FILENAME_ALT=="None");
    MODE=MODE_RACE_CONFIGS;
  }

  args.param("iogen-n", IOGEN_N);

  //flags that cause aborts
  if(args.param("reset").help("reset the config file to the default state and exit")){
    jalib::JTunableManager::instance().reset();
    MODE=MODE_ABORT;
  }
  if(args.param("name").help("print out the name of the main transform and exit")){
    std::cout << _mainName << std::endl;
    MODE=MODE_ABORT;
  }
  if(args.param("resolution").help("print out the timer resolution and exit")){
    std::cout << jalib::JTime::resolution() << std::endl;
    MODE=MODE_ABORT;
  }
/*#ifdef HAVE_OPENCL
  if(args.param("opencl-info").help("display information about OpenCL devices and exit")){
    if( 0 != OpenCLUtil::init( ) )
      std::cout << "Failed to initialize OpenCL." << std::endl;
    else
      OpenCLUtil::printDeviceList( );

    MODE=MODE_ABORT;
  }
#endif*/
  if(args.needHelp()){
    MODE=MODE_HELP;
  }

  if(args.needHelp())
    std::cerr << std::endl << "OPTIONS FOR ALTERNATE EXECUTION MODES:" << std::endl;

  args.param("autotune-transform", autotunetx).help("transform name to tune in --autotune mode");
  args.param("autotune-site",      autotunesites).help("choice sites to tune in --autotune mode");
  args.param("autotune-tunable",   autotunecutoffs).help("additional cutoff tunables to tune in --autotune mode");
  args.param("autotune-log",   ATLOG).help("log autotuner actions to given filename prefix");
  args.param("acctrain",    ACCTRAIN).help("retrain for accuracy requirements in -n mode");
  args.param("acctrials",   ACCTRIALS).help("number of tests to run when setting accuracy variabls");
  args.param("accimprovetries",   ACCIMPROVETRIES).help("number of increments tried with no accuracy improvement");
  args.param("min",         GRAPH_MIN).help("minimum input size for graph/autotuning");
  args.param("max",         GRAPH_MAX).help("maximum input size for graph/autotuning");
  args.param("step",        GRAPH_STEP).help("step size for graph/autotuning");
  args.param("exp",         GRAPH_EXP).help("grow input size exponentially in graph/autotuning mode");
  args.param("trials",      GRAPH_TRIALS).help("number of times to run each data point in graph/autotuning (averaged)");
  args.param("trials-sec",  GRAPH_TRIALS_SEC).help("keep running trials until total measured time is greater than a given number");
  args.param("trials-max",  GRAPH_TRIALS_MAX).help("maximum number of trials to run per configuration");
  args.param("smoothing",   GRAPH_SMOOTHING).help("smooth graphs by also running smaller/larger input sizes");
  args.param("offset",      OFFSET).help("size to add to N for each trial");
  args.param("max-sec",     GRAPH_MAX_SEC).help("stop graphs/autotuning after algorithm runs too slow");
  args.param("isolation",   ISOLATION).help("don't run timing tests in a forked subprocess");
  args.param("retries",     RETRIES).help("times to retry on test failure");
  args.param("race-multiplier", RACE_MULTIPLIER).help("how much extra time (percentage) the slower racer gets to finish");
  args.param("race-multiplier-lowacc", RACE_MULTIPLIER_LOWACC).help("how much extra time (percentage) the slower racer gets to finish");
  args.param("race-accuracy",    RACE_ACCURACY_TARGET).help("accuracy the second racer must achieve");
  args.param("race-split-ratio", RACE_SPLIT_RATIO).help("how to divide the chip for racing");

  size_t max_memory=0;
  if(args.param("max-memory", max_memory).help("kill the process when it tries to use this much memory")) {
    if(max_memory>0) {
      struct rlimit tmp;
      tmp.rlim_cur = max_memory;
      tmp.rlim_max = max_memory;
      JASSERT(setrlimit(RLIMIT_AS, &tmp)==0);
    }
  }


  args.param("hosts",      HOSTS_FILE).help("list of hostnames in distributed computation");
  args.param("slave-host", SLAVE_HOST);
  args.param("slave-port", SLAVE_PORT);


  args.param("reexecchild", REEXECCHILD);
  args.param("tireexec", TI_REEXEC).help("toggle if TestIsolation should reexecute the process");

  args.finishParsing(txArgs);

  if(SLAVE_HOST != "" && SLAVE_PORT>0) {
    ISOLATION=false;
    MODE=MODE_DISTRIBUTED_SLAVE;
    JTRACE("slave");
  }else if(HOSTS_FILE!=""){
    ISOLATION=false;
    JTRACE("parent");
    spawnDistributedNodes(argc, argv);
  }

  switch(MODE){
    case MODE_RUN_RANDOM:
    case MODE_GRAPH_INPUTSIZE:
    case MODE_GRAPH_PARAM:
    case MODE_GRAPH_THREADS:
    case MODE_GRAPH_TEMPLATE:
    case MODE_AUTOTUNE_PARAM:
    case MODE_IOGEN_RUN:
      if(ISOLATION)
        break;
      //fall through
    case MODE_IOGEN_CREATE:
    case MODE_RUN_IO:
    case MODE_DISTRIBUTED_SLAVE:
      //startup requested number of threads
      if(MODE!=MODE_GRAPH_THREADS && MODE!=MODE_ABORT){
        startWorkerThreads(worker_threads);
      }
    case MODE_ABORT:
    case MODE_HELP:
    case MODE_RACE_CONFIGS:
      break;
  }

  if(MODE==MODE_HELP){
    //normal mode with no options will print usage
    txArgs.clear();
    MODE=MODE_RUN_IO;
    std::cerr << std::endl;
  }

  JASSERT(MODE==MODE_RUN_IO||MODE==MODE_DISTRIBUTED_SLAVE||txArgs.size()==0)(txArgs.size())
    .Text("too many arguments");
}

void petabricks::PetabricksRuntime::startWorkerThreads(int worker_threads) {
  JTIMER_SCOPE(startworkers);
  GpuManager::start();
  JASSERT(worker_threads>=1)(worker_threads);
  DynamicScheduler::cpuScheduler().startWorkerThreads(worker_threads);
  _petabricksInit();
}

void petabricks::PetabricksRuntime::spawnDistributedNodes(int argc, const char** argv) {
  RemoteHostDB& db = RemoteHostDB::instance();
  std::ifstream fp(HOSTS_FILE.c_str());
  JASSERT(fp.is_open())(HOSTS_FILE).Text("failed to open file");
  std::string line;
  bool hadlocal = false;
  while(getline(fp, line)){
    std::string dat,com;
    jalib::SplitFirst(dat, com, line, '#');
    dat=jalib::StringTrim(dat);

    if(dat!="" && dat!="localhost") {
      db.remotefork(dat.c_str(), argc, argv, "--slave-host", "--slave-port");
      db.accept(dat.c_str());
    }

    if(dat == "localhost") {
      if(hadlocal) {
        db.remotefork(NULL, argc, argv, "--slave-host", "--slave-port");
        db.accept(dat.c_str());
      }
      hadlocal=true;
    }

    JASSERT(hadlocal);
  }

  db.setupConnectAllPairs();

  for(int i=REMOTEHOST_THREADS; i>0; --i) {
    db.spawnListenThread();
  }
}
void petabricks::PetabricksRuntime::distributedSlaveLoop() {
  RemoteHostDB& db = RemoteHostDB::instance();
  db.connect(SLAVE_HOST.c_str(), SLAVE_PORT);
  db.host(0)->setupLoop(db);
  for(int i=REMOTEHOST_THREADS; i>0; --i) {
    db.spawnListenThread();
  }
  JTRACE("slave loop starting");
  WorkerThread* self = WorkerThread::self();
  self->mainLoop();
}

petabricks::PetabricksRuntime::~PetabricksRuntime()
{
  saveConfig();
  _petabricksCleanup();
  GpuManager::shutdown();
  DynamicScheduler::cpuScheduler().shutdown();
  RemoteHostDB().instance().shutdown();
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
  JTIMER_SCOPE(runMain);

  switch(MODE){
    case MODE_RUN_IO:
      runNormal();
      break;
    case MODE_IOGEN_CREATE:
      iogenCreate(iogenFiles(IOGEN_PFX));
      break;
    case MODE_IOGEN_RUN:
      iogenRun(iogenFiles(IOGEN_PFX));
      break;
    case MODE_RACE_CONFIGS:
      raceConfigs(_randSize);
      return _rv;
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
    case MODE_GRAPH_TEMPLATE:
      runGraphTemplate();
      break;
    case MODE_GRAPH_THREADS:
      runGraphParallelMode();
      break;
    case MODE_AUTOTUNE_PARAM:
      optimizeParameter(graphParam);
      break;
    case MODE_DISTRIBUTED_SLAVE:
      distributedSlaveLoop();
      return 0;
    case MODE_ABORT:
    case MODE_HELP:
      break;
  }


  if(FORCEOUTPUT && _rv==0){
    JTIMER_SCOPE(forceoutput);
    std::vector<std::string> tmp;
    for(int i=_main->numInputs(); i-->0;)
      tmp.push_back(DEVNULL);
    for(int i=_main->numOutputs(); i-->0;)
      tmp.push_back("-");
    _main->writeOutputs(tmp);
  }

  ACCURACY=!theAccuracies.empty();
  DUMPTIMING=!theTimings.empty();
  if(ACCURACY || DUMPTIMING || HASH) std::cout << "<root>\n  <stats>\n";
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
  if(HASH){
    std::cout << "    <outputhash value=\"0x" << theLastHash << "\" />\n";
  }
  if(ACCURACY || DUMPTIMING || HASH) std::cout << "  </stats>\n</root>\n" << std::flush;

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
    main.readInputs(txArgs);
    try{
      DummyTestIsolation ti;
      t = computeWrapper(ti);
    }catch(...){
      UNIMPLEMENTED();
    }
    if(t>=0 && t<jalib::maxval<double>()/2.0){
      main.writeOutputs(txArgs);
    }else{
      _rv=66;
    }
  }
}

std::vector<std::string>  petabricks::PetabricksRuntime::iogenFiles(const std::string& pfx){
  std::vector<std::string> tmp;
  for(int i=_main->numInputs(); i-->0;)
    tmp.push_back(pfx + "_in" + jalib::XToString(i) + ".dat");
  for(int i=_main->numOutputs(); i-->0;)
    tmp.push_back(pfx + "_out" + jalib::XToString(i) + ".dat");
  return tmp;
}

void petabricks::PetabricksRuntime::iogenCreate(const std::vector<std::string>& files){
  _main->reallocate(_randSize);
  _main->randomize();
  _main->writeInputs(files);
  _main->writeOutputs(files);
}

void petabricks::PetabricksRuntime::iogenRun(const std::vector<std::string>& files){
  SubprocessTestIsolation sti(GRAPH_MAX_SEC);
  static DummyTestIsolation dti;
  TestIsolation* ti;
  if(ISOLATION) ti = &sti;
  else          ti = &dti;
  //computeWrapper(*ti, -1, -1, &files);
  runTrialFromFile(*ti, &files);
}

void petabricks::PetabricksRuntime::runGraphMode(){
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n=_incN(n)){
    setSize(n);
    double avg = runTrial(GRAPH_MAX_SEC, ACCTRAIN);
    if(avg<std::numeric_limits<double>::max()){
      if(theAccuracies.empty()){
        printf("%d %.6f\n", n, avg);
      }else{
        printf("%d %.6f %.6f\n", n, avg, _mean(theAccuracies));
        theAccuracies.clear();
      }
    }
    fflush(stdout);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

void petabricks::PetabricksRuntime::runGraphTemplate(){
  int i=0;
  for(; _main!=NULL; _main=_main->nextTemplateMain(),++i){
    double avg = runTrial(GRAPH_MAX_SEC, ACCTRAIN);
    if(theAccuracies.empty()){
      printf("%d %.6f\n", i, avg);
    }else{
      printf("%d %.6f %.6f %.6f\n", i, avg, _main->accuracyTarget(), _mean(theAccuracies));
      theAccuracies.clear();
    }
  }
}

void petabricks::PetabricksRuntime::runGraphParamMode(const std::string& param){
  jalib::JTunable* tunable = jalib::JTunableManager::instance().getReverseMap()[param];
  JASSERT(tunable!=0)(param).Text("parameter not found");
  GRAPH_MIN = std::max(GRAPH_MIN, tunable->min().i());
  GRAPH_MAX = std::min(GRAPH_MAX, tunable->max().i());
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n=_incN(n)){
    tunable->setValue(n);
    double avg = runTrial(GRAPH_MAX_SEC, ACCTRAIN);
    if(avg<std::numeric_limits<double>::max()){
      if(theAccuracies.empty()){
        printf("%d %.6f\n", n, avg);
      }else{
        printf("%d %.6f %.6f\n", n, avg, _mean(theAccuracies));
        theAccuracies.clear();
      }
    }
    fflush(stdout);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

void petabricks::PetabricksRuntime::runGraphParallelMode() {
  GRAPH_MIN = std::max(GRAPH_MIN, worker_threads.min().i());
  GRAPH_MAX = std::min(GRAPH_MAX, worker_threads.max().i());
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n=_incN(n)){
    worker_threads.setValue(n);
    if(!ISOLATION)
      DynamicScheduler::cpuScheduler().startWorkerThreads(worker_threads);
    double avg = runTrial(GRAPH_MAX_SEC, ACCTRAIN);
    if(avg<std::numeric_limits<double>::max())
      printf("%d %.6lf\n", n, avg);
    fflush(stdout);
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
  if(GRAPH_SMOOTHING==0){
    _randSize+=OFFSET;
    double rv=runTrialNoSmoothing(ti, train);
    _randSize-=OFFSET;
    return rv;
  }else{
    int origN = _randSize;
    std::vector<double> rslts;
    rslts.reserve(2*GRAPH_SMOOTHING+1);
    for( int n =  origN-GRAPH_SMOOTHING+OFFSET
       ; n <= origN+GRAPH_SMOOTHING+OFFSET
       ; ++n)
    {
      _randSize=n;
      rslts.push_back(runTrialNoSmoothing(ti,train));
      _randSize=origN;
    }
    std::sort(rslts.begin(), rslts.end());
    return rslts[GRAPH_SMOOTHING];
  }
}

double petabricks::PetabricksRuntime::runTrialNoSmoothing(TestIsolation& ti, bool train){
  JASSERT(_randSize>0)(_randSize).Text("'--n=NUMBER' is required");
  return runMultipleTrials(ti, _randSize, train, NULL);
}

void petabricks::PetabricksRuntime::runTrialFromFile(TestIsolation& ti, const std::vector<std::string>* files){
  runMultipleTrials(ti, -1, false, files);
}

double petabricks::PetabricksRuntime::runMultipleTrials(TestIsolation& ti, int n, bool train, const std::vector<std::string>* files){
  JASSERT((n != -1) ^ (files != NULL)).Text("Either specify a size for random generation or a file to load");
  JASSERT(GRAPH_TRIALS>=1).Text("invalid --trials");
  JASSERT(GRAPH_TRIALS_SEC>=0).Text("invalid --trials-sec");
  JASSERT(GRAPH_TRIALS_MAX>=GRAPH_TRIALS).Text("invalid --trials and --trials-max");
  double total = 0;
  int count = 0;
  do {
    if(count==0 && train)
      total += trainAndComputeWrapper(ti, n); // first trial can train
    else
      total += computeWrapper(ti, n, -1, files); // rest of trials cant train
    ++count;

    if(total>=jalib::maxval<double>())
      return jalib::maxval<double>();

  } while( count<GRAPH_TRIALS || ( total<GRAPH_TRIALS_SEC && count<GRAPH_TRIALS_MAX) );

  return total/count;
}

double petabricks::PetabricksRuntime::trainAndComputeWrapper(TestIsolation& ti, int n){
  try {
    _isTrainingRun = true;
    double t=computeWrapper(ti, n);
    _isTrainingRun = false;
    return t;
  }catch(ComputeRetryException e) {
    _isTrainingRun = false;
    return computeWrapper(ti, n);
  }
}

double petabricks::PetabricksRuntime::raceConfigs(int n, const std::vector<std::string>* files, int /*retries*/) {
  TestResult aresult;
  TestResult bresult;
  SubprocessTestIsolation ati(GRAPH_MAX_SEC);
  SubprocessTestIsolation bti(GRAPH_MAX_SEC);
  TunableManager& tm = TunableManager::instance();

  int ta = (int)( worker_threads * RACE_SPLIT_RATIO );
  int tb = worker_threads - ta;
  JTRACE("race split")(ta)(tb);
  try {
    loadTestInput(n, files);
    if(CONFIG_FILENAME_ALT != "None") {
      JASSERT(REEXECCHILD<0);
      if(ati.beginTest(worker_threads/2, REEXECCHILD)) {
        tm.load(CONFIG_FILENAME);
        _main->reallocate(std::max(IOGEN_N, n));
        computeWrapperSubproc(ati, -1, aresult, NULL);
        ati.endTest(aresult);
      } else if(bti.beginTest(worker_threads/2, REEXECCHILD)) {
        tm.load(CONFIG_FILENAME_ALT);
        _main->reallocate(std::max(IOGEN_N, n));
        computeWrapperSubproc(bti, -1, bresult, NULL);
        bti.endTest(bresult);
      }else{
        SubprocessTestIsolation::recvFirstResult(ati, aresult, bti, bresult);
      }
    } else {
      if(ati.beginTest(worker_threads, REEXECCHILD)) {
        tm.load(CONFIG_FILENAME);
        _main->reallocate(std::max(IOGEN_N, n));
        computeWrapperSubproc(ati, -1, aresult, NULL);
        ati.endTest(aresult);
      }else{
        ati.recvResult(aresult);
      }
    }
  } catch(...) {
    UNIMPLEMENTED();
  }
  std::cout.precision(15);
  std::cout << "<raceresult>" << std::endl;
  aresult.writexml(std::cout, "0");
  std::cout << std::endl;
  bresult.writexml(std::cout, "1");
  std::cout << std::endl;
  std::cout << "</raceresult>" << std::endl;
  return std::min(aresult.time, bresult.time);
}

double petabricks::PetabricksRuntime::computeWrapper(TestIsolation& ti, int n, int retries,
                                                     const std::vector<std::string>* files){
  TestResult result;
  try{
    if(ti.beginTest(worker_threads, REEXECCHILD)){
      computeWrapperSubproc(ti, n, result, files);
      ti.endTest(result);
    }else{
      ti.recvResult(result);
    }
  }catch(TestIsolation::UnknownTestFailure e){
    if(retries<0) retries=RETRIES;//from cmd line
    if(retries>0){
      std::cerr << "WARNING: test aborted unexpectedly (" << retries << " retries left)" << std::endl;
      return computeWrapper(ti, n, retries-1, files);
    }else{
      //std::cerr << "ERROR: test aborted unexpectedly" << std::endl;
      exit(e.rv);
    }
  }
  if(result.time<0)  throw ComputeRetryException();
  if(DUMPTIMING)     theTimings.push_back(result.time);
  if(ACCURACY)       theAccuracies.push_back(result.accuracy);
  if(HASH)           theLastHash = result.hash;
  return result.time;
}

void petabricks::PetabricksRuntime::loadTestInput(int n, const std::vector<std::string>* files) {
  if(ISOLATION && TI_REEXEC && REEXECCHILD<0) {
    //skip input loading when a child process will reexe
    return;
  }
  if(n>0){
    JTRACE("generating input")(n);
    JASSERT(files==NULL);
    JTIMER_SCOPE(randomize);
    JASSERT(n==_randSize);
    _main->deallocate();
    _main->reallocate(n);
    _main->randomize();
  }else if(files!=NULL){
    _main->readInputs(*files);
    _main->readOutputs(*files);
    if(IOGEN_N>0){
      _main->reallocate(IOGEN_N);
    }
  }
}

void petabricks::PetabricksRuntime::computeWrapperSubproc(TestIsolation& ti,
                                                          int n,
                                                          TestResult& result,
                                                          const std::vector<std::string>* files){
  try {
    ti.disableTimeout();
    if(_isTrainingRun && _main->isVariableAccuracy()){
      variableAccuracyTrainingLoop(ti);
    }
    if(n>0 || files!=NULL){
      loadTestInput(n, files);
    }
    _needTraingingRun = false;//reset flag set by isTrainingRun()
    _isRunning = true;
    ti.restartTimeout();
    jalib::JTime begin=jalib::JTime::now();
    _main->compute();
    jalib::JTime end=jalib::JTime::now();
    ti.disableTimeout();
    _isRunning = false;

    if(_needTraingingRun && _isTrainingRun){
      result.time=-1;
    }else{
      result.time=end-begin;
    }
    if(ACCURACY){
      result.accuracy = _main->accuracy();
    }
    if(HASH){
      jalib::HashGenerator hg;
      _main->hash(hg);
      result.hash = hg.final();
    }
  } catch(petabricks::DynamicScheduler::AbortException) {
    result.time=jalib::maxval<double>();
    _isRunning = false;
  } catch(...) {
    UNIMPLEMENTED();
    throw;
  }
}

void petabricks::PetabricksRuntime::variableAccuracyTrainingLoop(TestIsolation& /*ti*/){
  UNIMPLEMENTED();
  /*
  ti.disableTimeout();
  TunableListT tunables = _main->accuracyVariables(_randSize);
  std::vector<int> itersNeeded;

  for(int z=0; z<ACCTRIALS; ++z){
    reallocate();
    _main->randomizeInputs();
    int i=variableAccuracyTrainingLoopInner(ti);
    if(i>0) itersNeeded.push_back(i);
  }

  if(itersNeeded.empty())
    abort();

  std::sort(itersNeeded.begin(), itersNeeded.end());

  //median
  int i= itersNeeded[ (itersNeeded.size()+1)/2 ];

  tunables.resetMinAll(i);
  reallocate();
  ti.restartTimeout();
  */
}
int petabricks::PetabricksRuntime::variableAccuracyTrainingLoopInner(TestIsolation& /*ti*/){
  UNIMPLEMENTED();
  return 0;
  /*
  typedef MATRIX_ELEMENT_T ElementT;
  ElementT best   = jalib::minval<ElementT>();
  ElementT target = _main->accuracyTarget();
  TunableListT tunables = _main->accuracyVariables(_randSize);
  //reset tunables to min+1
  tunables.resetMinAll(1);
  int triesLeft = 0;

  for(int i=1; true;++i){
    reallocate();
    _main->randomizeOutputs();
    _isRunning = true;
    ti.restartTimeout();
    _main->compute();
    ti.disableTimeout();
    _isRunning = false;
    ElementT cur = _main->accuracy();
    if(cur >= target){
      JTRACE("training goal reached")(i)(cur)(target);
      return i;
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
  return 0;
  */
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

double petabricks::PetabricksRuntime::updateRaceTimeout(TestResult& result, int /*winnerid*/) {
  if(result.time < jalib::maxval<double>() && result.time >= 0){
    if(result.accuracy >= RACE_ACCURACY_TARGET)
      return std::min(GRAPH_MAX_SEC, result.time*RACE_MULTIPLIER);
    else
      return std::min(GRAPH_MAX_SEC, result.time*RACE_MULTIPLIER_LOWACC);
  }else{
    return GRAPH_MAX_SEC;
  }
}

bool petabricks::PetabricksRuntime::isTrainingRun(){
  _needTraingingRun = true;
  return _isTrainingRun;
}

void petabricks::PetabricksRuntime::abort(){
  TestIsolation* master = SubprocessTestIsolation::masterProcess();
  if(master!=NULL){
    JASSERT(false).Text("abort");
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


void petabricks::PetabricksRuntime::reexecTestIsolation(int fd) {
  if(TI_REEXEC) {
    saveConfig();
    std::string sfd = jalib::XToString(fd);
    const char** argv = new const char*[theArgc + 3];
    for(int i=0; i<theArgc; ++i)
      argv[i] = theArgv[i];
    argv[theArgc+0] = "--reexecchild";
    argv[theArgc+1] = sfd.c_str();
    argv[theArgc+2] = NULL;
    //JTRACE("reexec");
    execv(argv[0], (char**)argv);
    JASSERT(false).Text("execv failed");
  }
}

