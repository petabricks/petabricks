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

const char theHelp[] =
"\nALTERNATE MODES:" "\n"
"  --autotune PREFIX    : autotune a given alg choice site using a genetic tuner" "\n"
"  --optimize PARAM     : optimize a given configuration parameter using binary search" "\n"
"  --graph              : graph performance from --min to --max" "\n"
"  --graph-parallel     : graph parallel scalability with number of threads" "\n"
"  --graph-param PARAM  : graph the effect of changing a given parameter" "\n"
"  --fullmg             : specialized tuning for multigrid" "\n"
"  --multigrid          : specialized tuning for multigrid" "\n"
"  --help               : print this message and exit"         "\n"
"  --name               : print the name of the main transform and exit" "\n"
"  --reset              : reset all configuration parameters to their default and exit" "\n"
//"  --siman              : autotune using simulated annealing (not working)"        "\n"
"\nOPTIONS:" "\n"
"  --random N           : populate inputs with randomly generated data of a given size" "\n"
"  --transform NAME     : use a given transform instead of the default main transform" "\n"
"  --max N              : end point for graphs and tuning" "\n"
"  --max-sec N          : end point (in seconds) for graphs and tuning" "\n"
"  --min N              : start point for graphs and tuning" "\n"
"  --smoothing N        : apply smoothing to tests by averaging with similar sized inputs" "\n"
"  --step N             : step size for graphs and tuning" "\n"
"  --trials             : number of trails for graphs and tuning" "\n"
;

static std::string CONFIG_FILENAME;

static bool _isTrainingRun = false;
static bool _needTraingingRun = false;

static int TRAIN_MIN=64;
static int TRAIN_MAX=4096;
//static int TRAIN_LEVEL_THRESH=1.2;

static int GRAPH_MIN=8;
static int GRAPH_MAX=2048;
static int GRAPH_MAX_SEC=std::numeric_limits<int>::max();
static int GRAPH_STEP=8;
static int GRAPH_TRIALS=1;
static int GRAPH_SMOOTHING=0;
static int SEARCH_BRANCH_FACTOR=8;
static bool MULTIGRID_FLAG=false;
static bool FULL_MULTIGRID_FLAG=false;
static bool DUMPTIMING=false;
static bool ACCURACY=false;

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

// class ConfigTesterGlue : public jalib::JConfigurationTester {
//   typedef petabricks::PetabricksRuntime::Main Main;
//   typedef jalib::JTime JTime;
//   typedef jalib::JTunableConfiguration JTunableConfiguration;
// public:
//   ConfigTesterGlue(Main& m) : _main(m) {}
//
//   double test(const JTunableConfiguration& cfg){
//     cfg.makeActive();
//     JTime start=JTime::Now();
//#ifndef GRACEFUL_ABORT
//     _main.compute();
//#else
//     try{
//       _main.compute();
//     }catch(petabricks::DynamicScheduler::AbortException e){
//       petabricks::PetabricksRuntime::scheduler->abortEnd();
//       return std::numeric_limits<double>::max();
//     }
//#endif
//     return JTime::Now()-start;
//   }
// private:
//   Main& _main;
// };

}

petabricks::PetabricksRuntime::PetabricksRuntime(int argc, const char** argv, Main* m)
  : _main(m)
  , _randSize(4096)
{
  jalib::JArgs args(argc, argv);
  JASSERT(m!=NULL);
  _mainName = m->name();
  CONFIG_FILENAME = jalib::Filesystem::GetProgramPath() + ".cfg";
  
  args.param("cfg",    CONFIG_FILENAME);
  args.param("config", CONFIG_FILENAME).help("filename of the program configuration");
  args.param("tx",        _mainName);
  args.param("transform", _mainName).help("name of the transform to run or tune");

  //load config from disk
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0 && jalib::Filesystem::FileExists(CONFIG_FILENAME))
    tm.load(CONFIG_FILENAME);
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
  jalib::JArgs args(argc, argv);
  Main& main = *_main;
  JASSERT(_mainName == main.name())(_mainName).Text("Unknown transform");

  if(! args.param("fixedrandom")){
    struct timeval currentTimeVal;
    gettimeofday(&currentTimeVal, NULL);
    srand48(currentTimeVal.tv_usec);
  }

//  bool isSiman = false;
  bool isAutotuneMode = false;
  bool isGraphParallelMode = false;
  bool isOptimizeMode = false;
  bool doIO = true;
  std::string graphParam;
  std::vector<std::string> autotuneParams;
  std::string autotune2Param;

  if(args.param("multigrid").help("autotune multigrid")){
    doIO=false;
    MULTIGRID_FLAG = true;
    GRAPH_MIN = 1;
    GRAPH_MAX = 9;
    GRAPH_STEP = 1;
    TRAIN_MIN = 2;
    TRAIN_MAX = 64;
  }
  if(args.param("fullmg").help("autotune full multigrid")){
    doIO=false;
    MULTIGRID_FLAG = true;
    FULL_MULTIGRID_FLAG = true;
    GRAPH_MIN = 1;
    GRAPH_MAX = 9;
    GRAPH_STEP = 1;
    TRAIN_MIN = 2;
    TRAIN_MAX = 64;
  }
  if(args.param("autotune", autotuneParams)){
    isAutotuneMode = true;
    doIO=false;
  }
  if(args.param("autotune2", autotune2Param)){
    isAutotuneMode = true;
    doIO=false;
  }
  if(args.param("n", _randSize) || args.param("random", _randSize)){
    doIO = false;
  }
  args.param("max", TRAIN_MAX);
  args.param("max", GRAPH_MAX);
  args.param("min", TRAIN_MIN);
  args.param("min", GRAPH_MIN);
  args.param("step", GRAPH_STEP);
  args.param("trials", GRAPH_TRIALS);
  args.param("smoothing", GRAPH_SMOOTHING);
  args.param("max-sec", GRAPH_MAX_SEC);
  args.param("max-sec", GRAPH_MAX_SEC);
  args.param("accuracy", ACCURACY).help("print out accuracy of answer");
    

  if(args.param("graph-param", graphParam) || args.param("graph-tune", graphParam)){
    if(graphParam == "worker_threads")
      isGraphParallelMode = true;
  }
  if(args.param("graph-parallel")){
    worker_threads.setValue(1);
    isGraphParallelMode = true;
  }
  if(args.param("optimize", graphParam)){
    isOptimizeMode=true;
  }
  args.param("time", DUMPTIMING);
  if(args.param("reset")){
    jalib::JTunableManager::instance().reset();
    return 0;
  }
  if(args.param("name")){
    std::cout << main.name() << std::endl;
    return 0;
  }
  if(args.needHelp()){
    return 1;
  }

  JASSERT(worker_threads>=1)(worker_threads);
  DynamicScheduler::instance().startWorkerThreads(worker_threads);
  
  std::vector<std::string> txArgs;
  args.param("args", txArgs);

  if(doIO && main.numArgs() != (int)txArgs.size()){
    JTRACE("wrong arg count")(txArgs.size())(main.numInputs())(main.numOutputs())(main.numArgs());
    fprintf(stderr, "USAGE: %s %s\n", main.name(), main.helpString().c_str());
    return 1;
  }

  if(doIO){ //read inputs
    JTIMER_SCOPE(read);
    main.read(txArgs);
  }

  if(args.param("graph")){
    runGraphMode();
  }else if(isAutotuneMode){
    if (!MULTIGRID_FLAG) {
      if(autotune2Param.empty())
        runAutotuneMode(autotuneParams);
      else
        runAutotune2Mode(autotune2Param);
    } else {
      runMultigridAutotuneMode();
    }
  }else if(isOptimizeMode){
    optimizeParameter(graphParam);
  }else if(isGraphParallelMode) {
    runGraphParallelMode();
  }else if(!graphParam.empty()){
    runGraphParamMode(graphParam);
  }else if(!doIO) {
    JTIMER_SCOPE(runtrial);
    runTrial();
  }else{
    try{

      if(args.param("accuracytrain"))
        trainAndComputeWrapper();
      else
        computeWrapper();

#ifdef GRACEFUL_ABORT
    }catch(petabricks::DynamicScheduler::AbortException e){
      DynamicScheduler::instance().abortEnd();
      JWARNING(false).Text("PetabricksRuntime::abort() called");
      return 5;
#endif
    }catch(ComputeRetryException e){
      UNIMPLEMENTED();
      return 6;
    }

    { //write outputs
      JTIMER_SCOPE(write);
      main.write(txArgs);
    }
  }

  if(args.param("force-output")){
    JTIMER_SCOPE(forceoutput);
    std::vector<std::string> tmp;
    for(int i=main.numInputs(); i-->0;)
      tmp.push_back("/dev/null");
    for(int i=main.numOutputs(); i-->0;)
      tmp.push_back("-");
    main.write(tmp);
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

  return 0;
}
  
void petabricks::PetabricksRuntime::runAutotuneMode(const std::vector<std::string>& params){
  AutotunerList tuners;
  for(size_t i=0; i<params.size(); ++i)
    tuners.push_back(new Autotuner(*this, _main, params[i]));
  runAutotuneLoop(tuners);
}
  
  
void petabricks::PetabricksRuntime::runAutotune2Mode(const std::string& param){
  AutotunerList tuners;
  for(Main* m = _main; m!=NULL; m=m->nextTemplateMain())
    tuners.push_back(new Autotuner(*this, m, std::string(m->name())+"_"+param));
  runAutotuneLoop(tuners);
}

void petabricks::PetabricksRuntime::runAutotuneLoop(const AutotunerList& tuners){
  Main* old = _main;
  for(int n=TRAIN_MIN; n<=TRAIN_MAX; n*=2){
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
  // Set up a time out so we don't waste time running things that are
  // slower than what we have seen already.
  DynamicScheduler::instance().resetAbortFlag();
  if (thresh < std::numeric_limits<unsigned int>::max() - 1) {
    alarm((unsigned int) thresh + 1);
  }
#endif

  jalib::JTime begin=jalib::JTime::Now();
  _main->compute();
  jalib::JTime end=jalib::JTime::Now();

#ifdef GRACEFUL_ABORT
  // Disable previous alarm
  if (thresh < std::numeric_limits<unsigned int>::max() - 1) {
    alarm(0);
  }
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
  Autotuner at1(*this, _main, s1);
  Autotuner at2(*this, _main, s2);
  Autotuner at3(*this, _main, s3);
  Autotuner at4(*this, _main, s4);
  Autotuner at5(*this, _main, s5);

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
    at6 = new Autotuner(*this, _main, s6);
    at7 = new Autotuner(*this, _main, s7);
    at8 = new Autotuner(*this, _main, s8);
    at9 = new Autotuner(*this, _main, s9);
    at10 = new Autotuner(*this, _main, s10);

    run_fullmg_flag = m["run_fullmg_flag"];
    JASSERT(run_fullmg_flag != 0);
  }

  for(int n=TRAIN_MIN; n<=TRAIN_MAX; n*=2){
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



