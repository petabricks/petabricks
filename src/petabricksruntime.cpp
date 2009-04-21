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
#include "jtunable.h"
#include "jfilesystem.h"
#include "jtimer.h"
#include "dynamictask.h"
#include "autotuner.h"
#include "dynamicscheduler.h"

#include <algorithm>
#include <math.h>
#include <limits>


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
"  --siman              : autotune using simulated annealing (not working)"        "\n"
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

static bool _isTrainingRun = false;
static bool _needTraingingRun = false;

static int TRAIN_MIN=64;
static int TRAIN_MAX=4096;
//static int TRAIN_LEVEL_THRESH=1.2;

static int GRAPH_MIN=8;
static int GRAPH_MAX=2048;
static int GRAPH_MAX_SEC=std::numeric_limits<int>::max();
static int GRAPH_STEP=8;
static int GRAPH_TRIALS=3;
static int GRAPH_SMOOTHING=0;
static int SEARCH_BRANCH_FACTOR=8;
static bool MULTIGRID_FLAG=false;
static bool FULL_MULTIGRID_FLAG=false;

JTUNABLE(worker_threads,   8, MIN_NUM_WORKERS, MAX_NUM_WORKERS);

petabricks::DynamicScheduler* petabricks::PetabricksRuntime::scheduler = NULL;

namespace{//file local

typedef jalib::JTunableManager TunableManager;

class ConfigTesterGlue : public jalib::JConfigurationTester {
  typedef petabricks::PetabricksRuntime::Main Main;
  typedef jalib::JTime JTime;
  typedef jalib::JTunableConfiguration JTunableConfiguration;
public:
  ConfigTesterGlue(Main& m) : _main(m) {}

  double test(const JTunableConfiguration& cfg){
    cfg.makeActive();
    JTime start=JTime::Now();
#ifndef GRACEFUL_ABORT
    _main.compute();
#else
    try{
      _main.compute();
    }catch(petabricks::DynamicScheduler::AbortException e){
      petabricks::PetabricksRuntime::scheduler->abortEnd();
      return std::numeric_limits<double>::max();
    }
#endif
    return JTime::Now()-start;
  }
private:
  Main& _main;
};

}

#define shift argc--,argv++;

petabricks::PetabricksRuntime::PetabricksRuntime(int argc, const char** argv, Main* m)
  : _main(m)
  , _randSize(4096)
{
  JASSERT(scheduler==NULL);
  scheduler = new DynamicScheduler();
  JASSERT(m!=NULL);
  _mainName = m->name();
  //load config from disk
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0){
    std::string filename = jalib::Filesystem::GetProgramPath() + ".cfg";
    if(jalib::Filesystem::FileExists(filename))
      tm.load(filename);
  }

  while(argc>0){
    if(strcmp(argv[0],"--transform")==0 || strcmp(argv[0],"--tx")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      _mainName = argv[1];
      shift;
      shift;
    }else shift;
  }
}

petabricks::PetabricksRuntime::~PetabricksRuntime()
{
  saveConfig();
}

void petabricks::PetabricksRuntime::saveConfig()
{
  //save config to disk
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0){
    std::string filename = jalib::Filesystem::GetProgramPath() + ".cfg";
    tm.save(filename);
  }
}


int petabricks::PetabricksRuntime::runMain(int argc, const char** argv){
  Main& main = *_main;

  JASSERT(_mainName == main.name())(_mainName).Text("Unknown transform");

  bool isSiman = false;
  bool isAutotuneMode = false;
  bool isGraphMode = false;
  bool isGraphParallelMode = false;
  bool isOptimizeMode = false;
  bool doIO = true;
  std::string graphParam;
  std::vector<std::string> autotuneParams;

  //parse args
  shift;
  while(argc>0){
    if(strcmp(argv[0],"-h")==0 || strcmp(argv[0],"--help")==0){
      main.verifyArgs(-1, NULL);
      std::cerr << theHelp << std::endl;
      shift;
      return 1;
    }else if(strcmp(argv[0],"--siman")==0){
      isSiman = true;
      shift;
    }else if(strcmp(argv[0],"--multigrid")==0){
      MULTIGRID_FLAG = true;
      GRAPH_MIN = 1;
      GRAPH_MAX = 9;
      GRAPH_STEP = 1;
      TRAIN_MIN = 2;
      TRAIN_MAX = 64;
      shift;
    }else if(strcmp(argv[0],"--fullmg")==0){
      MULTIGRID_FLAG = true;
      FULL_MULTIGRID_FLAG = true;
      GRAPH_MIN = 1;
      GRAPH_MAX = 9;
      GRAPH_STEP = 1;
      TRAIN_MIN = 2;
      TRAIN_MAX = 64;
      shift;
    }else if(strcmp(argv[0],"--autotune")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      isAutotuneMode = true;
      autotuneParams.push_back(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"-g")==0 || strcmp(argv[0],"--graph")==0){
      isGraphMode = true;
      shift;
    }else if(strcmp(argv[0],"-n")==0 || strcmp(argv[0],"--random")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("--random expects an argument");
      doIO = false;
      main.randomInputs(_randSize=jalib::StringToInt(argv[1]));
      shift;
      shift;
    }else if(strcmp(argv[0],"--max")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      TRAIN_MAX = GRAPH_MAX = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--min")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      TRAIN_MIN = GRAPH_MIN = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--step")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      GRAPH_STEP = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--trials")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      GRAPH_TRIALS = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--smoothing")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      GRAPH_SMOOTHING = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--max-sec")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      GRAPH_MAX_SEC = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--graph-param")==0 || strcmp(argv[0],"--graph-tune")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      graphParam = argv[1];
      if(strcmp(argv[1], "worker_threads") == 0)
      isGraphParallelMode = true;
      shift;
      shift;
    }else if(strcmp(argv[0],"--graph-parallel")==0){
      worker_threads.setValue(1);
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      isGraphParallelMode = true;
      shift;
    }else if(strcmp(argv[0],"--optimize")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("argument expected");
      graphParam = argv[1];
      isOptimizeMode=true;
      shift;
      shift;
    }else if(strcmp(argv[0],"--reset")==0){
      jalib::JTunableManager::instance().reset();
      shift;
      return 0;
    }else if(strcmp(argv[0],"--name")==0){
      std::cout << main.name() << std::endl;
      shift;
      return 0;
    }else if(strcmp(argv[0],"--transform")==0 || strcmp(argv[0],"--tx")==0){
      //parsed above
      shift;
      shift;
    }else{
      break;
    }
  }

  JASSERT(worker_threads>=1)(worker_threads);
  scheduler->startWorkerThreads(worker_threads);

  if(isGraphMode){
    runGraphMode();
    return 0;
  }

  if(isAutotuneMode){
    if (!MULTIGRID_FLAG) {
      runAutotuneMode(autotuneParams);
    } else {
      runMultigridAutotuneMode();
    }
    return 0;
  }


  argc++, argv--;

  if(doIO && !main.verifyArgs(argc, argv))
    return 1;

  if(doIO){ //read inputs
    JTIMER_SCOPE(read);
    main.read(argc, argv);
  }

  if(isOptimizeMode){
    optimizeParameter(graphParam);
    return 0;
  }

  if(isGraphParallelMode) {
    runGraphParallelMode();
    return 0;
  }

  if(!graphParam.empty()){
    runGraphParamMode(graphParam);
    return 0;
  }


  if(isSiman){
    JTIMER_SCOPE(autotune);
    ConfigTesterGlue cfgtester(main);
    jalib::JTunableManager::instance().autotune(&cfgtester);
  }else{
#ifndef GRACEFUL_ABORT
    JTIMER_SCOPE(compute);
    main.compute();
#else
    try{
      JTIMER_SCOPE(compute);
      main.compute();
    }catch(petabricks::DynamicScheduler::AbortException e){
      scheduler->abortEnd();
      JWARNING(false).Text("PetabricksRuntime::abort() called");
      return 5;
    }
#endif
  }

  if(doIO){ //write outputs
    JTIMER_SCOPE(write);
    main.write(argc,argv);
  }

  return 0;
}
  
void petabricks::PetabricksRuntime::runAutotuneMode(const std::vector<std::string>& params){
  std::vector<AutotunerPtr> tuners;
  tuners.reserve(params.size());
  for(size_t i=0; i<params.size(); ++i)
    tuners.push_back(new Autotuner(*this, params[i]));

  for(int n=TRAIN_MIN; n<=TRAIN_MAX; n*=2){
    setSize(n + 1);
    for(size_t i=0; i<tuners.size(); ++i)
      tuners[i]->trainOnce();
    saveConfig();
  }
}

void petabricks::PetabricksRuntime::runMultigridAutotuneMode(){
  std::string s1 = "Poisson2D_Inner_Prec1_1";
  std::string s2 = "Poisson2D_Inner_Prec2_1";
  std::string s3 = "Poisson2D_Inner_Prec3_1";
  std::string s4 = "Poisson2D_Inner_Prec4_1";
  std::string s5 = "Poisson2D_Inner_Prec5_1";
  Autotuner at1(*this, s1);
  Autotuner at2(*this, s2);
  Autotuner at3(*this, s3);
  Autotuner at4(*this, s4);
  Autotuner at5(*this, s5);

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
    at6 = new Autotuner(*this, s6);
    at7 = new Autotuner(*this, s7);
    at8 = new Autotuner(*this, s8);
    at9 = new Autotuner(*this, s9);
    at10 = new Autotuner(*this, s10);

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
    scheduler->startWorkerThreads(worker_threads);

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
    _isTrainingRun = true;
    _needTraingingRun = false;
    for( int n =  _randSize-GRAPH_SMOOTHING
      ;     n <= _randSize+GRAPH_SMOOTHING
      ; ++n)
    {
      double t=0;
      for(int z=0;z<GRAPH_TRIALS; ++z){
        _main->randomInputs(n);

#ifdef GRACEFUL_ABORT
        // Set up a time out so we don't waste time running things that are
        // slower than what we have seen already.
        scheduler->resetAbortFlag();
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
          --z; //redo this iteration
        }else{
          t+=end-begin;
        }
      }
      double avg = t/GRAPH_TRIALS;
      rslts.push_back(avg);
    }
    std::sort(rslts.begin(), rslts.end());
    return rslts[GRAPH_SMOOTHING];
#ifdef GRACEFUL_ABORT
  }catch(petabricks::DynamicScheduler::AbortException e){
    scheduler->abortEnd();
    return std::numeric_limits<double>::max();
  }
#endif
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

// namespace{ //file local
//   std::string _mktname(int lvl, const std::string& prefix, const std::string& type){
//     return prefix + "_lvl" + jalib::XToString(lvl) + "_" + type;
//   }
// }
//
// void petabricks::PetabricksRuntime::runAutotuneMode(const std::string& prefix){
//   typedef jalib::JTunable JTunable;
//   jalib::JTunableReverseMap m = jalib::JTunableManager::instance().getReverseMap();
//
//   int numLevels = 1;
//
//   //find numlevels
//   for(int lvl=2; true; ++lvl){
//     JTunable* rule   = m[_mktname(lvl, prefix, "rule")];
//     JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
//     if(rule==0 && cutoff==0){
//       numLevels = lvl-1;
//       break;
//     }
//   }
//
//   JASSERT(numLevels>1)(prefix).Text("invalid prefix to autotune");
//
//   //initialize
//   for(int lvl=1; lvl<=numLevels; ++lvl){
//     resetLevel(lvl, prefix, m);
//   }
//
//   int curLevel = 1;
//   for(randSize=TRAIN_MIN; randSize<TRAIN_MAX; randSize*=2){
//     if (MULTIGRID_FLAG) {
//       randSize = (1 << (int) (floor(log2(randSize)))) + 1;
//     }
//     printf("autotune n=%d result... \n", randSize);
//     fflush(stdout);
//     double cur;
// //     if(curLevel>1)
// //       cur=autotuneTwoLevel(curLevel, prefix, m);
// //     else
//       cur=autotuneOneLevel(curLevel, prefix, m);
//     if(curLevel < numLevels){
//       double next = autotuneTwoLevel(curLevel+1, prefix, m);
//       if(cur > next*TRAIN_LEVEL_THRESH){
//         curLevel++;
//         JTRACE("promoting to next level")(curLevel)(randSize)(cur)(next);
//       }else{
//         resetLevel(curLevel+1, prefix, m);
//       }
//     }
//
//     for(int lvl=1; lvl<=curLevel; ++lvl){
//       JTunable* rule   = m[_mktname(lvl, prefix, "rule")];
//       JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
//       if(cutoff!=0) printf(" |%d| ", cutoff->value());
//       if(rule!=0) printf("ALG%d", rule->value());
//       else  printf("N/A");
//     }
//     printf("\n");
//   }
// }
//
// double petabricks::PetabricksRuntime::autotuneOneLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m){
//   typedef jalib::JTunable JTunable;
//   JTunable* rule = m[_mktname(lvl, prefix, "rule")];
//   JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
//   if(rule!=0){
//     int bestRule=0;
//     double best = std::numeric_limits<double>::max();
//     for(int r=rule->min(); r<=rule->max(); ++r){
//       rule->setValue(r);
//       double d = runTrial();
//       if(d<best){
//         best=d;
//         bestRule=r;
//       }
//       JTRACE("autotune level")(lvl)(best)(bestRule)(cutoff!=0?cutoff->value():-1);
//     }
//     rule->setValue(bestRule);
//     return best;
//   }else{
//     return runTrial();
//   }
// }
//
// double petabricks::PetabricksRuntime::autotuneTwoLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m){
//   typedef jalib::JTunable JTunable;
//   double best = std::numeric_limits<double>::max();
//   JTunable* rule1 =  m[_mktname(lvl-1,   prefix, "rule")];
//   JTunable* rule2 =  m[_mktname(lvl, prefix, "rule")];
//   JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
//   int bestRule1=0;
//   int bestRule2=0;
//   int bestCuttoff=0;
//   int r1min=0, r1max=0, r2min=0, r2max=0;
//   if(rule1!=0){
//     r1min=rule1->min();
//     r1max=rule1->max();
//   }
//   if(rule2!=0){
//     r2min=rule2->min();
//     r2max=rule2->max();
//   }
//   for(int r1=r1min; r1<=r1max; ++r1){
//     for(int r2=r2min; r2<=r2max; ++r2){
//       if(rule1!=0) rule1->setValue(r1);
//       if(rule2!=0) rule2->setValue(r2);
//       if(r1max==r2max && r1==r2) continue; //skip same rule
//       double d;
//       if(cutoff!=0)
//         d = optimizeParameter(*cutoff, cutoff->min(), randSize+1, (randSize+1-cutoff->min())/SEARCH_BRANCH_FACTOR);
//       else
//         d = runTrial();
//       if(d<best){
//         best=d;
//         bestRule1=r1;
//         bestRule2=r2;
//         if(cutoff!=0) bestCuttoff = *cutoff;
//       }
//     }
//   }
//   if(rule1!=0)  rule1->setValue(bestRule1);
//   if(rule2!=0)  rule2->setValue(bestRule2);
//   if(cutoff!=0) cutoff->setValue(bestCuttoff);
//   return best;
// }
//
// void petabricks::PetabricksRuntime::resetLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m){
//   jalib::JTunable* rule   = m[_mktname(lvl, prefix, "rule")];
//   jalib::JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
//   if(cutoff!=0) cutoff->setValue(cutoff->max());
//   if(rule!=0)   rule->setValue(rule->min());
// }

bool petabricks::PetabricksRuntime::isTrainingRun(){
  _needTraingingRun = true;
  return _isTrainingRun;
}

void petabricks::PetabricksRuntime::setIsTrainingRun(bool b){
  _isTrainingRun=b;
}

void petabricks::PetabricksRuntime::abort(){
#ifdef GRACEFUL_ABORT
  scheduler->abortBegin();
#else
  JASSERT(false).Text("PetabricksRuntime::abort() called");
#endif
}


