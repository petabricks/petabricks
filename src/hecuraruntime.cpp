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
#include "hecuraruntime.h"
#include "jtunable.h"
#include "jfilesystem.h"
#include "jtimer.h"
#include "dynamictask.h"

#include <algorithm>


static int TRAIN_MIN=16;
static int TRAIN_MAX=4096;
static int TRAIN_LEVEL_THRESH=1.15;

static int GRAPH_MIN=8;
static int GRAPH_MAX=1024;
static int GRAPH_MAX_SEC=10;
static int GRAPH_STEP=8;
static int GRAPH_TRIALS=2;
static int GRAPH_SMOOTHING=1;
static int SEARCH_BRANCH_FACTOR=32;
static bool GRAPH_MULTIGRID=false;

namespace{//file local

typedef jalib::JTunableManager TunableManager;

class ConfigTesterGlue : public jalib::JConfigurationTester {
  typedef hecura::HecuraRuntime::Main Main;
  typedef jalib::JTime JTime;
  typedef jalib::JTunableConfiguration JTunableConfiguration;
public:
  ConfigTesterGlue(Main& m) : _main(m) {}

  double test(const JTunableConfiguration& cfg){
    cfg.makeActive();
    JTime start=JTime::Now();
    _main.compute();
    return JTime::Now()-start;
  }
private:
  Main& _main;
};

}

hecura::HecuraRuntime::HecuraRuntime(Main& m) : main(m)
{
  randSize = 4096;
  //load config from disk
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0){
    std::string filename = jalib::Filesystem::GetProgramPath() + ".cfg";
    if(jalib::Filesystem::FileExists(filename))
      tm.load(filename);
  }
}

hecura::HecuraRuntime::~HecuraRuntime()
{
  //save config to disk
  TunableManager& tm = TunableManager::instance();
  if(tm.size()>0){
    std::string filename = jalib::Filesystem::GetProgramPath() + ".cfg";
    tm.save(filename);
  }
}

#define shift argc--,argv++;

int hecura::HecuraRuntime::runMain(int argc, const char** argv){
  bool isSiman = false;
  bool isAutotuneMode = false;
  bool isGraphMode = false;
  bool isOptimizeMode = false;
  bool doIO = true;
  std::string graphParam;

  //parse args
  shift;
  while(argc>0){
    if(strcmp(argv[0],"--siman")==0){
      isSiman = true;
      shift;
    }else if(strcmp(argv[0],"--autotune")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      isAutotuneMode = true;
      graphParam = argv[1];
      shift;
      shift;
    }else if(strcmp(argv[0],"-g")==0 || strcmp(argv[0],"--graph")==0){
      isGraphMode = true;
      shift;
    }else if(strcmp(argv[0],"-n")==0 || strcmp(argv[0],"--random")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("--random expects an argument");
      doIO = false;
      main.randomInputs(randSize=jalib::StringToInt(argv[1]));
      shift;
      shift;
    }else if(strcmp(argv[0],"--max")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      GRAPH_MAX = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--min")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      GRAPH_MIN = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--step")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      GRAPH_STEP = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--trials")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      GRAPH_TRIALS = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--smoothing")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      GRAPH_SMOOTHING = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--max-sec")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      GRAPH_MAX_SEC = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else if(strcmp(argv[0],"--graph-param")==0 || strcmp(argv[0],"--graph-tune")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      graphParam = argv[1];
      shift;
      shift;
    }else if(strcmp(argv[0],"--optimize")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      graphParam = argv[1];
      isOptimizeMode=true;
      shift;
      shift;
    }else if(strcmp(argv[0],"--multigrid")==0){
      GRAPH_MULTIGRID = true;
      shift;
    }else{
      break;
    }
  }

  if(isGraphMode){
    runGraphMode();
    return 0;
  }

  if(isAutotuneMode){
    runAutotuneMode(graphParam);
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

  if(!graphParam.empty()){
    runGraphParamMode(graphParam);
    return 0;
  }

  if(isSiman){
    JTIMER_SCOPE(autotune);
    ConfigTesterGlue cfgtester(main);
    jalib::JTunableManager::instance().autotune(&cfgtester);
  }else{
    JTIMER_SCOPE(compute);
    main.compute();
  }

  if(doIO){ //write outputs
    JTIMER_SCOPE(write);
    main.write(argc,argv); 
  }

  return 0;
}


void hecura::HecuraRuntime::runGraphMode(){
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n+=GRAPH_STEP){
    randSize = (GRAPH_MULTIGRID ? (1 << n)+1: n);
    double avg = runTrial();
    printf("%d %.6f\n", randSize, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

void hecura::HecuraRuntime::runGraphParamMode(const std::string& param){
  jalib::JTunable* tunable = jalib::JTunableManager::instance().getReverseMap()[param];
  JASSERT(tunable!=0)(param).Text("parameter not found");
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n+=GRAPH_STEP){
    tunable->setValue(n);
    double avg = runTrial();
    printf("%d %.6lf\n", n, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

double hecura::HecuraRuntime::runTrial(){
  std::vector<double> rslts;
  rslts.reserve(2*GRAPH_SMOOTHING+1);
  for( int n =  randSize-GRAPH_SMOOTHING
     ;     n <= randSize+GRAPH_SMOOTHING
     ; ++n)
  {
    double t=0;
    for(int z=0;z<GRAPH_TRIALS; ++z){
      main.randomInputs(n);
      jalib::JTime begin=jalib::JTime::Now();
      main.compute();
      jalib::JTime end=jalib::JTime::Now();
      t+=end-begin;
    }
    double avg = t/GRAPH_TRIALS;
    rslts.push_back(avg);
  }
  std::sort(rslts.begin(), rslts.end());
  return rslts[GRAPH_SMOOTHING];
}


double hecura::HecuraRuntime::optimizeParameter(const std::string& param){
  jalib::JTunable* tunable = jalib::JTunableManager::instance().getReverseMap()[param];
  JASSERT(tunable!=0)(param).Text("parameter not found");
  return optimizeParameter(*tunable, GRAPH_MIN, GRAPH_MAX, (GRAPH_MAX-GRAPH_MIN)/SEARCH_BRANCH_FACTOR);
}

double hecura::HecuraRuntime::optimizeParameter(jalib::JTunable& tunable, int min, int max, int step){
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

namespace{ //file local 
  std::string _mktname(int lvl, const std::string& prefix, const std::string& type){
    return prefix + "_lvl" + jalib::XToString(lvl) + "_" + type;
  }
}

void hecura::HecuraRuntime::runAutotuneMode(const std::string& prefix){
  typedef jalib::JTunable JTunable;
  jalib::JTunableReverseMap m = jalib::JTunableManager::instance().getReverseMap();
  
  int numLevels = 1;

  //find numlevels
  for(int lvl=2; true; ++lvl){
    JTunable* rule   = m[_mktname(lvl, prefix, "rule")];
    JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
    if(rule==0 && cutoff==0){
      numLevels = lvl-1;
      break;
    }
  }

  //initialize
  for(int lvl=1; lvl<=numLevels; ++lvl){
    resetLevel(lvl, prefix, m);
  }

  int curLevel = 1;
  for(randSize=TRAIN_MIN; randSize<TRAIN_MAX; randSize*=2){
    double cur = autotuneLevel(curLevel, prefix, m);
    if(curLevel < numLevels){
      double next = autotuneLevel(curLevel+1, prefix, m);
      if(cur > next*TRAIN_LEVEL_THRESH){
        curLevel++;
        JTRACE("promoting to next level")(curLevel)(randSize);
      }else{
        resetLevel(curLevel+1, prefix, m);
      }
    }
  }
}

double hecura::HecuraRuntime::autotuneLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m){
  typedef jalib::JTunable JTunable;
  JTunable* rule = m[_mktname(lvl, prefix, "rule")];
  JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
  if(rule!=0){
    int bestRule=0;
    double best = std::numeric_limits<double>::max();
    for(int r=rule->min(); r<=rule->max(); ++r){
      rule->setValue(r);
      double d;
      if(cutoff!=0)
        d = optimizeParameter(*cutoff, cutoff->min(), randSize+1, (randSize+1-cutoff->min())/SEARCH_BRANCH_FACTOR);
      else
        d = runTrial();
      if(d<best){
        best=d;
        bestRule=r;
      }
      JTRACE("autotune level")(lvl)(best)(bestRule)(cutoff!=0?cutoff->value():-1);
    }
    rule->setValue(bestRule);
    return best;
  }else{
    if(cutoff!=0)
      return optimizeParameter(*cutoff, cutoff->min(), randSize/2-1);
    else
      return -1;
  }
}

void hecura::HecuraRuntime::resetLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m){
  jalib::JTunable* rule   = m[_mktname(lvl, prefix, "rule")];
  jalib::JTunable* cutoff = m[_mktname(lvl, prefix, "cutoff")];
  if(cutoff!=0) cutoff->setValue(cutoff->max());
  if(rule!=0)   rule->setValue(rule->min());
}