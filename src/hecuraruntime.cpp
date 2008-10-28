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

static int GRAPH_MIN=8;
static int GRAPH_MAX=1024;
static int GRAPH_MAX_SEC=10;
static int GRAPH_STEP=8;
static int GRAPH_TRIALS=5;

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

hecura::HecuraRuntime::HecuraRuntime()
{
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

int hecura::HecuraRuntime::runMain(Main& main, int argc, const char** argv){
  bool isAutotuneMode = false;
  bool isGraphMode = false;
  bool doIO = true;

  //parse args
  shift;
  while(argc>0){
    if(strcmp(argv[0],"--autotune")==0){
      isAutotuneMode = true;
      shift;
    }else if(strcmp(argv[0],"-g")==0 || strcmp(argv[0],"--graph")==0){
      isGraphMode = true;
      shift;
    }else if(strcmp(argv[0],"-n")==0 || strcmp(argv[0],"--random")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("--random expects an argument");
      doIO = false;
      main.randomInputs(jalib::StringToInt(argv[1]));
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
    }else if(strcmp(argv[0],"--max-sec")==0){
      JASSERT(argc>1)(argv[0])(argc).Text("arguement expected");
      GRAPH_MAX_SEC = jalib::StringToInt(argv[1]);
      shift;
      shift;
    }else{
      break;
    }
  }

  if(isGraphMode){
    runGraphMode(main);
    return 0;
  }
  
  argc++, argv--;

  if(doIO && !main.verifyArgs(argc, argv))
    return 1;

  if(doIO){ //read inputs
    JTIMER_SCOPE(read);
    main.read(argc, argv);
  }

  if(isAutotuneMode){
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

void hecura::HecuraRuntime::runGraphMode(Main& main){
  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n+=GRAPH_STEP){
    float avg = runTrial(main, n);
    printf("%d %.6f\n", n, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }
}

double hecura::HecuraRuntime::runTrial(Main& main, int n){
  double t=0;
  for(int z=0;z<GRAPH_TRIALS; ++z){
    main.randomInputs(n);
    jalib::JTime begin=jalib::JTime::Now();
    main.compute();
    jalib::JTime end=jalib::JTime::Now();
    t+=end-begin;
  }
  double avg = t/GRAPH_TRIALS;
  return avg;
}