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

JTIMER(read);
JTIMER(compute);
JTIMER(write);

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

int hecura::HecuraRuntime::runMain(Main& main, int argc, const char** argv){
  bool isAutotuneMode = false;
  bool isGraphMode = false;

  //parse args
  while(argc>1){
    if(strcmp(argv[1],"--autotune")==0){
      isAutotuneMode = true;
      argc--,argv++;
    }else if(strcmp(argv[1],"--graph")==0){
      isGraphMode = true;
      argc--,argv++;
    }else{
      break;
    }
  }


  if(isGraphMode){
    runGraphMode(main);
    return 0;
  }

  if(main.verifyArgs(argc, argv)){
    { //read inputs
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
  
    { //write outputs
      JTIMER_SCOPE(write);
      main.write(argc,argv); 
    }
    return 0;
  }else{
    return 1;
  }
}

void hecura::HecuraRuntime::runGraphMode(Main& main){
  int GRAPH_MIN=8;
  int GRAPH_MAX=4096;
  int GRAPH_MAX_SEC=10;
  int GRAPH_STEP=8;
  int GRAPH_TRIALS=1;

  for(int n=GRAPH_MIN; n<=GRAPH_MAX; n+=GRAPH_STEP){
    double t=0;
    for(int z=0;z<GRAPH_TRIALS; ++z){
      main.randomInputs(n);
      jalib::JTime begin=jalib::JTime::Now();
      main.compute();
      jalib::JTime end=jalib::JTime::Now();
      t+=end-begin;
    }
    float avg = t/GRAPH_TRIALS;
    printf("%d    %.6f\n", n, avg);
    if(avg > GRAPH_MAX_SEC) break;
  }

}
