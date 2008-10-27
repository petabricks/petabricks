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

hecura::HecuraRuntime::HecuraRuntime(int& argc, const char**& argv)
  : _isAutotuneMode(false)
{
  //parse args
  while(argc>1){
    if(strcmp(argv[1],"--autotune")==0){
      _isAutotuneMode = true;
      argc--,argv++;
    }else{
      break;
    }
  }

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

void hecura::HecuraRuntime::runMain(Main& main, int argc, const char** argv){
  { //read inputs
    JTIMER_SCOPE(read);
    main.read(argc, argv);
  }

  if(_isAutotuneMode){
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
}

