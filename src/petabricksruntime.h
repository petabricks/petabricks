/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#ifndef PETABRICKSPETABRICKSRUNTIME_H
#define PETABRICKSPETABRICKSRUNTIME_H

#include <string>
#include <float.h>
#include <signal.h>

#include "jtunable.h"

namespace petabricks {

class DynamicScheduler;

class PetabricksRuntime{
public:
  /**
   * Interface used to construct a main routine in generated code
   */

  ///
  /// Scheduler for scheduling the dynamic tasks
  static DynamicScheduler *scheduler;


  typedef class __usr_main_interface{
  public:

    ///
    /// destructor
    virtual ~__usr_main_interface(){}

    ///
    /// initialize with random inputs
    virtual bool verifyArgs(int argc, const char** argv) = 0;

    ///
    /// Read inputs from disk
    virtual void read(int argc, const char** argv) = 0;

    ///
    /// Perform the computation
    virtual void compute() = 0;

    ///
    /// Write inputs to disk
    virtual void write(int argc, const char** argv) = 0;

    ///
    /// initialize with random inputs
    virtual void randomInputs(int size) = 0;

    virtual const char* name() = 0;
  private:
  } Main;

  ///
  /// Construct the runtime, parse any runtime-specific args
  PetabricksRuntime(int argc, const char** argv, Main*);

  ///
  /// Destruct the runtime, saving config to disk
  ~PetabricksRuntime();

  ///
  /// Run the given main routine
  int runMain(int argc, const char** argv);


  void runGraphMode();

  void runGraphParamMode(const std::string& param);

  void runGraphParallelMode();

  double optimizeParameter(const std::string& param);
  double optimizeParameter(jalib::JTunable& param, int min, int max, int step=-1);

  double runTrial(double thresh = DBL_MAX);

//   void runAutotuneMode(const std::string& prefix);
//
//   double autotuneOneLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m);
//   double autotuneTwoLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m);
//   void resetLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m);

  static bool isTrainingRun();
  static void setIsTrainingRun(bool b);

  void setSize(int n){_randSize=n;};
  int curSize() const { return _randSize;};

  static void abort();

  static void saveConfig();

  bool alternateTransforms() const { return _main->name() != _mainName; }
  void addTransform(Main* tx){ JASSERT(tx!=NULL); if(tx->name() == _mainName) _main=tx; }

private:
  Main* _main;
  std::string _mainName;
  int _randSize;
};

}

#endif
