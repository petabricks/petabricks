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
#ifndef HECURAHECURARUNTIME_H
#define HECURAHECURARUNTIME_H

#include <string>

#include "jtunable.h"

namespace hecura {

class HecuraRuntime{
public:
  /**
   * Interface used to construct a main routine in generated code
   */
  class Main {
  public:
    ///
    /// destructor
    virtual ~Main(){}

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
  };

  ///
  /// Construct the runtime, parse any runtime-specific args
  HecuraRuntime(Main&);

  ///
  /// Destruct the runtime, saving config to disk
  ~HecuraRuntime();

  ///
  /// Run the given main routine
  int runMain(int argc, const char** argv);

  void runGraphMode();

  void runGraphParamMode(const std::string& param);

  void runGraphParallelMode();

  double optimizeParameter(const std::string& param);
  double optimizeParameter(jalib::JTunable& param, int min, int max, int step=-1);

  double runTrial();

//   void runAutotuneMode(const std::string& prefix);
// 
//   double autotuneOneLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m);
//   double autotuneTwoLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m);
//   void resetLevel(int lvl, const std::string& prefix, jalib::JTunableReverseMap& m);

  static bool isTrainingRun();
  static void setIsTrainingRun(bool b);

  void setSize(int n){randSize=n;};
  int curSize() const { return randSize;};

  static void abort();
  static void saveConfig();

private:
  Main& main;
  int randSize;
};

}

#endif
