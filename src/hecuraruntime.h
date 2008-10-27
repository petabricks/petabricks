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
    /// Read inputs from disk
    virtual void read(int argc, const char** argv) = 0;

    ///
    /// Perform the computation
    virtual void compute() = 0;

    ///
    /// Write inputs to disk
    virtual void write(int argc, const char** argv) = 0;
  };

  ///
  /// Construct the runtime, parse any runtime-specific args
  HecuraRuntime(int& argc, const char**& argv);

  ///
  /// Destruct the runtime, saving config to disk
  ~HecuraRuntime();

  ///
  /// Run the given main routine
  void runMain(Main& main, int argc, const char** argv);

private:
  bool _isAutotuneMode;
};

}

#endif
