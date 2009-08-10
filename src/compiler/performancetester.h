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
#ifndef PETABRICKSPERFORMANCETESTER_H
#define PETABRICKSPERFORMANCETESTER_H

#include "common/jprintable.h"
#include "common/jrefcounted.h"

#include <string>
#include <vector>

namespace petabricks {

  class TestCase;
  typedef jalib::JRef<TestCase> TestCasePtr;
  typedef std::vector<TestCasePtr> TestCaseList;

  /**
   * Stores a set of valid args to output progrm
   */
  class TestCase : public jalib::JRefCounted, public jalib::JPrintable {
  public:
    void addMatrix(const std::string& m){_inputs.push_back(m);}
    void print(std::ostream& o) const;
  private:
    std::vector<std::string> _inputs;
  };

  /**
   * Manages test cases and runs peformance tests
   */
  class PerformanceTester{
  public:
    void setBinary(const std::string& path){_binary=path;}
    void addTestCase(const TestCasePtr& tc){_testCases.push_back(tc);}
    const TestCaseList& testCases() const {return _testCases;}

    void setIOSizes(int i, int o){_numInputs=i; _numOutputs=o;}

    ///
    /// Run a given test case, return how many seconds it took
    double runTest(const TestCase& tc);

    ///
    /// Run all tests, return total time
    double runAllTests(){
      double d=0;
      for(TestCaseList::const_iterator i=_testCases.begin(); i!=_testCases.end(); ++i)
        d+=runTest(*i);
      return d;
    }
  private:
    std::string _binary;
    TestCaseList _testCases;
    int _numInputs;
    int _numOutputs;
  };

}

#endif
