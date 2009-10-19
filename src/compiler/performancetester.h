/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
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
