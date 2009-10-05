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
#include "performancetester.h"

#include "common/jtimer.h"

using jalib::JTime;

void petabricks::TestCase::print(std::ostream& o) const{
  printStlList(o, _inputs.begin(), _inputs.end(), " ");
}

double petabricks::PerformanceTester::runTest(const TestCase& tc){
  //TODO check correctness of output
  //TODO exclude time to load/write matrix
  std::ostringstream ss;
  ss << _binary << ' ' << tc;
  for(int i=0; i<_numOutputs; ++i)
    ss << " /dev/null";
  JTRACE("Running TestCase")(tc)(ss.str());
  JTime start = JTime::now();
  int rv = system(ss.str().c_str());
  JASSERT(rv==0)(rv)(ss.str()).Text("Failed to run TestCase");
  return JTime::now()-start;;
}
