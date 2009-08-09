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
#include "performancetester.h"

#include "jtimer.h"
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
  JTime start = JTime::Now();
  int rv = system(ss.str().c_str());
  JASSERT(rv==0)(rv)(ss.str()).Text("Failed to run TestCase");
  return JTime::Now()-start;;
}
