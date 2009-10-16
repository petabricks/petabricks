//compile with:
// g++ -o stlsorttest stlsorttest.cpp ../src/libpbcommon.a

#include "../src/common/jconvert.h"
#include "../src/common/jassert.h"
#include "../src/common/jtimer.h"

#include <vector>
#include <algorithm>
#include <stdlib.h>


double test(size_t n){
  using jalib::JTime;

  std::vector<double> data(n);
  for(size_t i=0; i<n; ++i)
    data[i]=drand48()*1000000.0;

  JTime begin = JTime::Now();
  std::sort(data.begin(), data.end());
  JASSERT(data.front()!=17.1 && data.back()!=17.2);
  return JTime::Now()-begin;
}

int main(int argc, const char** argv){
  JASSERT(argc==4)(argc).Text("USAGE: stlsorttest MIN MAX STEP");
  int min  = jalib::StringToInt(argv[1]);
  int max  = jalib::StringToInt(argv[2]);
  int step = jalib::StringToInt(argv[3]);
  for(size_t i=min; i<=max; i+=step){
    printf("%d %.8lf\n", i, test(i));
  }
}

