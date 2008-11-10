
#include "../src/jconvert.h"
#include "../src/jassert.h"
#include "../src/jtimer.h"

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
  JASSERT(argc==2)(argc);
  int n = jalib::StringToInt(argv[1]);
  for(size_t i=8; i<=n; i+=8){
    printf("%d %.8lf\n", i, test(i));
  }
}

