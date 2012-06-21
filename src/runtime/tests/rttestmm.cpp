/*****************************************************************************
 *  Copyright (C) 2008-2011 Massachusetts Institute of Technology            *
 *                                                                           *
 *  Permission is hereby granted, free of charge, to any person obtaining    *
 *  a copy of this software and associated documentation files (the          *
 *  "Software"), to deal in the Software without restriction, including      *
 *  without limitation the rights to use, copy, modify, merge, publish,      *
 *  distribute, sublicense, and/or sell copies of the Software, and to       *
 *  permit persons to whom the Software is furnished to do so, subject       *
 *  to the following conditions:                                             *
 *                                                                           *
 *  The above copyright notice and this permission notice shall be included  *
 *  in all copies or substantial portions of the Software.                   *
 *                                                                           *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY                *
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE               *
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      *
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   *
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   *
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    *
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE           *
 *                                                                           *
 *  This source code is part of the PetaBricks project:                      *
 *    http://projects.csail.mit.edu/petabricks/                              *
 *                                                                           *
 *****************************************************************************/
#include "petabricks.h"
#include "common/jtimer.h"

#if defined(HAVE_EMMINTRIN_H) && defined(HAVE_PMMINTRIN_H)


#include <emmintrin.h>
#include <pmmintrin.h>

using namespace petabricks;

int n = 512;

void niave(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  for(int i=0; i<n; ++i) {
    for(int j=0; j<n; ++j) {
      ElementT sum = 0.0;
      for(int k=0; k<n; ++k) {
        sum += a.cell(i,k) * b.cell(k,j);
      }
      c.cell(i,j) = sum;
    }
  }
}


void transposed(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  for(int i=0; i<n; ++i) {
    for(int j=0; j<n; ++j) {
      ElementT sum = 0.0;
      for(int k=0; k<n; ++k) {
        sum += a.cell(i,k) * b.cell(k,j);
      }
      c.cell(i,j) = sum;
    }
  }
}

template<typename T>
bool is_aligned(const T* t) {
  return (reinterpret_cast<intptr_t>(t) % 16) == 0;
}


void sse_v0(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  for(int i=0; i<n; ++i) {
    for(int j=0; j<n; ++j) {
      __m128d sum = _mm_setzero_pd();
      int k = 0;
      for(; k<n; k+=2) {
        __m128d av = _mm_setr_pd(a.cell(i,k), a.cell(i,k+1));
        __m128d bv = _mm_setr_pd(b.cell(k,j), b.cell(k+1,j));
        sum = _mm_add_pd(sum,  _mm_mul_pd(av, bv));
      }
      //sum of odd elements are in sum[0]
      //sum of even elements are in sum[1]
      //hadd combines these
      sum = _mm_hadd_pd(sum, sum);
      _mm_store_sd(&c.cell(i,j), sum);
    }
  }
}


void sse_v1(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  for(int i=0; i<n; ++i) {
    for(int j=0; j<n; ++j) {
      __m128d sum = _mm_setzero_pd();
      int k = 0;
      for(; k<n; k+=2) {
        __m128d av = _mm_loadu_pd(&a.cell(i,k));
        __m128d bv = _mm_loadu_pd(&b.cell(k,j));
        sum = _mm_add_pd(sum,  _mm_mul_pd(av, bv));
      }
      //sum of odd elements are in sum[0]
      //sum of even elements are in sum[1]
      //hadd combines these
      sum = _mm_hadd_pd(sum, sum);
      _mm_store_sd(&c.cell(i,j), sum);
    }
  }
}


void sse_v2(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  for(int i=0; i<n; ++i) {
    for(int j=0; j<n; ++j) {
      __m128d sum = _mm_setzero_pd();
      int k = 0;
      for(; k<n && !is_aligned(&a.cell(i,k)); k+=1) {
        __m128d av = _mm_load_sd(&a.cell(i,k));
        __m128d bv = _mm_load_sd(&b.cell(k,j));
        sum = _mm_add_pd(sum,  _mm_mul_pd(av, bv));
      }
      for(; k<n; k+=2) {
        __m128d av = _mm_load_pd(&a.cell(i,k));
        __m128d bv = _mm_load_pd(&b.cell(k,j));
        sum = _mm_add_pd(sum,  _mm_mul_pd(av, bv));
      }
      for(; k<n; k+=1) {
        __m128d av = _mm_load_sd(&a.cell(i,k));
        __m128d bv = _mm_load_sd(&b.cell(k,j));
        sum = _mm_add_pd(sum, _mm_mul_pd(av, bv));
      }
      //sum of odd elements are in sum[0]
      //sum of even elements are in sum[1]
      //hadd combines these
      sum = _mm_hadd_pd(sum, sum);
      _mm_store_sd(&c.cell(i,j), sum);
    }
  }
}


void sse_v3(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  for(int i=0; i<n; i+=2) {
    for(int j=0; j<n; j+=2) {
      __m128d sum00 = _mm_setzero_pd();
      __m128d sum01 = _mm_setzero_pd();
      __m128d sum10 = _mm_setzero_pd();
      __m128d sum11 = _mm_setzero_pd();
      int k = 0;
      for(; k<n && !is_aligned(&a.cell(i,k)); k+=1) {
        __m128d av0x = _mm_load_sd(&a.cell(i,k));
        __m128d av1x = _mm_load_sd(&a.cell(i+1,k));
        __m128d bvx0 = _mm_load_sd(&b.cell(k,j));
        __m128d bvx1 = _mm_load_sd(&b.cell(k,j+1));
        sum00 = _mm_add_pd(sum00,  _mm_mul_pd(av0x, bvx0));
        sum01 = _mm_add_pd(sum01,  _mm_mul_pd(av0x, bvx1));
        sum10 = _mm_add_pd(sum10,  _mm_mul_pd(av1x, bvx0));
        sum11 = _mm_add_pd(sum11,  _mm_mul_pd(av1x, bvx1));
      }
      for(; k<n; k+=2) {
        __m128d av0x = _mm_load_pd(&a.cell(i,k));
        __m128d av1x = _mm_load_pd(&a.cell(i+1,k));
        __m128d bvx0 = _mm_load_pd(&b.cell(k,j));
        __m128d bvx1 = _mm_load_pd(&b.cell(k,j+1));
        sum00 = _mm_add_pd(sum00,  _mm_mul_pd(av0x, bvx0));
        sum01 = _mm_add_pd(sum01,  _mm_mul_pd(av0x, bvx1));
        sum10 = _mm_add_pd(sum10,  _mm_mul_pd(av1x, bvx0));
        sum11 = _mm_add_pd(sum11,  _mm_mul_pd(av1x, bvx1));
      }
      for(; k<n; k+=1) {
        __m128d av0x = _mm_load_sd(&a.cell(i,k));
        __m128d av1x = _mm_load_sd(&a.cell(i+1,k));
        __m128d bvx0 = _mm_load_sd(&b.cell(k,j));
        __m128d bvx1 = _mm_load_sd(&b.cell(k,j+1));
        sum00 = _mm_add_pd(sum00,  _mm_mul_pd(av0x, bvx0));
        sum01 = _mm_add_pd(sum01,  _mm_mul_pd(av0x, bvx1));
        sum10 = _mm_add_pd(sum10,  _mm_mul_pd(av1x, bvx0));
        sum11 = _mm_add_pd(sum11,  _mm_mul_pd(av1x, bvx1));
      }
      //sum of odd elements are in sum[0]
      //sum of even elements are in sum[1]
      //hadd combines these sum = {sum[0]+sum[1], sum[0]+sum[1]}
      sum00 = _mm_hadd_pd(sum00, sum00);
      sum01 = _mm_hadd_pd(sum01, sum01);
      sum10 = _mm_hadd_pd(sum10, sum10);
      sum11 = _mm_hadd_pd(sum11, sum11);
      _mm_store_sd(&c.cell(i+0,j+0), sum00);
      _mm_store_sd(&c.cell(i+0,j+1), sum01);
      _mm_store_sd(&c.cell(i+1,j+0), sum10);
      _mm_store_sd(&c.cell(i+1,j+1), sum11);
    }
  }
}


void sse_v4(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  for(int i=0; i<n; i+=2) {
    for(int j=0; j<n; j+=2) {
      __m128d sum00 = _mm_setzero_pd();
      __m128d sum01 = _mm_setzero_pd();
      __m128d sum10 = _mm_setzero_pd();
      __m128d sum11 = _mm_setzero_pd();
      int k = 0;
      for(; k<n; k+=2) {
        __m128d av0x = _mm_setr_pd(a.cell(i,k)  , a.cell(i,k+1));
        __m128d av1x = _mm_setr_pd(a.cell(i+1,k), a.cell(i+1,k+1));
        __m128d bvx0 = _mm_setr_pd(b.cell(k,j)  , b.cell(k+1,j));
        __m128d bvx1 = _mm_setr_pd(b.cell(k,j+1), b.cell(k+1,j+1));
        sum00 = _mm_add_pd(sum00,  _mm_mul_pd(av0x, bvx0));
        sum01 = _mm_add_pd(sum01,  _mm_mul_pd(av0x, bvx1));
        sum10 = _mm_add_pd(sum10,  _mm_mul_pd(av1x, bvx0));
        sum11 = _mm_add_pd(sum11,  _mm_mul_pd(av1x, bvx1));
      }
      //sum of odd elements are in sum[0]
      //sum of even elements are in sum[1]
      //hadd combines these
      sum00 = _mm_hadd_pd(sum00, sum00);
      sum01 = _mm_hadd_pd(sum01, sum01);
      sum10 = _mm_hadd_pd(sum10, sum10);
      sum11 = _mm_hadd_pd(sum11, sum11);
      _mm_store_sd(&c.cell(i+0,j+0), sum00);
      _mm_store_sd(&c.cell(i+0,j+1), sum01);
      _mm_store_sd(&c.cell(i+1,j+0), sum10);
      _mm_store_sd(&c.cell(i+1,j+1), sum11);
    }
  }
}

void sse_v5(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  for(int i=0; i<n; i+=4) {
    for(int j=0; j<n; j+=4) {
      __m128d sum00 = _mm_setzero_pd();
      __m128d sum01 = _mm_setzero_pd();
      __m128d sum02 = _mm_setzero_pd();
      __m128d sum03 = _mm_setzero_pd();
      __m128d sum10 = _mm_setzero_pd();
      __m128d sum11 = _mm_setzero_pd();
      __m128d sum12 = _mm_setzero_pd();
      __m128d sum13 = _mm_setzero_pd();
      __m128d sum20 = _mm_setzero_pd();
      __m128d sum21 = _mm_setzero_pd();
      __m128d sum22 = _mm_setzero_pd();
      __m128d sum23 = _mm_setzero_pd();
      __m128d sum30 = _mm_setzero_pd();
      __m128d sum31 = _mm_setzero_pd();
      __m128d sum32 = _mm_setzero_pd();
      __m128d sum33 = _mm_setzero_pd();
      int k = 0;
      for(; k<n; k+=2) {
        __m128d av0x = _mm_setr_pd(a.cell(i+0,k), a.cell(i+0,k+1));
        __m128d av1x = _mm_setr_pd(a.cell(i+1,k), a.cell(i+1,k+1));
        __m128d av2x = _mm_setr_pd(a.cell(i+2,k), a.cell(i+2,k+1));
        __m128d av3x = _mm_setr_pd(a.cell(i+3,k), a.cell(i+3,k+1));
        __m128d bvx0 = _mm_setr_pd(b.cell(k,j+0), b.cell(k+1,j+0));
        __m128d bvx1 = _mm_setr_pd(b.cell(k,j+1), b.cell(k+1,j+1));
        __m128d bvx2 = _mm_setr_pd(b.cell(k,j+2), b.cell(k+1,j+2));
        __m128d bvx3 = _mm_setr_pd(b.cell(k,j+3), b.cell(k+1,j+3));
        sum00 = _mm_add_pd(sum00,  _mm_mul_pd(av0x, bvx0));
        sum01 = _mm_add_pd(sum01,  _mm_mul_pd(av0x, bvx1));
        sum02 = _mm_add_pd(sum02,  _mm_mul_pd(av0x, bvx2));
        sum03 = _mm_add_pd(sum03,  _mm_mul_pd(av0x, bvx3));
        sum10 = _mm_add_pd(sum10,  _mm_mul_pd(av1x, bvx0));
        sum11 = _mm_add_pd(sum11,  _mm_mul_pd(av1x, bvx1));
        sum12 = _mm_add_pd(sum12,  _mm_mul_pd(av1x, bvx2));
        sum13 = _mm_add_pd(sum13,  _mm_mul_pd(av1x, bvx3));
        sum20 = _mm_add_pd(sum20,  _mm_mul_pd(av2x, bvx0));
        sum21 = _mm_add_pd(sum21,  _mm_mul_pd(av2x, bvx1));
        sum22 = _mm_add_pd(sum22,  _mm_mul_pd(av2x, bvx2));
        sum23 = _mm_add_pd(sum23,  _mm_mul_pd(av2x, bvx3));
        sum30 = _mm_add_pd(sum30,  _mm_mul_pd(av3x, bvx0));
        sum31 = _mm_add_pd(sum31,  _mm_mul_pd(av3x, bvx1));
        sum32 = _mm_add_pd(sum32,  _mm_mul_pd(av3x, bvx2));
        sum33 = _mm_add_pd(sum33,  _mm_mul_pd(av3x, bvx3));
      }
      //sum of odd elements are in sum[0]
      //sum of even elements are in sum[1]
      //hadd combines these
      sum00 = _mm_hadd_pd(sum00, sum00);
      sum01 = _mm_hadd_pd(sum01, sum01);
      sum10 = _mm_hadd_pd(sum10, sum10);
      sum11 = _mm_hadd_pd(sum11, sum11);
      _mm_store_sd(&c.cell(i+0,j+0), _mm_hadd_pd(sum00, sum00));
      _mm_store_sd(&c.cell(i+0,j+1), _mm_hadd_pd(sum01, sum01));
      _mm_store_sd(&c.cell(i+0,j+2), _mm_hadd_pd(sum02, sum02));
      _mm_store_sd(&c.cell(i+0,j+3), _mm_hadd_pd(sum03, sum03));
      _mm_store_sd(&c.cell(i+1,j+0), _mm_hadd_pd(sum10, sum10));
      _mm_store_sd(&c.cell(i+1,j+1), _mm_hadd_pd(sum11, sum11));
      _mm_store_sd(&c.cell(i+1,j+2), _mm_hadd_pd(sum12, sum12));
      _mm_store_sd(&c.cell(i+1,j+3), _mm_hadd_pd(sum13, sum13));
      _mm_store_sd(&c.cell(i+2,j+0), _mm_hadd_pd(sum20, sum20));
      _mm_store_sd(&c.cell(i+2,j+1), _mm_hadd_pd(sum21, sum21));
      _mm_store_sd(&c.cell(i+2,j+2), _mm_hadd_pd(sum22, sum22));
      _mm_store_sd(&c.cell(i+2,j+3), _mm_hadd_pd(sum23, sum23));
      _mm_store_sd(&c.cell(i+3,j+0), _mm_hadd_pd(sum30, sum30));
      _mm_store_sd(&c.cell(i+3,j+1), _mm_hadd_pd(sum31, sum31));
      _mm_store_sd(&c.cell(i+3,j+2), _mm_hadd_pd(sum32, sum32));
      _mm_store_sd(&c.cell(i+3,j+3), _mm_hadd_pd(sum33, sum33));
    }
  }
}


void x(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c) {
  a = a.transposed();
  int i=0;
  int j=0;

  for(; i<n; i+=2) {
    for(j=0; j<n; j+=2) {
      __m128d sumx0 = _mm_setzero_pd();
      __m128d sumx1 = _mm_setzero_pd();
      int k00=0;
      int k01=0;
      int k10=0;
      int k11=0;
      for(; k00<n && k01<n && k10<n && k11<n; ++k00,++k01,++k10,++k11) {
        sumx0 = _mm_add_pd(sumx0,
                  _mm_mul_pd(_mm_setr_pd(a.cell(i+0,k00), a.cell(i+1,k10)),
                             _mm_setr_pd(b.cell(k00,j+0), b.cell(k10,j+0 ))));
        sumx1 = _mm_add_pd(sumx1,
                  _mm_mul_pd(_mm_setr_pd(a.cell(i+0,k01), a.cell(i+1,k11)),
                             _mm_setr_pd(b.cell(k01,j+1), b.cell(k11,j+1 ))));
      }
      _mm_storeu_pd(&c.cell(i+0,j+0), sumx0);
      _mm_storeu_pd(&c.cell(i+0,j+1), sumx1);
    }
  }

  for(; i<n; ++i) {
    for(j=0; j<n; ++j) {
      ElementT sum = 0.0;
      for(int k=0; k<n; ++k) {
        sum += a.cell(i,k) * b.cell(k,j);
      }
      c.cell(i,j) = sum;
    }
  }
}




typedef void (*testfn)(sequential::MatrixRegion2D a, sequential::MatrixRegion2D b, sequential::MatrixRegion2D c);
void test(const char* name, testfn f) {
  sequential::MatrixRegion2D a = sequential::MatrixRegion2D::allocate(n,n);
  sequential::MatrixRegion2D b = sequential::MatrixRegion2D::allocate(n,n);
  sequential::MatrixRegion2D c = sequential::MatrixRegion2D::allocate(n,n);
  a.randomize();
  b.randomize();

  (*f)(a,b,c);

  {
  jalib::JTime t1 = jalib::JTime::now();
  (*f)(a,b,c);
  jalib::JTime t2 = jalib::JTime::now();
  printf("%20s %.4f\n", name, t2-t1);
  }
}

int main(int /*argc*/, const char** /*argv*/){
  test("niave", &niave);
  test("transposed", &transposed);
  test("sse_setr", &sse_v0);
  test("sse_loadu", &sse_v1);
  test("sse_load", &sse_v2);
  test("sse_load_2blocked", &sse_v3);
  test("sse_setr_2blocked", &sse_v4);
  test("sse_setr_4blocked", &sse_v5);
  test("x", &x);
  return 0;
}

#else
int main(int, const char**) { return 0; }
#endif

petabricks::PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
petabricks::PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}
void _petabricksInit() {}
void _petabricksCleanup() {}

