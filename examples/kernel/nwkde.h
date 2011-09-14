#ifndef NWKDE_H
#define NWKDE_H

//#define DEBUG

//#define INCLUDE_METHOD1
//#define INCLUDE_METHOD2

// algorithm makes use of NaN values to signal "no data"
// since NaNs not yet supported by runtime IO, use sentinel value instead
#define MYNAN -667
#define ISNAN(x) ((x) == MYNAN)

// workaround for generator having specific sizes
#define WORKAROUND

// relative directory for test data used by generator
#define TESTDIR "examples/kernel/test/"

// Problem dimensions
#ifdef WORKAROUND

// make sure generator headers are compatible
#define M 4
#define N 8750
#define L l
#define P 8
#define Q q
#define M2 4
#define N2 8750

#else

#define M m
#define N n
#define L l
#define P p
#define Q q
#define M2 m2
#define N2 n2

#endif

#endif // NWKDE_H
