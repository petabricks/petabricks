#ifndef NWKDE_H
#define NWKDE_H

//#define DEBUG

// workaround for generator having specific sizes
#define WORKAROUND

//#define TESTDIR "/afs/csail.mit.edu/u/c/cychan/projects/petabricks/examples/kernel/test/"
#define TESTDIR "/home/cychan/projects/petabricks/examples/kernel/test/"

// Problem dimensions
#ifdef WORKAROUND

// make sure generator and metric transform headers are compatible
#define M 4
#define N 8750
#define L l
#define P 8
#define Q q
#define M2 4
#define N2 8750

#else

// PPBUG: uncommenting this section causes M to be replaced incorrectly
//   in nwkdeVA.pbcc rule body

//#define M m
//#define N n
//#define L l
//#define P p
//#define Q q
//#define M2 m2
//#define N2 n2

#endif

#endif // NWKDE_H
