#ifndef NWKDE_H
#define NWKDE_H

//#define DEBUG

// relative directory for test data used by generator
#define TESTDIR "examples/kernel/test/"

// workaround for generator having specific sizes
#define WORKAROUND

// Problem dimensions
#ifdef WORKAROUND

// make sure generator headers are compatible
#define _M_ 4
#define _N_ 8750
#define _L_ l
#define _P_ 8
#define _Q_ q
#define _M2_ 4
#define _N2_ 8750

#else

#define _M_ m
#define _N_ n
#define _L_ l
#define _P_ p
#define _Q_ q
#define _M2_ m2
#define _N2_ n2

#endif

#endif // NWKDE_H
