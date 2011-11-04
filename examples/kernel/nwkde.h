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
#define __M__ 4
#define __N__ 8750
#define __L__ l
#define __P__ 8
#define __Q__ q
#define __M2__ 4
#define __N2__ 8750

#else

#define __M__ m
#define __N__ n
#define __L__ l
#define __P__ p
#define __Q__ q
#define __M2__ m2
#define __N2__ n2

#endif

#endif // NWKDE_H
