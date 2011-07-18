/* src/config.h.  Generated from config.h.in by configure.  */
/* src/config.h.in.  Generated from configure.ac by autoheader.  */

/* var name in output code for target accuracy */
#define ACCTARGET_STR "accuracyTarget"

/* macro for gcc function __attribute__s */
#define ATTRIBUTE(x) __attribute__((x))

/* generated file name */
#define BIN_TMPFILE "bin"

/* larges cache line size in system */
#define CACHE_LINE_SIZE 64

/* var name in output code for begin coord */
#define COORD_BEGIN_STR "_iter_begin"

/* var name in output code for end coord */
#define COORD_END_STR "_iter_end"

/* C++ compiler used by pbc */
#define CXX "g++"

/* C++ preprocessor used by pbc */
#define CXXCPP "g++ -E -x c++"

/* C++ compiler defines used by pbc */
#define CXXDEFS " -DHAVE_CONFIG_H"

/* C++ compiler flags used by pbc */
#define CXXFLAGS " -O0 -g -Wall -Wextra -fdiagnostics-show-option -march=native -fno-rtti  -I/afs/csail.mit.edu/u/m/mangpo/NVIDIA_GPU_Computing_SDK/OpenCL/common/inc -I/afs/csail.mit.edu/u/m/mangpo/NVIDIA_GPU_Computing_SDK/shared/inc"

/* C++ compiler link flags used by pbc */
#define CXXLDFLAGS " -rdynamic"

/* C++ compiler libraries used by pbc */
#define CXXLIBS "-loclUtil_x86_64 -lshrutil_x86_64 -lOpenCL -lrt -lpthread -lm -llapack -lfftw3 -lblas -lcrypto  -L/afs/csail.mit.edu/u/m/mangpo/NVIDIA_GPU_Computing_SDK/OpenCL/common/lib/ -L/afs/csail.mit.edu/u/m/mangpo/NVIDIA_GPU_Computing_SDK/shared/lib -L/afs/csail.mit.edu/u/m/mangpo/NVIDIA_GPU_Computing_SDK/shared/lib/linux"

/* verbose debug printouts and log files in /tmp */
#define DEBUG 

/* accuracy stand-in for fixed-accuracy rules */
#define DEFAULT_ACCURACY 1

/* null sync device path */
#define DEVNULL "/dev/null"

/* dont generate continuation code after SPAWN/enqueue */
/* #undef DISABLE_CONTINUATIONS */

/* the default maximum number of iterations for for_enough{} */
#define FORENOUGH_MAX_ITERS 1000

/* the default minimum number of iterations for for_enough{} */
#define FORENOUGH_MIN_ITERS 1

/* generated file name */
#define GENHEADER "_pbhdr.h"

/* generated file name */
#define GENMISC "_pbmisc"

/* set to 1 to use SHA1 instead of MD5 */
#define HASH_USE_SHA1 0

/* Define to 1 if you have the `backtrace_symbols' function. */
#define HAVE_BACKTRACE_SYMBOLS 1

/* Define to 1 if you have the <boost/random.hpp> header file. */
#define HAVE_BOOST_RANDOM_HPP 1

/* Define to 1 if you have the <cxxabi.h> header file. */
#define HAVE_CXXABI_H 1

/* Define to 1 if you have the <execinfo.h> header file. */
#define HAVE_EXECINFO_H 1

/* Define to 1 if you have the <fftw3.h> header file. */
#define HAVE_FFTW3_H 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `blas' library (-lblas). */
#define HAVE_LIBBLAS 1

/* Define to 1 if you have the `fftw3' library (-lfftw3). */
#define HAVE_LIBFFTW3 1

/* Define to 1 if you have the `lapack' library (-llapack). */
#define HAVE_LIBLAPACK 1

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the `oclUtil_x86_64' library (-loclUtil_x86_64). */
#define HAVE_LIBOCLUTIL_X86_64 1

/* Define to 1 if you have the `OpenCL' library (-lOpenCL). */
#define HAVE_LIBOPENCL 1

/* Define to 1 if you have the `pthread' library (-lpthread). */
#define HAVE_LIBPTHREAD 1

/* Define to 1 if you have the `rt' library (-lrt). */
#define HAVE_LIBRT 1

/* Define to 1 if you have the `shrutil_x86_64' library (-lshrutil_x86_64). */
#define HAVE_LIBSHRUTIL_X86_64 1

/* Define to 1 if you have the <math.h> header file. */
#define HAVE_MATH_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have a working `mmap' system call. */
#define HAVE_MMAP 1

/* Enable OpenCL code generation */
#define HAVE_OPENCL 1

/* Define to 1 if you have the <openssl/md5.h> header file. */
#define HAVE_OPENSSL_MD5_H 1

/* Define to 1 if you have the <openssl/sha.h> header file. */
#define HAVE_OPENSSL_SHA_H 1

/* Define to 1 if you have the <poll.h> header file. */
#define HAVE_POLL_H 1

/* Define to 1 if you have the <signal.h> header file. */
#define HAVE_SIGNAL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/mman.h> header file. */
#define HAVE_SYS_MMAN_H 1

/* Define to 1 if you have the <sys/prctl.h> header file. */
#define HAVE_SYS_PRCTL_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/wait.h> header file. */
#define HAVE_SYS_WAIT_H 1

/* define if __thread works with this compiler */
#define HAVE_THREADLOCAL 

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* keyword to force inlining */
#define INLINE inline __attribute__((flatten))

/* run null tasks immediately when they are enqueued */
#define INLINE_NULL_TASKS 

/* define to add a system var to transform scope */
/* #undef INPUT_PERIMETER_STR */

/* define to add a system var to transform scope */
/* #undef INPUT_SIZE_STR */

/* Use a custom allocator from JALIB */
#define JALIB_ALLOCATOR 1

/* Cause the custom allocator to call malloc instead of mmap */
/* #undef JALIB_USE_MALLOC */

/* skip some copies in debug printing, conflicts with JASSERT_LOG */
#define JASSERT_FAST 

/* log traces to /tmp/jassertlog.pid */
/* #undef JASSERT_LOG */

/* include source line numbers in error messages */
#define JASSERT_USE_SRCPOS 

/* first port to try to use to listen */
#define LISTEN_PORT_FIRST 22550

/* type for elements in the matrix */
#define MATRIX_ELEMENT_T float

/* type for indices into the matrix */
#define MATRIX_INDEX_T int

/* cache results from maxima */
/* #undef MAXIMA_CACHING */

/* define in order to generate maxima.log, (a log of maxima commands) */
/* #undef MAXIMA_LOG */

/* path to maxima program */
#define MAXIMA_PATH "/usr/bin/maxima"

/* the maximum number of dimensions supported */
#define MAX_DIMENSIONS 64

/* the maximum number of dimensions supported */
#define MAX_INPUT_BITS 32

/* max number of workers supported */
#define MAX_NUM_WORKERS 512

/* number of levels for recursive choices */
#define MAX_REC_LEVELS 12

/* min number of workers supported */
#define MIN_NUM_WORKERS 1

/* binutils tool used by pbc */
#define NM "/usr/bin/nm"

/* enable low level optimization such as specializations and prefetching */
#define OPT_LOWLEVEL 1

/* define to add a system var to transform scope */
/* #undef OUTPUT_SIZE_STR */

/* Name of package */
#define PACKAGE "petabricks"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "jansel@csail.mit.edu"

/* Define to the full name of this package. */
#define PACKAGE_NAME "petabricks"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "petabricks 3.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "petabricks"

/* Define to the version of this package. */
#define PACKAGE_VERSION "3.0"

/* define to run tasks inline instead of enqueuing them */
/* #undef PBCC_SEQUENTIAL */

/* path to python interpreter */
#define PYTHON "/usr/bin/python2.5"

/* keyword to indicate non-aliased pointers/refs */
#define RESTRICT __restrict__

/* var name in output code for rule output variable */
#define RETURN_VAL_STR "_pb_rv"

/* git revision for HEAD at the time ./configure was run */
#define REVISION_LONG "db0b1260fdb5fc9a733eed0163f810de8c23116e"

/* git revision for HEAD at the time ./configure was run */
#define REVISION_SHORT "db0b126"

/* define to use a global sequential cutoff instead of a per-transform one */
#define SINGLE_SEQ_CUTOFF 1

/* var name in output code */
#define SPLIT_CHUNK_SIZE "TRANSFORM_LOCAL(splitsize)"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* number of times to attempt to steal in mainLoop */
#define STEAL_ATTEMPTS_MAINLOOP 16

/* number of times to attempt to steal in waitUntilComplete */
#define STEAL_ATTEMPTS_WAITING 8

/* var name in output code for accuracy template parameter */
#define TEMPLATE_BIN_STR "_acc_bin"

/* Signal used to kill processes that timeout */
#define TIMEOUTKILLSIG SIGKILL

/* Grace period before slow tests are killed */
#define TIMEOUT_GRACESEC 0.03

/* record timing information to stderr and jtimings.csv */
/* #undef TIMING */

/* prefix for expanded template implementations */
#define TMPL_IMPL_PFX "_tmpl"

/* define to add a system var to transform scope */
#define TRANSFORM_N_STR "transform_n"

/* postfix for dynamic versions of transforms */
#define TX_DISTRIB_POSTFIX "_distrib"

/* postfix for dynamic versions of transforms */
#define TX_DYNAMIC_POSTFIX "_worksteal"

/* postfix for opencl versions of transforms */
#define TX_OPENCL_POSTFIX "_opencl"

/* postfix for static versions of transforms */
#define TX_STATIC_POSTFIX "_seq"

/* enable unsafe optimizations such as removing error checking. */
/* #undef UNSAFE */

/* define if clock_gettime() doesnt work */
/* #undef USE_GETTIMEOFDAY */

/* extra directory to search for files in */
#define UTIL_SEARCH_DIR_A "src"

/* extra directory to search for files in */
#define UTIL_SEARCH_DIR_B "scripts"

/* Version number of package */
#define VERSION "3.0"

/* support WorkerThread::inject() with a separate queue */
/* #undef WORKERTHREAD_INJECT */

/* use a special 1-element queue for the first item pushed, for better
   locality */
#define WORKERTHREAD_ONDECK 

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
/* #undef YYTEXT_POINTER */

/* Define for Solaris 2.5.1 so the uint32_t typedef from <sys/synch.h>,
   <pthread.h>, or <semaphore.h> is not used. If the typedef was allowed, the
   #define below would cause a syntax error. */
/* #undef _UINT32_T */

/* Define for Solaris 2.5.1 so the uint64_t typedef from <sys/synch.h>,
   <pthread.h>, or <semaphore.h> is not used. If the typedef was allowed, the
   #define below would cause a syntax error. */
/* #undef _UINT64_T */

/* Define to the type of an unsigned integer type of width exactly 32 bits if
   such a type exists and the standard includes do not define it. */
/* #undef uint32_t */

/* Define to the type of an unsigned integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
/* #undef uint64_t */
