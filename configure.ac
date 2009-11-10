AC_INIT([petabricks], [1.99], [jansel@csail.mit.edu])

AC_CONFIG_SRCDIR([src/compiler/pbc.cpp])
AC_CANONICAL_TARGET

AM_CONFIG_HEADER([src/config.h])
AM_INIT_AUTOMAKE

default_cxxflags=no
if test "x$CXXFLAGS" = "x"; then
  default_cxxflags=yes
fi

################################################################################
# PROGRAMS ####################################################################
################################################################################

AC_LANG_CPLUSPLUS
AC_PROG_CXX
AC_PROG_CXXCPP
AC_PROG_EGREP
AC_PROG_RANLIB

AM_PROG_LEX
if test "x$ac_cv_prog_LEX" = "x"; then
  AC_MSG_ERROR([failed to find 'flex' in \$path, try running:`sudo apt-get install flex`.])
fi

AC_PROG_YACC
if test "x$ac_cv_prog_YACC" = "x"; then
  AC_MSG_ERROR([failed to find 'bison' in \$path, try running:`sudo apt-get install bison`.])
fi

AC_CHECK_PROG([has_maxima], [maxima], [yes], [no])
if test "$has_maxima" = "no"; then
  AC_MSG_ERROR([failed to find 'maxima' in \$path, try running:`sudo apt-get install maxima`.])
fi

AC_CHECK_PROG([has_dot], [dot], [yes], [no])
if test "$has_dot" = "no"; then
  AC_MSG_WARN([failed to find 'dot' in \$path, some debug outputs will not be generated.])
fi

AC_CHECK_PROG([has_doxygen], [doxygen], [yes], [no])
if test "$has_doxygen" = "no"; then
  AC_MSG_WARN([failed to find 'doxygen' in \$path, documentation will not be generated.])
fi

AC_CHECK_PROG([has_git], [git], [yes], [no])
if test "$has_git" = "no"; then
  AC_MSG_WARN([failed to find 'git' in \$path, will not be able to use revision control.])
fi

rev_short="unknown"
rev_long="unknown"
AC_MSG_CHECKING([git revision])
if test "$has_git" = "yes" -a -d .git ; then
  rev_short=`git log -n 1 --pretty=format:%h`
  rev_long=`git log -n 1 --pretty=format:%H`
fi
AC_MSG_RESULT([$rev_long])


################################################################################
# LIBRARIES ####################################################################
################################################################################


AC_FUNC_MMAP
AC_CHECK_FUNCS([backtrace_symbols])

AC_TYPE_UINT32_T
AC_TYPE_UINT64_T

AC_CHECK_HEADERS([math.h fftw3.h], [], [AC_MSG_WARN([failed to find header file, some benchmarks may not work])])
AC_CHECK_HEADERS([openssl/md5.h],  [], [AC_MSG_ERROR([missing package libssl-dev])])
AC_CHECK_HEADERS([cxxabi.h execinfo.h signal.h sys/prctl.h sys/select.h sys/socket.h sys/time.h sys/types.h sys/wait.h sys/mman.h])
AC_CHECK_LIB([ssl],[MD5],                [], [AC_MSG_ERROR([missing package libssl-dev])])
AC_CHECK_LIB([m],[cos],                  [], [AC_MSG_WARN([failed to find -lm, some benchmarks may not work])])
AC_CHECK_LIB([fftw3], [fftw_malloc],     [], [AC_MSG_WARN([failed to find -lfftw3, some benchmarks may not work])])
AC_CHECK_LIB([pthread],[pthread_create], [], [AC_MSG_WARN([failed to find -lpthread, some benchmarks may not work])])
AC_CHECK_LIB([lapack], [dposv_],         [], [AC_MSG_WARN([failed to find -llapack, some benchmarks may not work])])
AC_CHECK_LIB([rt], [clock_gettime],      [], [AC_DEFINE([USE_GETTIMEOFDAY],[],[define if clock_gettime() doesnt work])])
#AC_CHECK_LIB([gslcblas],[cblas_dgemm],   [], [AC_MSG_WARN([failed to find -lgslcblas, some benchmarks may not work])]) 
#AC_CHECK_LIB([gsl],[gsl_blas_dgemm],     [], [AC_MSG_WARN([failed to find -lgsl, some benchmarks may not work])])

################################################################################
# COMPILER PROPERTIES ##########################################################
################################################################################

AC_MSG_CHECKING([if compiler supports __thread])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[__thread int x;]])],
                  [  
                    AC_MSG_RESULT([yes]);
                    has_threadlocal=yes; 
                    AC_DEFINE([HAVE_THREADLOCAL],[],[define if __thread works with this compiler])
                  ],
                  [  
                    AC_MSG_RESULT([no]); 
                    has_threadlocal=no;
                  ])

AC_MSG_CHECKING([if compiler supports forced inlining])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[inline __attribute__((always_inline)) __attribute__((flatten)) void foo(){}]])],
                  [  
                    AC_DEFINE([INLINE],[inline __attribute__((always_inline)) __attribute__((flatten))], [keyword to force inlining])
                    AC_MSG_RESULT([yes]);
                  ],
                  [  
                    AC_DEFINE([INLINE],[inline])
                    AC_MSG_RESULT([no]); 
                  ])

AC_MSG_CHECKING([if compiler supports __restrict__])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[void foo(int *__restrict__ t);]])],
                  [  
                    AC_DEFINE([RESTRICT],[__restrict__], [keyword to indicate non-aliased pointers/refs])
                    AC_MSG_RESULT([yes]);
                  ],
                  [  
                    AC_DEFINE([RESTRICT],[])
                    AC_MSG_RESULT([no]); 
                  ])

AC_MSG_CHECKING([if compiler supports __attribute__])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[__attribute__((undefinedabdef)) void foo();]])],
                  [  
                    AC_DEFINE([ATTRIBUTE(x)],[__attribute__((x))], [macro for gcc function __attribute__s])
                    AC_MSG_RESULT([yes]);
                  ],
                  [  
                    AC_DEFINE([ATTRIBUTE(x)],[])
                    AC_MSG_RESULT([no]); 
                  ])

################################################################################
# DEFINES   ####################################################################
################################################################################

AC_ARG_ENABLE([debug],
            [AS_HELP_STRING([--enable-debug],
                            [enable (very) verbose debug printouts
                             and write log files to /tmp (default is
                             disabled)])],
            [use_jassert=$enableval],
            [use_jassert=no])
if test "$use_jassert" = "yes"; then
  AC_DEFINE([DEBUG],[],[verbose debug printouts and log files in /tmp])
  if test "`hostname`" = "kleptocracy"; then
    AC_DEFINE([MAXIMA_LOG],[1],[define in order to generate maxima.log, (a log of maxima commands)])
  fi
  YFLAGS="$YFLAGS -v"
fi

AC_ARG_ENABLE([unsafe],
            [AS_HELP_STRING([--enable-unsafe],
                            [enable unsafe optimizations such as removing error checking.])],
            [use_unsafe=$enableval],
            [use_unsafe=no])

if test "$use_unsafe" = "yes"; then
  AC_DEFINE([UNSAFE],[],[enable unsafe optimizations such as removing error checking.])
fi


AC_ARG_ENABLE([timing],
            [AS_HELP_STRING([--enable-timing],
                            [record checkpoint/restart timing information
                            to jtimings.csv, in working directory of
                            dmtcp_coordinator, and to stderr.])],
            [use_jtiming=$enableval],
            [use_jtiming=no])

if test "$use_jtiming" = "yes"; then
  AC_DEFINE([TIMING],[],[record timing information to stderr and jtimings.csv])
fi

AC_ARG_ENABLE([allocator],
            [AS_HELP_STRING([--disable-allocator],
                            [disable use of the custom memory block allocator])],
            [use_jalloc=$enableval],
            [use_jalloc=$has_threadlocal],])

if test "$use_jalloc" = "yes"; then
  AC_DEFINE([JALIB_ALLOCATOR],   [1], [Use a custom allocator from JALIB])
fi

AC_DEFINE([ACCIMPROVETRIES],     [3], [number of increments tried with no accuracy improvement])
AC_DEFINE([CACHE_LINE_SIZE],     [64], [larges cache line size in system])
AC_DEFINE([COORD_BEGIN_STR],     ["_iter_begin"], [var name in output code for begin coord])
AC_DEFINE([COORD_END_STR],       ["_iter_end"], [var name in output code for end coord])
AC_DEFINE([FORENOUGH_MAX_ITERS], [1000], [the default maximum number of iterations for for_enough{}])
AC_DEFINE([FORENOUGH_MIN_ITERS], [1], [the default minimum number of iterations for for_enough{}])
AC_DEFINE([HASH_FN],             [MD5], [hash function used for memoization (MD5 or SHA1)])
AC_DEFINE([HASH_LEN],            [MD5_DIGEST_LENGTH], [length of hash used for memoization (must match HASH_FN)])
AC_DEFINE([INLINE_NULL_TASKS],   [], [run null tasks immediately when they are enqueued])
AC_DEFINE([MATRIX_ELEMENT_T],    [double], [type for elements in the matrix])
AC_DEFINE([MATRIX_INDEX_T],      [int], [type for indices into the matrix])
AC_DEFINE([MAX_DIMENSIONS],      [64], [the maximum number of dimensions supported])
AC_DEFINE([MAX_INPUT_BITS],      [32], [the maximum number of dimensions supported])
AC_DEFINE([MAX_NUM_WORKERS],     [512], [max number of workers supported])
AC_DEFINE([MAX_REC_LEVELS],      [6], [number of levels for recursive choices])
AC_DEFINE([MIN_NUM_WORKERS],     [1], [min number of workers supported])
AC_DEFINE([RETURN_VAL_STR],      ["_pb_rv"], [var name in output code for rule output variable])
AC_DEFINE([RUN_CPP],             [1], [undefine to disable cpp])
AC_DEFINE([SHORT_TYPE_NAMES],    [1], [add using namespace petabricks to output])
AC_DEFINE([SPLIT_CHUNK_SIZE],    ["TRANSFORM_LOCAL(splitsize)"], [var name in output code])
AC_DEFINE([STEAL_ATTEMPTS_MAINLOOP], [32], [number of times to attempt to steal in mainLoop])
AC_DEFINE([STEAL_ATTEMPTS_WAITING],  [3], [number of times to attempt to steal in waitUntilComplete])
AC_DEFINE([TEMPLATE_BIN_STR],    ["_acc_bin"], [var name in output code for accuracy template parameter])
AC_DEFINE([TIMEOUT_GRACESEC],    [0.2], [Grace period before slow tests are killed])
AC_DEFINE([TIMEOUTKILLSIG],      [SIGKILL], [Signal used to kill processes that timeout])
AC_DEFINE([TMPL_IMPL_PFX],       ["_tmpl"], [prefix for expanded template implementations])
AC_DEFINE([TRANSFORM_N_STR],     ["transform_n"], [define to add a system var to transform scope])
AC_DEFINE([TX_DYNAMIC_POSTFIX],  ["_dynamic"], [postfix for static versions of transforms])
AC_DEFINE([TX_STATIC_POSTFIX],   ["_static"], [postfix for static versions of transforms])
AC_DEFINE([UTIL_SEARCH_DIR_A],   ["src"], [extra directory to search for files in])
AC_DEFINE([UTIL_SEARCH_DIR_B],   ["scripts"], [extra directory to search for files in])
AC_DEFINE([WORKERTHREAD_ONDECK], [], [use a special 1-element queue for the first item pushed, for better locality])

AC_DEFINE_UNQUOTED([REVISION_LONG], ["$rev_long"], [git revision for HEAD at the time ./configure was run])
AC_DEFINE_UNQUOTED([REVISION_SHORT], ["$rev_short"], [git revision for HEAD at the time ./configure was run])


if false; then
  #disabled by default:
  AC_DEFINE([DISABLE_CONTINUATIONS], [], [dont generate continuation code after SPAWN/enqueue])
  AC_DEFINE([INPUT_PERIMETER_STR], ["input_perimeter"], [define to add a system var to transform scope])
  AC_DEFINE([INPUT_SIZE_STR],     ["input_count"],      [define to add a system var to transform scope])
  AC_DEFINE([JALIB_USE_MALLOC],      [1],[Cause the custom allocator to call malloc instead of mmap])
  AC_DEFINE([JASSERT_LOG],           [], [log traces to /tmp/jassertlog.pid])
  AC_DEFINE([OUTPUT_SIZE_STR],    ["output_count"],     [define to add a system var to transform scope])
  AC_DEFINE([PBCC_SEQUENTIAL], [], [define to run tasks inline instead of enqueuing them])
  AC_DEFINE([WORKERTHREAD_INJECT], [], [support WorkerThread::inject() with a separate queue])
fi

################################################################################
# COMPILER FLAGS ###############################################################
################################################################################

compileflags="-Wall -Wextra"
linkflags=""
  
if test "$use_jassert" = "yes"; then
  linkflags="-rdynamic"
fi

if test "$default_cxxflags" = "yes"; then
  if test "$use_jassert" = "yes"; then
    CXXFLAGS=""
    compileflags="-O0 -g $compileflags -fdiagnostics-show-option"
  else
    CXXFLAGS=""
    compileflags="-O3 $compileflags"
  fi
  case "$target_cpu" in
    *sparc*) compileflags="$compileflags -mcpu=niagara -mtune=native";;
    *)       compileflags="$compileflags -march=native -fno-rtti";;
  esac
fi

for extraflag in $compileflags
do
  save_CXXFLAGS="$CXXFLAGS"
  AC_MSG_CHECKING([if compiler supports $extraflag])
  CXXFLAGS="$CXXFLAGS $extraflag"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
                    [AC_MSG_RESULT([yes])],
                    [AC_MSG_RESULT([no]);      CXXFLAGS="$save_CXXFLAGS"])
done

for extraflag in $linkflags
do
  save_LDFLAGS="$LDFLAGS"
  AC_MSG_CHECKING([if linker supports $extraflag])
  LDFLAGS="$LDFLAGS $extraflag"
  AC_LINK_IFELSE([AC_LANG_PROGRAM()],
                 [AC_MSG_RESULT([yes])],
                 [AC_MSG_RESULT([no]);      LDFLAGS="$save_LDFLAGS"])
done

################################################################################
################################################################################
################################################################################

#pass compiler configuration to config.h
AC_DEFINE_UNQUOTED([CXX],        ["$CXX"],                  [C++ compiler used by pbc])
AC_DEFINE_UNQUOTED([CXXCPP],     ["$CXXCPP -x c++"],        [C++ preprocessor used by pbc])
AC_DEFINE_UNQUOTED([CXXDEFS],    ["$DEFS -DHAVE_CONFIG_H"], [C++ compiler defines used by pbc])
AC_DEFINE_UNQUOTED([CXXFLAGS],   ["$CXXFLAGS"],             [C++ compiler flags used by pbc])
AC_DEFINE_UNQUOTED([CXXLDFLAGS], ["$LDFLAGS"],              [C++ compiler link flags used by pbc])
AC_DEFINE_UNQUOTED([CXXLIBS],    ["$LIBS"],                 [C++ compiler libraries used by pbc])

################################################################################
################################################################################
################################################################################

AC_OUTPUT(Makefile src/Makefile)

