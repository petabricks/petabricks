AC_INIT([petabricks], [3.0], [jansel@csail.mit.edu])

AC_CONFIG_SRCDIR([src/compiler/pbc.cpp])
AC_CANONICAL_TARGET

AM_CONFIG_HEADER([src/config.h])
AM_INIT_AUTOMAKE([foreign])

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
AC_PROG_LN_S

AM_PROG_LEX
if test "x$ac_cv_prog_LEX" = "x"; then
  AC_MSG_ERROR([failed to find 'flex' in \$path, try running:`sudo apt-get install flex`.])
fi

AC_PROG_YACC
if test "x$ac_cv_prog_YACC" = "x"; then
  AC_MSG_ERROR([failed to find 'bison' in \$path, try running:`sudo apt-get install bison`.])
fi

AC_PATH_PROGS([DOTPATH], [dot], [no])
if test "$DOTPATH" = "no"; then
  AC_MSG_WARN([failed to find 'dot' in \$path, try running:`sudo apt-get install graphviz`.])
fi


AC_CHECK_PROG([has_doxygen], [doxygen], [yes], [no])
if test "$has_doxygen" = "no"; then
  AC_MSG_WARN([failed to find 'doxygen' in \$path, documentation will not be generated.])
fi

AC_CHECK_PROG([has_git], [git], [yes], [no])
if test "$has_git" = "no"; then
  AC_MSG_WARN([failed to find 'git' in \$path, will not be able to use revision control.])
fi

AC_PATH_PROGS([PYTHON], [python2.8 python2.7 python2.6 python2.5 python2.4 python2 python], [no])
if test "$PYTHON" = "no"; then
  AC_MSG_ERROR([failed to find 'python' in \$path, try running:`sudo apt-get install python`.])
fi

AC_PATH_PROGS([NM], [nm], [no])
if test "$NM" = "no"; then
  AC_MSG_ERROR([failed to find 'nm' in \$path, try running:`sudo apt-get install binutils`.])
fi

AC_PATH_PROGS([MAXIMA], [maxima], [no])
if test "$MAXIMA" = "no"; then
  AC_MSG_ERROR([failed to find 'maxima' in \$path, try running:`sudo apt-get install maxima`.])
fi

AC_MSG_CHECKING([python modules])
if "$PYTHON" "$srcdir/scripts/checkpython.py"; then
  AC_MSG_RESULT([ok])
else
  AC_MSG_ERROR(["missing python modules"])
fi

AC_MSG_CHECKING([maxima compatibility])
if "$PYTHON" "$srcdir/scripts/checkmaxima.py" "$MAXIMA"; then
  AC_MSG_RESULT([ok])
else
  AC_MSG_ERROR(["broken maxima version"])
fi

rev_short="unknown"
rev_long="unknown"
AC_MSG_CHECKING([git revision])
if test "$has_git" = "yes"; then
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
AC_CHECK_HEADERS([openssl/sha.h],  [], [])
AC_CHECK_HEADERS([cxxabi.h execinfo.h poll.h signal.h sys/prctl.h sys/select.h sys/socket.h sys/time.h sys/types.h sys/wait.h sys/mman.h])
AC_CHECK_HEADERS([boost/random.hpp], [], [AC_MSG_WARN([missing boost/random, falling back to slower random number generation])])
AC_CHECK_HEADERS([cblas.h],  [], [])
AC_CHECK_HEADERS([Accelerate/Accelerate.h], [], [])
AC_CHECK_HEADERS([sqlite3.h],      [], [AC_MSG_ERROR([failed to find sqlite3.h header file])])

AC_SEARCH_LIBS([MD5_Init], [crypto ssl], [], [AC_MSG_ERROR([missing package libssl-dev])])

AC_CHECK_LIB([cblas],      [cblas_dgemm],    [], [])
AC_CHECK_LIB([blas],       [cblas_dgemm],    [], [AC_MSG_WARN([failed to find -lblas, some benchmarks may not work])])
AC_CHECK_LIB([fftw3],      [fftw_malloc],    [], [AC_MSG_WARN([failed to find -lfftw3, some benchmarks may not work])])
AC_CHECK_LIB([lapack],     [dposv_],         [], [AC_MSG_WARN([failed to find -llapack, some benchmarks may not work])])
AC_CHECK_LIB([m],          [cos],            [], [AC_MSG_WARN([failed to find -lm, some benchmarks may not work])])
AC_CHECK_LIB([pthread],    [pthread_create], [], [AC_MSG_WARN([failed to find -lpthread, some benchmarks may not work])])
AC_CHECK_LIB([rt],         [clock_gettime],  [],
                        [AC_DEFINE([USE_GETTIMEOFDAY],[],[define if clock_gettime() doesnt work])])
AC_CHECK_LIB([sqlite3],	   [sqlite3_open],   [], [AC_MSG_ERROR([failed to find -lsqlite, unable to compile the long term learning for heuristics])])
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
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[inline __attribute__((flatten)) void foo(){}]])],
                  [
                    AC_DEFINE([INLINE],[inline __attribute__((flatten))], [keyword to force inlining])
                    AC_MSG_RESULT([yes]);
                  ],
                  [
                    AC_DEFINE([INLINE],[inline])
                    AC_MSG_RESULT([no]);
                  ])

AC_MSG_CHECKING([if compiler supports packed])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[struct foo{int x;} __attribute__((__packed__));]])],
                  [
                    AC_DEFINE([PACKED],[__attribute__((__packed__))], [keyword to force packed struct])
                    AC_MSG_RESULT([yes]);
                  ],
                  [
                    AC_DEFINE([PACKED],[])
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
                              [record timing information to jtimings.csv,
                               and to stderr.])],
              [use_jtiming=$enableval],
              [use_jtiming=no])

if test "$use_jtiming" = "yes"; then
  AC_DEFINE([TIMING],[],[record timing information to stderr and jtimings.csv])
fi

AC_ARG_WITH([opencl],
	[AS_HELP_STRING([--with-opencl=PATH],[build compiler and runtime with support for OpenCL-enabled GPUs.])],
	[],
	[with_opencl=no])

AC_ARG_WITH([amdopencl],
	[AS_HELP_STRING([--with-amdopencl=PATH],[build compiler and runtime with support for OpenCL-enabled CPUs and AMD GPUs.])],
	[],
	[with_amdopencl=no])

AC_ARG_WITH([macopencl],
	[AS_HELP_STRING([--with-macopencl=PATH],[build compiler and runtime with support for OpenCL on Mac OSX])],
	[],
	[with_macopencl=no])

AC_ARG_WITH([intelopencl],
	[AS_HELP_STRING([--with-intelopencl=PATH],[build compiler and runtime with support for OpenCL-enabled CPUs and intel GPUs.])],
	[],
	[with_intelopencl=no])

if ! test "$with_opencl" = "no"; then
   AC_DEFINE([HAVE_OPENCL], [1], [Enable OpenCL code generation])
   AC_DEFINE([MATRIX_ELEMENT_T],[float], [type for elements in the matrix])
   AC_DEFINE([NVIDIA],[1], [NVIDIA SDK])

   CPPFLAGS="$CPPFLAGS -I$with_opencl/OpenCL/common/inc -I$with_opencl/shared/inc"
   LIBS="$LIBS -L$with_opencl/OpenCL/common/lib/ -L$with_opencl/shared/lib -L$with_opencl/shared/lib/linux"

   AC_CHECK_LIB([OpenCL], [clGetDeviceIDs], [], [AC_MSG_ERROR([Failed to find -lOpenCL.  Check LIBRARY_PATH, or reconfigure without OpenCL.])])
   AC_CHECK_LIB([shrutil_x86_64], [shrLogEx], [], [AC_MSG_ERROR([Failed to find -lshrutil_x86_64.  Check LIBRARY_PATH, or reconfigure without shrutil_x86_64.])])
   AC_CHECK_LIB([oclUtil_x86_64], [oclGetPlatformID], [], [AC_MSG_ERROR([Failed to find -loclUtil_x86_64.  Check LIBRARY_PATH, or reconfigure without oclUtil_x86_64.])])

   AC_CHECK_HEADERS([oclUtils.h], [], [])

elif ! test "$with_amdopencl" = "no"; then
   AC_DEFINE([HAVE_OPENCL], [1], [Enable OpenCL code generation])
   AC_DEFINE([MATRIX_ELEMENT_T],[float], [type for elements in the matrix])
   AC_DEFINE([AMD],[1], [AMD SDK])

   CPPFLAGS="$CPPFLAGS -I$with_amdopencl/samples/opencl/SDKUtil/include -I$with_amdopencl/include"
   LIBS="$LIBS -L$with_amdopencl/lib/x86_64 -L$with_amdopencl/TempSDKUtil/lib/x86_64"

   AC_CHECK_LIB([OpenCL], [clGetDeviceIDs], [], [AC_MSG_ERROR([Failed to find -lOpenCL.  Check LIBRARY_PATH, or reconfigure without OpenCL.])])

   AC_CHECK_HEADERS([SDKCommon.hpp SDKApplication.hpp SDKFile.hpp], [], [])

elif ! test "$with_intelopencl" = "no"; then
   AC_DEFINE([HAVE_OPENCL], [1], [Enable OpenCL code generation])
   AC_DEFINE([MATRIX_ELEMENT_T],[float], [type for elements in the matrix])
   AC_DEFINE([INTEL],[1], [INTEL SDK])

   CPPFLAGS="$CPPFLAGS -I/usr/include/CL"
   LIBS="$LIBS -L/usr/lib64/OpenCL/vendors/intel"

   AC_CHECK_LIB([OpenCL], [clGetDeviceIDs], [], [AC_MSG_ERROR([Failed to find -lOpenCL.  Check LIBRARY_PATH, or reconfigure without OpenCL.])])
   
   AC_CHECK_HEADERS([opencl.h], [], [])

elif ! test "$with_macopencl" = "no"; then
   AC_DEFINE([HAVE_OPENCL], [1], [Enable OpenCL code generation])
   AC_DEFINE([MATRIX_ELEMENT_T], [float], [type for elements in the matrix])
   AC_DEFINE([MAC],[1],[MAC OS])
   
   LDFLAGS="$LDFLAGS -framework OpenCL"

   AC_CHECK_HEADERS([OpenCL/opencl.h], [], [])

else
   AC_DEFINE([MATRIX_ELEMENT_T],[double])
fi


AC_ARG_ENABLE([allocator],
            [AS_HELP_STRING([--disable-allocator],
                            [disable use of the custom memory block allocator])],
            [use_jalloc=$enableval],
            [use_jalloc=$has_threadlocal],])

if test "$with_amdopencl" = "no"; then
  if test "$use_jalloc" = "yes"; then
    AC_DEFINE([JALIB_ALLOCATOR],   [1], [Use a custom allocator from JALIB])
  fi
fi


AC_DEFINE([ACCTARGET_STR],       ["accuracyTarget"], [var name in output code for target accuracy])
AC_DEFINE([BIN_TMPFILE],         ["bin"],     [generated file name])
AC_DEFINE([CACHE_LINE_SIZE],     [64], [larges cache line size in system])
AC_DEFINE([COORD_BEGIN_STR],     ["_iter_begin"], [var name in output code for begin coord])
AC_DEFINE([COORD_END_STR],       ["_iter_end"], [var name in output code for end coord])
AC_DEFINE([DEFAULT_ACCURACY],    [1], [accuracy stand-in for fixed-accuracy rules])
AC_DEFINE([DEVNULL],             ["/dev/null"], [null sync device path])
AC_DEFINE([DISABLE_DISTRIBUTED], [1], [dont generate distributed code])
AC_DEFINE([DISTRIBUTED_GC_FREQ], [1000], [after how many allocations to invoke distributed gc])
AC_DEFINE([FORENOUGH_MAX_ITERS], [1000], [the default maximum number of iterations for for_enough{}])
AC_DEFINE([FORENOUGH_MIN_ITERS], [1], [the default minimum number of iterations for for_enough{}])
AC_DEFINE([GENHEADER],           ["_pbhdr.h"], [generated file name])
AC_DEFINE([GENMISC],             ["_pbmisc"], [generated file name])
AC_DEFINE([HASH_USE_SHA1],       [0], [set to 1 to use SHA1 instead of MD5])
AC_DEFINE([INLINE_NULL_TASKS],   [], [run null tasks immediately when they are enqueued])
AC_DEFINE([JASSERT_USE_SRCPOS],  [],[include source line numbers in error messages])
AC_DEFINE([LISTEN_PORT_FIRST],   [22550], [first port to try to use to listen])
AC_DEFINE([MATRIX_INDEX_T],      [int], [type for indices into the matrix])
AC_DEFINE([MAX_DIMENSIONS],      [64], [the maximum number of dimensions supported])
AC_DEFINE([MAX_INPUT_BITS],      [32], [the maximum number of dimensions supported])
AC_DEFINE([MAX_NUM_WORKERS],     [512], [max number of workers supported])
AC_DEFINE([MAX_REC_LEVELS],      [12], [number of levels for recursive choices])
AC_DEFINE([MIN_NUM_WORKERS],     [1], [min number of workers supported])
AC_DEFINE([OPT_LOWLEVEL],        [1], [enable low level optimization such as specializations and prefetching])
AC_DEFINE([RETURN_VAL_STR],      ["_pb_rv"], [var name in output code for rule output variable])
AC_DEFINE([SINGLE_SEQ_CUTOFF],   [1], [define to use a global sequential cutoff instead of a per-transform one])
AC_DEFINE([SPLIT_CHUNK_SIZE],    ["TRANSFORM_LOCAL(splitsize)"], [var name in output code])
AC_DEFINE([STEAL_ATTEMPTS_MAINLOOP], [16], [number of times to attempt to steal in mainLoop])
AC_DEFINE([STEAL_ATTEMPTS_WAITING],  [8], [number of times to attempt to steal in waitUntilComplete])
AC_DEFINE([TEMPLATE_BIN_STR],    ["_acc_bin"], [var name in output code for accuracy template parameter])
AC_DEFINE([TIMEOUT_GRACESEC],    [0.03], [Grace period before slow tests are killed])
AC_DEFINE([TIMEOUTKILLSIG],      [SIGKILL], [Signal used to kill processes that timeout])
AC_DEFINE([TMPL_IMPL_PFX],       ["_tmpl"], [prefix for expanded template implementations])
AC_DEFINE([TRANSFORM_N_STR],     ["transform_n"], [define to add a system var to transform scope])
AC_DEFINE([TX_DISTRIB_POSTFIX],  ["_distributed"], [postfix for dynamic versions of transforms])
AC_DEFINE([TX_DYNAMIC_POSTFIX],  ["_workstealing"], [postfix for dynamic versions of transforms])
AC_DEFINE([TX_OPENCL_POSTFIX],   ["_opencl"], [postfix for opencl versions of transforms])
AC_DEFINE([TX_STATIC_POSTFIX],   ["_sequential"], [postfix for static versions of transforms])
AC_DEFINE([UTIL_SEARCH_DIR_A],   ["src"], [extra directory to search for files in])
AC_DEFINE([UTIL_SEARCH_DIR_B],   ["scripts"], [extra directory to search for files in])
AC_DEFINE([WORKERTHREAD_INJECT], [], [support WorkerThread::inject() with a separate queue])
AC_DEFINE([WORKERTHREAD_ONDECK], [], [use a special 1-element queue for the first item pushed, for better locality])
AC_DEFINE([JASSERT_FAST],        [], [skip some copies in debug printing, conflicts with JASSERT_LOG])

AC_DEFINE_UNQUOTED([MAXIMA_PATH],    ["$MAXIMA"], [path to maxima program])
AC_DEFINE_UNQUOTED([REVISION_LONG],  ["$rev_long"], [git revision for HEAD at the time ./configure was run])
AC_DEFINE_UNQUOTED([REVISION_SHORT], ["$rev_short"], [git revision for HEAD at the time ./configure was run])

if false; then
  #disabled by default:
  AC_DEFINE([DISABLE_CONTINUATIONS], [], [dont generate continuation code after SPAWN/enqueue])
  AC_DEFINE([INPUT_PERIMETER_STR], ["input_perimeter"], [define to add a system var to transform scope])
  AC_DEFINE([INPUT_SIZE_STR],      ["input_count"],      [define to add a system var to transform scope])
  AC_DEFINE([JALIB_USE_MALLOC],    [1],[Cause the custom allocator to call malloc instead of mmap])
  AC_DEFINE([JASSERT_FAST],        [], [skip some copies in debug printing, conflicts with JASSERT_LOG])
  AC_DEFINE([JASSERT_LOG],         [], [log traces to /tmp/jassertlog.pid])
  AC_DEFINE([MAXIMA_CACHING],      [], [cache results from maxima])
  AC_DEFINE([OUTPUT_SIZE_STR],     ["output_count"],     [define to add a system var to transform scope])
  AC_DEFINE([PBCC_SEQUENTIAL],     [], [define to run tasks inline instead of enqueuing them])
  AC_DEFINE([REGIONMATRIX_TEST],   [1], [always use RegionMatrix code])
  AC_DEFINE([DISTRIBUTED_CACHE],   [], [use cache in distributed code])
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
    compileflags="-O2 $compileflags"
  fi
  case "$target_cpu" in
    *sparc*) compileflags="$compileflags -mcpu=niagara -mtune=native";;
    *)       compileflags="$compileflags -march=native";;
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
AC_DEFINE_UNQUOTED([CXXFLAGS],   ["$CXXFLAGS $CPPFLAGS"],   [C++ compiler flags used by pbc])
AC_DEFINE_UNQUOTED([CXXLDFLAGS], ["$LDFLAGS"],              [C++ compiler link flags used by pbc])
AC_DEFINE_UNQUOTED([CXXLIBS],    ["$LIBS"],                 [C++ compiler libraries used by pbc])
AC_DEFINE_UNQUOTED([NM],         ["$NM"],                   [binutils tool used by pbc])
AC_DEFINE_UNQUOTED([PYTHON],     ["$PYTHON"],               [path to python interpreter])

################################################################################
################################################################################
################################################################################

AC_OUTPUT(Makefile src/Makefile)

