AC_INIT([petabricks], [1.99], [jansel@csail.mit.edu])

AC_CONFIG_SRCDIR([src/pbc.cpp])
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

AC_DISABLE_SHARED
AM_PROG_LIBTOOL

AC_LANG_CPLUSPLUS
AC_PROG_CXX
AC_PROG_CXXCPP
AM_PROG_LEX
AC_PROG_YACC
AC_PROG_EGREP

AC_FUNC_MMAP
AC_CHECK_FUNCS([backtrace_symbols])
#AC_CHECK_FUNCS([getdelim getline gettimeofday backtrace_symbols])

AC_TYPE_UINT32_T
AC_TYPE_UINT64_T


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
  rev_short=`git-log -n 1 --pretty=format:%h`
  rev_long=`git-log -n 1 --pretty=format:%H`
fi
AC_MSG_RESULT([$rev_long])



################################################################################
# LIBRARIES ####################################################################
################################################################################

AC_CHECK_HEADERS([math.h fftw3.h execinfo.h cxxabi.h], [], [AC_MSG_WARN([failed to find header file, some benchmarks may not work])])
AC_CHECK_LIB([m],[cos],                  [], [AC_MSG_WARN([failed to find -lm, some benchmarks may not work])])
AC_CHECK_LIB([fftw3], [fftw_malloc],     [], [AC_MSG_WARN([failed to find -lfftw3, some benchmarks may not work])])
AC_CHECK_LIB([pthread],[pthread_create], [], [AC_MSG_WARN([failed to find -lpthread, some benchmarks may not work])])
AC_CHECK_LIB([lapack], [dposv_],         [], [AC_MSG_WARN([failed to find -llapack, some benchmarks may not work])])
#AC_CHECK_LIB([gslcblas],[cblas_dgemm],   [], [AC_MSG_WARN([failed to find -lgslcblas, some benchmarks may not work])]) 
#AC_CHECK_LIB([gsl],[gsl_blas_dgemm],     [], [AC_MSG_WARN([failed to find -lgsl, some benchmarks may not work])])

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

AC_DEFINE([MATRIX_ELEMENT_T],  [double],         [type for elements in the matrix])
AC_DEFINE([MAX_DIMENSIONS],    [64],             [the maximum number of dimensions supported])
AC_DEFINE([GRACEFUL_ABORT],    [1],              [allow abort without program shutdown (uses exceptions)])
AC_DEFINE([SHORT_TYPE_NAMES],  [1],              [undefine this if a large number of dimensions are needed, or one wants a cleaner user namespace])
AC_DEFINE([RUN_CPP],           [1],              [undefine to disable cpp])
AC_DEFINE([MAX_REC_LEVELS],    [12],             [number of levels for recursive choices])
AC_DEFINE([INPUT_SIZE_STR],    ["input_count"],  [var name in output code])
AC_DEFINE([OUTPUT_SIZE_STR],   ["output_count"], [var name in output code])
AC_DEFINE([SPLIT_CHUNK_SIZE],  ["chunk_size"],   [var name in output code])
AC_DEFINE([INPUT_PERIMETER_STR],  ["_input_perimeter"],   [var name in output code])
AC_DEFINE([TMPL_IMPL_PFX],     ["_tmpl"],        [prefix for expanded template implementations])
AC_DEFINE([TX_STATIC_POSTFIX], ["_static"], [postfix for static versions of transforms])
AC_DEFINE([TX_DYNAMIC_POSTFIX], ["_dynamic"], [postfix for static versions of transforms])

AC_DEFINE([MAX_NUM_WORKERS], [64], [max number of workers supported])
AC_DEFINE([MIN_NUM_WORKERS], [0 ], [min number of workers supported])
AC_DEFINE([CACHE_LINE_SIZE], [64], [larges cache line size in system])




if false; then
  #disabled by default:
  AC_DEFINE([JASSERT_LOG],       [],               [log traces to /tmp/jassertlog.pid])
  AC_DEFINE([JALIB_ALLOCATOR],   [1],              [Use a custom allocator from JALIB])
  AC_DEFINE([JALIB_USE_MALLOC],  [],               [Cause the custom allocator to call malloc instead of mmap])
  AC_DEFINE([DISABLE_CONTINUATIONS],  [],          [don't generate continuation code after SPAWN/enqueue])
fi

AC_DEFINE_UNQUOTED([REVISION_SHORT],  ["$rev_short"],   [git sha1 hash for HEAD at the time ./configure was run])
AC_DEFINE_UNQUOTED([REVISION_LONG],   ["$rev_long"],    [git sha1 hash for HEAD at the time ./configure was run])

################################################################################
# COMPILER FLAGS ###############################################################
################################################################################

compileflags=""
linkflags=""
  
if test "$use_jassert" = "yes"; then
  linkflags="-rdynamic"
fi

if test "$default_cxxflags" = "yes"; then
  if test "$use_jassert" = "yes"; then
    CXXFLAGS=""
    compileflags="-O0 -g"
  else
    CXXFLAGS=""
    compileflags="-O3"
  fi
  case $target_cpu in
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

AC_OUTPUT(Makefile src/Makefile src/cxxconfig.h examples/Makefile)

