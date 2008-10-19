AC_INIT(configure.ac)

AM_CONFIG_HEADER(src/config.h)
AM_INIT_AUTOMAKE(hecura, 0.10)

AC_LANG_CPLUSPLUS
AC_PROG_CXX
AM_PROG_LIBTOOL
AM_PROG_LEX
AC_PROG_YACC

AC_CHECK_PROG([has_maxima], [maxima], [yes], [no])
if test "$has_maxima" = "no"; then
  AC_MSG_ERROR([failed to find maxima in \$path. (try running: sudo apt-get install maxima)])
fi

AC_CHECK_PROG([has_dot], [dot], [yes], [no])
if test "$has_dot" = "no"; then
  AC_MSG_WARN([failed to find dot in \$path. (try running: sudo apt-get install graphviz)])
fi


AC_DEFINE([MATRIX_ELEMENT_T],[double],[type for elements in the matrix])
AC_DEFINE([MAX_DIMENSIONS],[64],[the maximum number of dimensions supported])
AC_DEFINE([SHORT_TYPE_NAMES],[1],[undefine this if a large number of dimensions are needed, or one wants a cleaner user namespace])


AC_DEFINE([INPUT_SIZE_STR],["input_size"],[var name in output code])
AC_DEFINE([OUTPUT_SIZE_STR],["output_size"],[var name in output code])


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

AC_OUTPUT(Makefile src/Makefile src/cxxconfig.h)

