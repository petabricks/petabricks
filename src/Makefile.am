INCLUDES = -I$(srcdir)/common
METASOURCES = AUTO
AM_YFLAGS = 
AM_LFLAGS = 
OBJDIR=obj

noinst_LIBRARIES = libpbcommon.a libpbcompiler.a libpbruntime.a libpbmain.a
noinst_PROGRAMS = pbc 

noinst_HEADERS = \
  compiler/choicegrid.h \
  compiler/codegenerator.h \
  compiler/formula.h \
  compiler/iterationorders.h \
  compiler/learner.h \
  compiler/matrixdef.h \
  compiler/matrixdependency.h \
  compiler/maximawrapper.h \
  compiler/performancetester.h \
  compiler/region.h \
  compiler/rircompilerpass.h \
  compiler/rirscope.h \
  compiler/rulechoice.h \
  compiler/rule.h \
  compiler/ruleir.h \
  compiler/staticscheduler.h \
  compiler/syntheticrule.h \
  compiler/trainingdeps.h \
  compiler/transform.h \
  compiler/userrule.h \
  compiler/maximaparser.h \
  compiler/pbparser.h \
  compiler/ruleirparser.h

pkglib_HEADERS=  \
  common/jalloc.h \
  common/jargs.h \
  common/jasm.h \
  common/jassert.h \
  common/jblockingqueue.h \
  common/jconvert.h \
  common/jfilesystem.h \
  common/jmutex.h \
  common/jprintable.h \
  common/jrefcounted.h \
  common/jserialize.h \
  common/jsocket.h \
  common/jtimer.h \
  common/jtunable.h \
  runtime/autotuner.h \
  runtime/dynamicscheduler.h \
  runtime/dynamictask.h \
  runtime/matrix.h \
  runtime/matrixio.h \
  runtime/petabricks.h \
  runtime/petabricksruntime.h \
  runtime/ruleinstance.h \
  runtime/spatialdynamictask.h \
  runtime/transforminstance.h

libpbcommon_a_CXXFLAGS = 
libpbcommon_a_SOURCES =  \
  common/jalloc.cpp \
  common/jargs.cpp \
  common/jassert.cpp \
  common/jfilesystem.cpp \
  common/jprintable.cpp \
  common/jrefcounted.cpp \
  common/jserialize.cpp \
  common/jsocket.cpp \
  common/jtimer.cpp \
  common/jtunable.cpp

libpbcompiler_a_LFLAGS = 
libpbcompiler_a_YFLAGS = -d
libpbcompiler_a_CXXFLAGS = -I$(srcdir)/compiler
libpbcompiler_a_SOURCES = \
  compiler/choicegrid.cpp \
  compiler/codegenerator.cpp \
  compiler/formula.cpp \
  compiler/iterationorders.cpp \
  compiler/learner.cpp \
  compiler/matrixdef.cpp \
  compiler/matrixdependency.cpp \
  compiler/maximalexer.lpp \
  compiler/maximaparser.ypp \
  compiler/maximawrapper.cpp \
  compiler/pblexer.lpp \
  compiler/pbparser.ypp \
  compiler/performancetester.cpp \
  compiler/region.cpp \
  compiler/rircompilerpass.cpp \
  compiler/rirscope.cpp \
  compiler/rulechoice.cpp \
  compiler/rule.cpp \
  compiler/ruleir.cpp \
  compiler/ruleirlexer.lpp \
  compiler/ruleirparser.ypp \
  compiler/staticscheduler.cpp \
  compiler/syntheticrule.cpp \
  compiler/trainingdeps.cpp \
  compiler/transform.cpp \
  compiler/userrule.cpp

libpbruntime_a_LFLAGS = 
libpbruntime_a_CXXFLAGS = -I$(srcdir)/runtime
libpbruntime_a_SOURCES =  \
  runtime/autotuner.cpp \
  runtime/dynamicscheduler.cpp \
  runtime/dynamictask.cpp \
  runtime/matrix.cpp \
  runtime/matrixio.cpp \
  runtime/matrixreader.lpp \
  runtime/petabricksruntime.cpp \
  runtime/ruleinstance.cpp \
  runtime/spatialdynamictask.cpp \
  runtime/transforminstance.cpp

libpbmain_a_CXXFLAGS = -Iruntime
libpbmain_a_SOURCES = runtime/redirectmain.cpp

pbc_CXXFLAGS = -Icompiler
pbc_SOURCES = compiler/pbc.cpp
pbc_LDADD = libpbcompiler.a libpbcommon.a 

CLEANFILES = libpbcompiler_a-maximalexer.cpp libpbcompiler_a-maximaparser.cpp libpbcompiler_a-maximaparser.h \
             libpbcompiler_a-pblexer.cpp libpbcompiler_a-pbparser.cpp libpbcompiler_a-pbparser.h \
             libpbcompiler_a-ruleirlexer.cpp libpbcompiler_a-ruleirparser.cpp libpbcompiler_a-ruleirparser.h \
             libpbcompiler_a-maximaparser.output libpbcompiler_a-pbparser.output libpbcompiler_a-ruleirparser.output \
             libpbruntime_a-matrixreader.cpp

#copy bison headers to compiler dir
compiler/maximaparser.h: libpbcompiler_a-maximaparser.cpp
	cp libpbcompiler_a-maximaparser.h $(srcdir)/compiler/maximaparser.h
compiler/pbparser.h:		 libpbcompiler_a-pbparser.cpp
	cp libpbcompiler_a-pbparser.h     $(srcdir)/compiler/pbparser.h
compiler/ruleirparser.h: libpbcompiler_a-ruleirparser.cpp
	cp libpbcompiler_a-ruleirparser.h $(srcdir)/compiler/ruleirparser.h




