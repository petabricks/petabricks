#INCLUDES =
METASOURCES = AUTO
AM_YFLAGS =
AM_LFLAGS =
OBJDIR=obj

noinst_LIBRARIES = libpbcommon.a libpbcompiler.a libpbruntime.a libpbmain.a
noinst_PROGRAMS = pbc rttest1 rttest2 rttestmm regionmatrixtest migrationtest

noinst_HEADERS = \
  compiler/choicedepgraph.h \
  compiler/choicegrid.h \
  compiler/clcodegenerator.h \
  compiler/codegenerator.h \
  compiler/configitem.h \
  compiler/formula.h \
  compiler/gpurule.h \
  compiler/iterationorders.h \
  compiler/matrixdef.h \
  compiler/matrixdependency.h \
  compiler/maximawrapper.h \
  compiler/pbc.h \
  compiler/region.h \
  compiler/rircompilerpass.h \
  compiler/rirscope.h \
  compiler/rulechoice.h \
  compiler/rule.h \
  compiler/ruleir.h \
  compiler/scheduler.h \
  compiler/syntheticrule.h \
  compiler/trainingdeps.h \
  compiler/transform.h \
  compiler/userrule.h \
  compiler/heuristic.h \
  compiler/heuristicmanager.h \
  compiler/dbmanager.h \
  compiler/tinyxml.h \
  compiler/tinystr.h \
  common/hash.h \
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
  common/openclutil.h \
  common/srcpos.h \
  common/thedeque.h \
  runtime/cellproxy.h \
  runtime/distributedgc.h \
  runtime/dynamicscheduler.h \
  runtime/dynamictask.h \
  runtime/gpudynamictask.h \
  runtime/gpumanager.h \
  runtime/gputaskinfo.h \
  runtime/iregionreplyproxy.h \
  runtime/matrixio.h \
  runtime/matrixregion.h \
  runtime/matrixspecializations.h \
  runtime/matrixstorage.h \
  runtime/memoization.h \
  runtime/petabricks.h \
  runtime/petabricksruntime.h \
  runtime/regiondata0D.h \
  runtime/regiondatai.h \
  runtime/regiondataraw.h \
  runtime/regiondataremotecache.h \
  runtime/regiondataremote.h \
  runtime/regiondataremotemessages.h \
  runtime/regiondatasplit.h \
  runtime/regionhandler.h \
  runtime/regionmatrix.h \
  runtime/regionmatrixi.h \
  runtime/regionmatrixproxy.h \
  runtime/remotehost.h \
  runtime/remoteobject.h \
  runtime/remotetask.h \
  runtime/ruleinstance.h \
  runtime/specializeddynamictasks.h \
  runtime/testisolation.h \
  runtime/transforminstance.h \
  runtime/subregioncachemanager.h \
  runtime/workerthread.h

libpbcommon_a_CXXFLAGS = -I$(srcdir)/common
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
  common/jtunable.cpp \
  common/openclutil.cpp \
  common/srcpos.cpp

libpbcompiler_a_LFLAGS =
libpbcompiler_a_YFLAGS = -d
libpbcompiler_a_CXXFLAGS = -I. -I$(srcdir)/compiler
libpbcompiler_a_SOURCES = \
  compiler/choicedepgraph.cpp \
  compiler/choicegrid.cpp \
  compiler/clcodegenerator.cpp \
  compiler/codegenerator.cpp \
  compiler/configitem.cpp \
  compiler/formula.cpp \
  compiler/gpurule.cpp \
  compiler/iterationorders.cpp \
  compiler/matrixdef.cpp \
  compiler/matrixdependency.cpp \
  compiler/maximalexer.lpp \
  compiler/maximawrapper.cpp \
  compiler/region.cpp \
  compiler/rircompilerpass.cpp \
  compiler/rirscope.cpp \
  compiler/rulechoice.cpp \
  compiler/rule.cpp \
  compiler/ruleir.cpp \
  compiler/ruleirlexer.lpp \
  compiler/scheduler.cpp \
  compiler/syntheticrule.cpp \
  compiler/trainingdeps.cpp \
  compiler/transform.cpp \
  compiler/userrule.cpp \
  compiler/heuristic.cpp \
  compiler/heuristicmanager.cpp \
  compiler/dbmanager.cpp \
  compiler/tinyxml.cpp \
  compiler/tinyxmlerror.cpp \
  compiler/tinyxmlparser.cpp \
  compiler/tinystr.cpp \
  libpbcompiler_a-maximalexer.cpp \
  libpbcompiler_a-maximaparser.cpp \
  libpbcompiler_a-pblexer.cpp \
  libpbcompiler_a-pbparser.cpp \
  libpbcompiler_a-ruleirlexer.cpp \
  libpbcompiler_a-ruleirparser.cpp
# compiler/ruleirparser.ypp
# compiler/pblexer.lpp
# compiler/pbparser.ypp
# compiler/maximaparser.ypp

libpbruntime_a_LFLAGS =
libpbruntime_a_CXXFLAGS = -I$(srcdir)/runtime
libpbruntime_a_SOURCES =  \
  runtime/cellproxy.cpp \
  runtime/distributedgc.cpp \
  runtime/dynamicscheduler.cpp \
  runtime/dynamictask.cpp \
  runtime/gpudynamictask.cpp \
  runtime/gpumanager.cpp \
  runtime/gputaskinfo.cpp \
  runtime/matrixio.cpp \
  runtime/matrixstorage.cpp \
  runtime/memoization.cpp \
  runtime/petabricksruntime.cpp \
  runtime/regiondatai.cpp \
  runtime/regiondataraw.cpp \
  runtime/regiondataremotecache.cpp \
  runtime/regiondataremote.cpp \
  runtime/regiondatasplit.cpp \
  runtime/regionhandler.cpp \
  runtime/regionmatrixi.cpp \
  runtime/regionmatrixproxy.cpp \
  runtime/remotehost.cpp \
  runtime/remoteobject.cpp \
  runtime/remotetask.cpp \
  runtime/ruleinstance.cpp \
  runtime/specializeddynamictasks.cpp \
  runtime/subregioncachemanager.cpp \
  runtime/testisolation.cpp \
  runtime/transforminstance.cpp \
  runtime/workerthread.cpp \
  libpbruntime_a-matrixreader.cpp
#  runtime/matrixreader.lpp

libpbmain_a_CXXFLAGS = -Iruntime
libpbmain_a_SOURCES = runtime/redirectmain.cpp

pbc_CXXFLAGS = -Icompiler
pbc_SOURCES = compiler/pbc.cpp
pbc_LDADD = libpbcompiler.a libpbcommon.a

rttest1_CXXFLAGS = -Iruntime
rttest1_SOURCES  = runtime/rttest1.cpp
rttest1_LDADD    = libpbruntime.a libpbcommon.a

rttest2_CXXFLAGS = -Iruntime
rttest2_SOURCES  = runtime/rttest2.cpp
rttest2_LDADD    = libpbruntime.a libpbcommon.a

rttestmm_CXXFLAGS = -Iruntime
rttestmm_SOURCES  = runtime/tests/rttestmm.cpp
rttestmm_LDADD    = libpbruntime.a libpbcommon.a

regionmatrixtest_CXXFLAGS = -Iruntime
regionmatrixtest_SOURCES  = runtime/tests/regionmatrixtest.cpp
regionmatrixtest_LDADD    = libpbruntime.a libpbcommon.a

migrationtest_CXXFLAGS = -Iruntime
migrationtest_SOURCES  = runtime/tests/migrationtest.cpp
migrationtest_LDADD    = libpbruntime.a libpbcommon.a


CLEANFILES = libpbcompiler_a-maximalexer.cpp libpbcompiler_a-maximaparser.cpp libpbcompiler_a-maximaparser.h \
             libpbcompiler_a-pblexer.cpp libpbcompiler_a-pbparser.cpp libpbcompiler_a-pbparser.h \
             libpbcompiler_a-ruleirlexer.cpp libpbcompiler_a-ruleirparser.cpp libpbcompiler_a-ruleirparser.h \
             libpbcompiler_a-maximaparser.output libpbcompiler_a-pbparser.output libpbcompiler_a-ruleirparser.output \
             libpbruntime_a-matrixreader.cpp

#copy bison headers
# maximaparser.h: libpbcompiler_a-maximaparser.h
# 	test -h $@ || $(LN_S) $< $@
# pbparser.h:		 libpbcompiler_a-pbparser.h
# 	test -h $@ || $(LN_S) $< $@
# ruleirparser.h: libpbcompiler_a-ruleirparser.h
# 	test -h $@ || $(LN_S) $< $@

#for new versions of bison
#libpbcompiler_a-maximaparser.h: libpbcompiler_a-maximaparser.hpp
#	test -h $@ || $(LN_S) $< $@
#libpbcompiler_a-pbparser.h: libpbcompiler_a-pbparser.hpp
#	test -h $@ || $(LN_S) $< $@
#libpbcompiler_a-ruleirparser.h: libpbcompiler_a-ruleirparser.hpp
#	test -h $@ || $(LN_S) $< $@

#make sure the output of flex/bison has the latest version of the header
# $(srcdir)/compiler/maximalexer.lpp: maximaparser.h
# $(srcdir)/compiler/pblexer.lpp:     pbparser.h
# $(srcdir)/compiler/ruleirlexer.lpp: ruleirparser.h

