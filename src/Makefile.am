INCLUDES =
METASOURCES = AUTO
AM_YFLAGS = 
AM_LFLAGS = 
OBJDIR=obj

noinst_LIBRARIES = libpbcommon.a libpbcompiler.a libpbruntime.a libpbmain.a
noinst_PROGRAMS = pbc rttest1 rttest2

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
  compiler/maximaparser.h \
  compiler/maximawrapper.h \
  compiler/openclutil.h \
  compiler/pbc.h \
  compiler/pbparser.h \
  compiler/region.h \
  compiler/rircompilerpass.h \
  compiler/rirscope.h \
  compiler/rulechoice.h \
  compiler/rule.h \
  compiler/ruleir.h \
  compiler/ruleirparser.h \
  compiler/scheduler.h \
  compiler/syntheticrule.h \
  compiler/trainingdeps.h \
  compiler/transform.h \
  compiler/userrule.h

pkglib_HEADERS=  \
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
  common/srcpos.h \
  common/thedeque.h \
  runtime/dynamicscheduler.h \
  runtime/dynamictask.h \
  runtime/matrixio.h \
  runtime/matrixregion.h \
  runtime/matrixspecializations.h \
  runtime/matrixstorage.h \
  runtime/memoization.h \
  runtime/petabricks.h \
  runtime/petabricksruntime.h \
  runtime/remotehost.h \
  runtime/remoteobject.h \
  runtime/ruleinstance.h \
  runtime/specializeddynamictasks.h \
  runtime/testisolation.h \
  runtime/transforminstance.h \
  runtime/workerthread.h \
  runtime/openclutil.h

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
  common/srcpos.cpp

libpbcompiler_a_LFLAGS = 
libpbcompiler_a_YFLAGS = -d
libpbcompiler_a_CXXFLAGS = -I$(srcdir)/compiler
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
  compiler/maximaparser.ypp \
  compiler/maximawrapper.cpp \
  compiler/openclutil.cpp \
  compiler/pblexer.lpp \
  compiler/pbparser.ypp \
  compiler/region.cpp \
  compiler/rircompilerpass.cpp \
  compiler/rirscope.cpp \
  compiler/rulechoice.cpp \
  compiler/rule.cpp \
  compiler/ruleir.cpp \
  compiler/ruleirlexer.lpp \
  compiler/ruleirparser.ypp \
  compiler/scheduler.cpp \
  compiler/syntheticrule.cpp \
  compiler/trainingdeps.cpp \
  compiler/transform.cpp \
  compiler/userrule.cpp

libpbruntime_a_LFLAGS = 
libpbruntime_a_CXXFLAGS = -I$(srcdir)/runtime
libpbruntime_a_SOURCES =  \
  runtime/dynamicscheduler.cpp \
  runtime/dynamictask.cpp \
  runtime/matrixio.cpp \
  runtime/matrixreader.lpp \
  runtime/matrixstorage.cpp \
  runtime/memoization.cpp \
  runtime/petabricksruntime.cpp \
  runtime/remotehost.cpp \
  runtime/remoteobject.cpp \
  runtime/ruleinstance.cpp \
  runtime/specializeddynamictasks.cpp \
  runtime/testisolation.cpp \
  runtime/transforminstance.cpp \
  runtime/workerthread.cpp \
  runtime/openclutil.cpp

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

CLEANFILES = libpbcompiler_a-maximalexer.cpp libpbcompiler_a-maximaparser.cpp libpbcompiler_a-maximaparser.h \
             libpbcompiler_a-pblexer.cpp libpbcompiler_a-pbparser.cpp libpbcompiler_a-pbparser.h \
             libpbcompiler_a-ruleirlexer.cpp libpbcompiler_a-ruleirparser.cpp libpbcompiler_a-ruleirparser.h \
             libpbcompiler_a-maximaparser.output libpbcompiler_a-pbparser.output libpbcompiler_a-ruleirparser.output \
             libpbruntime_a-matrixreader.cpp

#copy bison headers to compiler dir
$(srcdir)/compiler/maximaparser.h: libpbcompiler_a-maximaparser.h
	cp $< $@ 
$(srcdir)/compiler/pbparser.h:		 libpbcompiler_a-pbparser.h
	cp $< $@ 
$(srcdir)/compiler/ruleirparser.h: libpbcompiler_a-ruleirparser.h
	cp $< $@ 

$(srcdir)/compiler/maximalexer.lpp: $(srcdir)/compiler/maximaparser.h
$(srcdir)/compiler/pblexer.lpp: $(srcdir)/compiler/pbparser.h
$(srcdir)/compiler/ruleirlexer.lpp: $(srcdir)/compiler/ruleirparser.h

