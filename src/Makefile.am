INCLUDES = 
METASOURCES = AUTO
AM_YFLAGS = -d
AM_LFLAGS = 

noinst_LIBRARIES = libhecura.a
noinst_PROGRAMS = pbc 

noinst_HEADERS = choicegrid.h codegenerator.h config.h dynamicscheduler.h \
	dynamictask.h formula.h hecura.h hecuraruntime.h jasm.h jassert.h jblockingqueue.h \
	jbuffer.h jconvert.h jfilesystem.h jmutex.h jprintable.h jrefcounted.h \
	jserialize.h jsocket.h jthread.h jtimer.h jtunable.h learner.h matrix.h matrixdef.h \
	matrixdependency.h matrixio.h matrixoperations.h maximawrapper.h performancetester.h \
	region.h rule.h rulechoice.h spatialdynamictask.h staticscheduler.h transform.h

libhecura_a_SOURCES = choicegrid.cpp codegenerator.cpp dynamicscheduler.cpp \
	dynamictask.cpp formula.cpp hecuraruntime.cpp jassert.cpp jbuffer.cpp jfilesystem.cpp \
	jprintable.cpp jserialize.cpp jsocket.cpp jthread.cpp jtimer.cpp jtunable.cpp \
	learner.cpp matrix.cpp matrixdef.cpp matrixdependency.cpp matrixio.cpp \
	matrixreader.lpp maximalexer.lpp maximaparser.ypp maximawrapper.cpp pblexer.lpp \
	pbparser.ypp performancetester.cpp region.cpp rule.cpp rulechoice.cpp \
	spatialdynamictask.cpp staticscheduler.cpp transform.cpp

pbc_SOURCES = pbc.cpp
pbc_LDADD = libhecura.a -lpthread 

#force bison before flex
pbparser.h: pbparser.cpp
pblexer.cpp pblexer.lpp: pbparser.h
maximaparser.h: maximaparser.cpp
maximalexer.cpp maximalexer.lpp: maximaparser.h

