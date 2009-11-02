#!/usr/bin/python

import pbutil
import os
import sys

relpath=None
benchmark=None
gdb = False

if "--gdb" in sys.argv:
  gdb=True
  sys.argv=filter(lambda x: x!="--gdb", sys.argv)

try:
  relpath=os.path.relpath #only in python 2.6
except:
  relpath=lambda p: p[len(os.path.commonprefix([p,os.getcwd()])):]
  
if len(sys.argv)>1:  
  if sys.argv[1] == "--help":
    print "**** USAGE: ",sys.argv[0], "[BENCHMARK] [args]"
    print "****    1) Compile pbc"
    print "****    2) If BENCHMARK is given, compile BENCHMARK"
    print "****    3) If args are given, run BENCHMARK"
    sys.exit(1)
  else:
    benchmark=sys.argv[1]
    if os.path.isfile(benchmark) or os.path.isfile(benchmark+".pbcc"):
      benchmark=os.path.abspath(benchmark)

pbutil.chdirToPetabricksRoot();
pbutil.compilePetabricks();

if benchmark is not None:
  benchmark=pbutil.normalizeBenchmarkName(relpath(benchmark))

cmd=[]
if gdb:
  cmd.extend(["/usr/bin/gdb", "--args"])

if len(sys.argv)==2:
  cmd.extend(["./src/pbc", pbutil.benchmarkToSrc(benchmark)])
  print " ".join(cmd)
  os.execv(cmd[0], cmd)
elif len(sys.argv)>2:
  pbutil.compileBenchmarks([benchmark])
  cmd.extend([pbutil.benchmarkToBin(benchmark)])
  cmd.extend(sys.argv[2:])
  print " ".join(cmd)
  os.execv(cmd[0], cmd)


