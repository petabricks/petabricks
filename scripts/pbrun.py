#!/usr/bin/python

import pbutil
import os
import sys

relpath=None
try:
  relpath=os.path.relpath #only in python 2.6
except:
  relpath=lambda p: p[len(os.path.commonprefix([p,os.getcwd()])):]

if len(sys.argv)<2:
  print "USAGE: ",sys.argv[0], "BENCHMARK args"
  print "Compiles and runs the given benchmark"
  sys.exit(-1);

benchmark=os.path.abspath(sys.argv[1])



pbutil.chdirToPetabricksRoot();
pbutil.compilePetabricks();

benchmark=pbutil.normalizeBenchmarkName(relpath(benchmark))

pbutil.compileBenchmarks([benchmark])

cmd=[pbutil.benchmarkToBin(benchmark)]
cmd.extend(sys.argv[2:])
os.execv(cmd[0], cmd)

