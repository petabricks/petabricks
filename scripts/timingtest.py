#!/usr/bin/python

import pbutil
import time
import os
import re 
import socket

benchmarks=pbutil.loadAndCompileBenchmarks("./scripts/timingtest.tests")

cleanupPathRe = re.compile("[ /.]")
cleanupPath=lambda n: cleanupPathRe.sub('_',n)

timingdir = "./testdata/.timing/"+socket.gethostname()
if not os.path.isdir(timingdir):
  print "creating "+timingdir
  os.makedirs(timingdir)

def runSingleTest(name, n):
  pfx=timingdir+"/%s_%d"%(cleanupPath(name),n)
  cfg = pfx+".cfg"
  dat = pfx+".dat"
  args=["--trials=3","--config="+cfg]
  print name, pfx
  print args
  rslt=pbutil.executeTimingRun(pbutil.benchmarkToBin(name), n, args)
  print name, n, rslt

print "Running timing tests..."
for name,n in benchmarks:
  runSingleTest(name,int(n))







