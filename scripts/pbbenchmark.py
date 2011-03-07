#!/usr/bin/python
import os
import pbutil
import progress
import math 
import sys
import time

TRAILS=5

pbutil.chdirToPetabricksRoot()

r, lines = pbutil.loadAndCompileBenchmarks("./scripts/pbbenchmark.tests")

if filter(lambda x: x.rv!=0, r):
  print "compile failed"
  sys.exit(1)

def fmtPerf(perf, baseline):
  baseline=100.0*float(baseline)
  mean      = baseline/perf['average']
  #meanprime = baseline/(perf['average']+perf['stddev']/math.sqrt(TRAILS))
  meanprime = baseline/(perf['average']+perf['stddev'])
  std  = mean-meanprime
  return "%7.1f +- %3.1f (%.4f)" % (mean, std, perf['average'])

expandCfg = lambda x: './testdata/configs/'+x
fmtCfg = lambda x: x.replace('.cfg','').ljust(10)

print
print

for benchmark, cfg, n, acc, baseline in lines:
  assert os.path.isfile(expandCfg(cfg))
  perf = pbutil.executeTimingRun(pbutil.benchmarkToBin(benchmark),
                                 int(n),
                                 [ '--trials=%d'%TRAILS,
                                   '--config='+expandCfg(cfg)])
  print fmtCfg(cfg), fmtPerf(perf, baseline)





