#!/usr/bin/python
import os
import pbutil
import progress
import math 
import sys
import scipy
import time

TRAILS=5
SHORTBAR='-'*40
LONGBAR='='*60

def geomean(nums):
  return (reduce(lambda x, y: x*y, nums))**(1.0/len(nums))

def perfScore(perf, baseline):
  return 100.0*baseline/perf['average']

def fmtPerf(perf, baseline):
  baseline=100.0*baseline
  mean      = baseline/perf['average']
  #meanprime = baseline/(perf['average']+perf['stddev']/math.sqrt(TRAILS))
  meanprime = baseline/(perf['average']+perf['stddev'])
  std  = mean-meanprime
  return "%7.1f +- %4.1f" % (mean, std)


def fmtAcc(acc, target):
  diff = acc['average'] - target
  if diff == 0:
    return ""
  if diff < 0:
    s = " ERROR ACCURACY TOO LOW"
  else:
    s = ""
  if target != 0:
    diffStr = "%.2f%%"%(1.0+diff/abs(target))
  else:
    diffStr = "%.2f"%diff
  return "acc: "+diffStr+s

expandCfg = lambda x: './testdata/configs/'+x
fmtCfg = lambda x: x.replace('.cfg','').ljust(10)

class Benchmark:
  def __init__(self, benchmark, cfg, n, acc_target, baseline):
    self.benchmark  = benchmark
    self.cfg        = cfg
    self.n          = n
    self.acc_target = float(acc_target)
    self.baseline   = float(baseline)
    self.fixed_perf = None
    self.fixed_acc  = None
    self.tuned_perf = None
    self.tuned_acc  = None
    assert os.path.isfile(expandCfg(cfg))

  def scoreFixed(self):
    return perfScore(self.fixed_perf, self.baseline)

  def runFixed(self):
    self.fixed_perf, self.fixed_acc = \
        pbutil.executeTimingRun(pbutil.benchmarkToBin(self.benchmark),
                                int(self.n),
                                ['--trials=%d'%TRAILS,
                                 '--config='+expandCfg(self.cfg),
                                 '--accuracy'],
                                None,
                                ['timing', 'accuracy'])

  def printFixed(self):
    print fmtCfg(self.cfg), \
          fmtPerf(self.fixed_perf, self.baseline), \
          fmtAcc(self.fixed_acc, self.acc_target)


def main():
  pbutil.chdirToPetabricksRoot()

  r, lines = pbutil.loadAndCompileBenchmarks("./scripts/pbbenchmark.tests")

  if filter(lambda x: x.rv!=0, r):
    print "compile failed"
    sys.exit(1)

  print 
  print "All scores are relative performance to a baseline system."
  print "Higher is better"
  print
  print LONGBAR
  print "Fixed (no autotuning) scores:"
  print SHORTBAR

  benchmarks=[]

  for benchmark, cfg, n, accTarg, baseline in lines:
    benchmarks.append(Benchmark(benchmark, cfg, n, accTarg, baseline))

  for b in benchmarks:
    b.runFixed()
    b.printFixed()

  print SHORTBAR
  print "Fixed Score (pbbenchmark v1.0): %.2f" % geomean(map(Benchmark.scoreFixed, benchmarks))
  print LONGBAR
  print

if __name__ == "__main__":
  main()


