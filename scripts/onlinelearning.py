#!/usr/bin/python
import logging, time, sys, itertools
import pbutil
import sgatuner
import tunerconfig
import tunerwarnings
import warnings 
import mutators 
import candidatetester 
import math
import numpy
import storagedirs
from tunerconfig import config, option_callback
from candidatetester import Candidate, CandidateTester
from traininginfo import TrainingInfo
from configtool import defaultConfigFile
from storagedirs import timers
from sgatuner import Population
from mutators import MutateFailed

def weightedChoice(choices, wfn):
  weights = map(wfn, choices)
  low = min(weights)
  if low<0:
    warnings.warn("saw negative weights in weightedChoice")
    weights = map(lambda x: x-low, weights)
  total = sum(weights)
  r = numpy.random.uniform(0.0, total)
  for c,w in zip(choices, weights):
    r -= w
    if r<=0:
      return c
  assert False

pctrange  = lambda n: map(lambda x: x/float(n-1), xrange(n))
gettime   = lambda c: c.metrics[config.timing_metric_idx][config.n].mean()
getacc    = lambda c: c.metrics[config.accuracy_metric_idx][config.n].mean()
gettrials = lambda c: math.log(c.numTests(config.n))
lastacc   = lambda c: c.metrics[config.accuracy_metric_idx][config.n].last() 
lasttime   = lambda c: c.metrics[config.timing_metric_idx][config.n].last() 
parentlimit = lambda c: c.metrics[config.timing_metric_idx][config.n].dataDistribution().ppf(0.70)

class OnlinePopulation:
  def __init__(self, seed):
    self.members = [seed]
    self.fns = []
    self.n = config.n
    self.wt = (1.0, 1.0, 1.0)
    for c in pctrange(6):
      for b in pctrange(25):
        self.fns.append(self.linearFitness(1.0-b, b, c))

  def linearFitness(self, a, b, cw):
    if b==0 and cw==0:
      return lambda c: self.wt[0]*a*gettime(c)
    if cw==0:
      return lambda c: self.wt[0]*a*gettime(c) - self.wt[1]*b*getacc(c)
    return lambda c: self.wt[0]*a*gettime(c) - self.wt[1]*b*getacc(c) - self.wt[2]*cw*gettrials(c)

  def add(self,m):
    self.members.append(m)

  def prune(self):
    for m in self.members:
      m.keep = False
    for fn in self.fns:
      m = min(self.members, key=fn)
      m.keep = True
    self.members = filter(lambda x: x.keep, self.members)
    self.members.sort(key=self.linearFitness(1,0,0))

  def select(self, fn):
    return min(self.members, key=fn)

  def choice(self, timelimit, fn):
    return weightedChoice(filter(lambda x: gettime(x)<=timelimit, self.members), fn)

  def output(self, active=[]):
    for m in self.members:
      if m in active:
        print '  ***', m.resultsStr(self.n)
      else:
        print '   - ', m.resultsStr(self.n)

  def reweight(self):
    s = (sum(map(gettime, self.members)), sum(map(getacc, self.members)), sum(map(gettrials, self.members)))
    t = sum(s)
    self.wt = map(lambda x: t/x, s)
    print "Weights = ", self.wt

def resultingTimeAcc(p, c):
  if not c.wasTimeout:
    if not p.wasTimeout:
      t = max(lasttime(p), lasttime(c))
      a = max(lastacc(p), lastacc(c))
    else:
      t = lasttime(c)
      a = lastacc(c)
  else:
    t = lasttime(p)
    a = lastacc(p)
  return t,a


def onlinelearnInner(benchmark):
  if config.debug:
    logging.basicConfig(level=logging.DEBUG)

  n = config.n
  W = config.window_size

  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  main = sgatuner.mainname([pbutil.benchmarkToBin(benchmark)])
  tester = CandidateTester(benchmark, n)
  candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml.transform(main))
  sgatuner.addMutators(candidate, infoxml.globalsec())
  sgatuner.addMutators(candidate, infoxml.transform(main))
  candidate.addMutator(mutators.MultiMutator(2))
  pop = OnlinePopulation(candidate)
  result = candidatetester.Results()
  elapsed = 0.0
  
  def fitness(candidate):
    if lastacc(candidate) is None:
      return None
    t=gettime(candidate)
    a=getacc(candidate)
    if config.accuracy_target is not None and config.accuracy_target > a:
      return t + 100.0*(config.accuracy_target-a)
    return t

  pop.fns.append(fitness)

  if not config.delete_output_dir:
    storagedirs.cur.dumpConfig()
    storagedirs.cur.dumpGitStatus()
    storagedirs.cur.saveFile(pbutil.benchmarkToInfo(benchmark))
    storagedirs.cur.saveFile(pbutil.benchmarkToBin(benchmark))

  ''' mutators in the last time window that produced improved candidates, 
  ordered by descending fitness of the candidates'''
  mutatorLog = []

  ''' actual size of the candidate window, in case we have fewer candidates
  than the maximum window size'''
  actual_w = 0 
    
  try:
    timers.total.start()
    config.end_time = time.time() + config.max_time
        
    for gen in itertools.count():

      if gen%config.reweight_interval==0 and gen>0:
        pop.reweight()
      p = pop.select(fitness)

      if p.numTests(n)>0:
        c = pop.choice(parentlimit(p), getacc)
      else:
        c = p
      c = p.cloneAndMutate(n, True, mutatorLog)
      if tester.race(p, c):
        # slide the candidate window
        if actual_w >= W:
          mutatorLog.pop(actual_w - 1);
        else:
          actual_w += 1


        # log the mutation
        mutatorLog = [c.lastMutator] + mutatorLog

        if gen==0 and p.wasTimeout:
          pop.members=[c]
        if not c.wasTimeout:
          pop.add(c)
          pop.prune()
        t,a = resultingTimeAcc(p, c)
        if a is not None and t is not None:
          result.add(a)
          elapsed += t
        print "Generation",gen,"elapsed",elapsed,"time", t,"accuracy",a, "limit", parentlimit(p)
        pop.output((p,c))
      else:
        print 'error'

    timers.total.stop()
  finally:
    at = storagedirs.getactivetimers()
    if len(at):
      storagedirs.openCsvStats("timers", at.keys()).writerow(at.values())
    tester.cleanup()

def onlinelearn(benchmark):
  storagedirs.callWithLogDir(lambda: onlinelearnInner(benchmark),
                             config.output_dir,
                             config.delete_output_dir)

if __name__ == "__main__":
  tunerconfig.applypatch(tunerconfig.patch_onlinelearning)
  from optparse import OptionParser
  parser = OptionParser(usage="usage: onlinelearning.py [options] Benchmark -n N")
  parser.add_option("--check",
                    action="store_true", dest="check", default=False,
                    help="check for correctness")
  parser.add_option("--debug",
                    action="store_true", dest="debug", default=False,
                    help="enable debugging options")
  parser.add_option("-n", type="int", help="input size to train for")
  parser.add_option("--max_time",              type="float",  action="callback", callback=option_callback)
  parser.add_option("--rounds_per_input_size", type="int",    action="callback", callback=option_callback)
  parser.add_option("--mutations_per_mutator", type="int",    action="callback", callback=option_callback)
  parser.add_option("--output_dir",            type="string", action="callback", callback=option_callback)
  parser.add_option("--population_high_size",  type="int",    action="callback", callback=option_callback)
  parser.add_option("--population_low_size",   type="int",    action="callback", callback=option_callback)
  parser.add_option("--offset",                type="int",    action="callback", callback=option_callback)
  parser.add_option("--name",                  type="string", action="callback", callback=option_callback)
  parser.add_option("--accuracy_target",       type="float",  action="callback", callback=option_callback)
  (options, args) = parser.parse_args()
  if len(args)!=1 or not options.n:
    parser.print_usage()
    sys.exit(1)
  if options.debug:
    tunerconfig.applypatch(tunerconfig.patch_debug)
  if options.n:
    tunerconfig.applypatch(tunerconfig.patch_n(options.n))
  config.benchmark=args[0]
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  config.benchmark=pbutil.normalizeBenchmarkName(config.benchmark)
  pbutil.compileBenchmarks([config.benchmark])
  onlinelearn(config.benchmark)

