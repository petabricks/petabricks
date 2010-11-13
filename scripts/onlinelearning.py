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
gettrials = lambda c: math.sqrt(c.numTests(config.n))
lastacc   = lambda c: c.metrics[config.accuracy_metric_idx][config.n].last() 
lasttime   = lambda c: c.metrics[config.timing_metric_idx][config.n].last() 
parentlimit = lambda c: c.metrics[config.timing_metric_idx][config.n].dataDistribution().ppf(0.70)


lastMutatorId = 0
class MutatorLogEntry:
  def __init__(self, mutator, candidate, time):
    global lastMutatorId

    self.mutator = mutator
    self.candidate = candidate
    self.time = time
    self.id = lastMutatorId + 1
    lastMutatorId = self.id
    


  def __repr__(self):
    return "%s (%f s) [id = %i]" % (str(self.mutator), self.time, self.id)

def sortedMutatorLog(log):
  return sorted(log, key = lambda m: m.time)

class OnlinePopulation:
  def __init__(self):
    self.members = []
    self.fns = []
    self.n = config.n
    self.wt = (1.0, 1.0, 1.0)
    for c in (0.0, 0.3, 0.6):
      for b in pctrange(25):
        self.fns.append(self.linearFitness(1.0-b, b, c))
    if config.accuracy_target is not None:
      for t in (config.accuracy_target*0.90,
                config.accuracy_target*0.95,
                config.accuracy_target*1.00,
                config.accuracy_target*1.05,
                config.accuracy_target*1.10):
        self.fns.append(self.thresholdAccuracyFitness(t))
    if config.timing_target is not None:
      for t in (config.timing_target*0.90,
                config.timing_target*0.95,
                config.timing_target*1.00,
                config.timing_target*1.05,
                config.timing_target*1.10):
        self.fns.append(self.thresholdTimingFitness(t))


  def linearFitness(self, a, b, cw):
    if b==0 and cw==0:
      return lambda c: self.wt[0]*a*gettime(c)
    if cw==0:
      return lambda c: self.wt[0]*a*gettime(c) - self.wt[1]*b*getacc(c)
    return lambda c: self.wt[0]*a*gettime(c) - self.wt[1]*b*getacc(c) - self.wt[2]*cw*gettrials(c)
  
  def thresholdAccuracyFitness(self, target, mult=config.threshold_multiplier_default):
    def fitness(c):
      t=gettime(c)
      a=getacc(c)
      if a<target:
        a = (target-a)*mult
      else:
        a = 0
      return self.wt[0]*t + self.wt[1]*a
    return fitness
  
  def thresholdTimeFitness(self, target, mult=config.threshold_multiplier_default):
    def fitness(c):
      t=gettime(c)
      a=getacc(c)
      if t>target:
        t = (t-target)*mult
      else:
        t = 0
      return self.wt[0]*t - self.wt[1]*a
    return fitness

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
    if len(self.members)<=1:
      return self.members[0]
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


class ObjectiveTuner:
  def __init__(self, pop):
    self.pop            = pop
    self.wiggleroom     = 0.20
    self.window         = config.max_trials
    self.timing         = candidatetester.Results()
    self.timingRecent   = candidatetester.Results()
    self.accuracy       = candidatetester.Results()
    self.accuracyRecent = candidatetester.Results()
    self.elapsed        = 0
    self.computeFitnessFunction()

  def result(self, t, a):
    self.elapsed += t
    self.timing.add(t)
    self.timingRecent.add(t)
    self.timingRecent.discard(self.window)
    self.accuracy.add(a)
    self.accuracyRecent.add(a)
    self.accuracyRecent.discard(self.window)
    self.computeFitnessFunction()

  def score(self):
    if len(self.timing):
      if config.accuracy_target is not None:
        return self.accuracyRecent.dataDistribution().ppf(0.20)/config.accuracy_target
      elif config.timing_target is not None:
        return 1.0 - self.timingRecent.dataDistribution().ppf(0.20)/config.timing_target
    return 1.0

  def computeFitnessFunction(self):
    score = self.score()
    mult = config.threshold_multiplier_default
    while score<.9:
      mult*=2
      score+=.1
    while score>1.1:
      mult/=2
      score-=.1
    mult = min(config.threshold_multiplier_max, 
           max(config.threshold_multiplier_min, mult))

    if config.accuracy_target is not None:
      self.fitness = self.pop.thresholdAccuracyFitness(config.accuracy_target, mult)
    elif config.timing_target is not None:
      self.fitness = self.pop.thresholdTimingFitness(config.timing_target, mult)
    else:
      self.fitness = self.pop.linearFitness(1,0,0)

  def getlimits(self, safe, seed, experiment):
    return None, config.accuracy_target

  def __str__(self):
    return str(self.score())

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
  pop = OnlinePopulation()
  objectives = ObjectiveTuner(pop)
  
  if not config.delete_output_dir:
    storagedirs.cur.dumpConfig()
    storagedirs.cur.dumpGitStatus()
    storagedirs.cur.saveFile(pbutil.benchmarkToInfo(benchmark))
    storagedirs.cur.saveFile(pbutil.benchmarkToBin(benchmark))

  ''' mutators in the last time window that produced improved candidates, 
  ordered by descending fitness of the candidates'''
  mutatorLog = []

  ostats = storagedirs.openCsvStats("onlinestats", ['gen',
                                                    'elapsed',
                                                    'timing',
                                                    'accuracy',
                                                    'objective_score'])
    
  try:
    timers.total.start()
    config.end_time = time.time() + config.max_time

    '''seed first round'''
    p = candidate
    c = p.cloneAndMutate(n, config.use_bandit, mutatorLog)
    if not tester.race(p, c):
      raise Exception()
    if not p.wasTimeout:
      pop.add(p)
    if not c.wasTimeout:
      pop.add(c)

    '''now normal rounds'''  
    for gen in itertools.count(1):
      if time.time() > config.end_time:
        break
      if gen%config.reweight_interval==0:
        pop.reweight()

      p = pop.select(objectives.fitness)
      #s = pop.choice(parentlimit(p), getacc)
      s = p
      c = s.cloneAndMutate(n, config.use_bandit, mutatorLog)
      tlim, atarg = objectives.getlimits(p, s, c)
      if tester.race(p, c, tlim, atarg):
        p.discardResults(config.max_trials)
        if not c.wasTimeout:
          pop.add(c)
          pop.prune()

        # slide the candidate window
        if len(mutatorLog) >= W:
          mutatorLog.sort(key=lambda x: x.id)
          mutatorLog.pop(0);

        mutatorLog = sortedMutatorLog([MutatorLogEntry(c.lastMutator, c, gettime(c))] + mutatorLog)
        
        if config.bandit_verbose:
          if gettime(c) < gettime(p): # candidate better than parent
            print "Child better than parent: %f vs. %f" % (gettime(c), gettime(p))
          else:
            print "Child equal/worse than parent: %f vs. %f" % (gettime(c), gettime(p))

        t,a = resultingTimeAcc(p, c)
        print "Generation", gen, "elapsed",objectives.elapsed,"time", t,"accuracy",a
        print "Objectives", objectives
        ostats.writerow([gen, objectives.elapsed, t, a, objectives.score()])
        if a is not None and t is not None:
          objectives.result(t,a)
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

