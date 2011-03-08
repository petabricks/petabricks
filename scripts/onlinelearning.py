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
import configtool
import random
from tunerconfig import config, option_callback
from candidatetester import Candidate, CandidateTester, MutatorLogFile
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

pctrange    = lambda n: map(lambda x: x/float(n-1), xrange(n))
gettime     = lambda c: c.metrics[config.timing_metric_idx][config.n].mean()
getacc      = lambda c: c.metrics[config.accuracy_metric_idx][config.n].mean()
lastacc     = lambda c: c.metrics[config.accuracy_metric_idx][config.n].last() 
lasttime    = lambda c: c.metrics[config.timing_metric_idx][config.n].last() 
parentlimit = lambda c: c.metrics[config.timing_metric_idx][config.n].dataDistribution().ppf(0.70)
def getconf(c):
  if c.numTests(config.n) > 1:
    return 1+c.metrics[config.timing_metric_idx][config.n].invstderr()+\
             c.metrics[config.accuracy_metric_idx][config.n].invstderr()
  else:
    return 1


lastMutatorId = 0
class MutatorLogEntry:
  def __init__(self, mutator, candidate, dtime, daccuracy, time, acc):
    global lastMutatorId

    self.mutator = mutator
    self.candidate = candidate
    self.dtime = dtime
    self.daccuracy = daccuracy
    self.time = time
    self.accuracy = acc
    self.id = lastMutatorId + 1
    lastMutatorId = self.id
 
  def __repr__(self):
    return "%s (%f s, %s acc) [id = %i]" % (str(self.mutator),
                                            self.dtime,
                                            "n/a" if self.daccuracy == None else str(self.daccuracy),
                                            self.id)


class MutatorLog:
  def __init__(self, name):
    self.log = []
    self.name = name

  def add(self, c, dtime, dacc, time, acc):
    # slide the candidate window
    if len(self.log) >= config.window_size:
      self.log.sort(key=lambda x: x.id)
      self.log.pop(0);

    self.log = [MutatorLogEntry(c.lastMutator, c, dtime, dacc, time, acc)] + self.log


  '''biggest speedups first'''
  def getSortedByTime(self):
    sortedLog = MutatorLog(self.name)
    sortedLog.log = sorted(self.log, key=lambda entry: entry.time)
    return sortedLog

  '''biggest jumps in accuracy first'''
  def getSortedByAcc(self):
    sortedLog = MutatorLog(self.name)
    sortedLog.log = sorted(self.log, key=lambda entry: -entry.accuracy if entry.accuracy != None else 100000)
    return sortedLog


  '''biggest speedups first'''
  def getSortedByDeltaTime(self):
    sortedLog = MutatorLog(self.name)
    sortedLog.log = sorted(self.log, key=lambda entry: entry.dtime)
    return sortedLog

  '''biggest jumps in accuracy first'''
  def getSortedByDeltaAcc(self):
    sortedLog = MutatorLog(self.name)
    sortedLog.log = sorted(self.log, key=lambda entry: -entry.daccuracy if entry.daccuracy != None else 100000)
    return sortedLog

  def __repr__(self):
    return str(self.log)

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
    return lambda c: self.wt[0]*a*gettime(c) - self.wt[1]*b*getacc(c) - self.wt[2]*cw*getconf(c)
  
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
  
  def thresholdTimingFitness(self, target, mult=config.threshold_multiplier_default):
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
    s = (abs(sum(map(gettime, self.members))),
         abs(sum(map(getacc, self.members))),
         abs(sum(map(getconf, self.members))))
    t = sum(s)
    self.wt = map(lambda x: t/x, s)
    logging.debug("weights = "+str(self.wt))

  statsHeader = ['gen', 'pop_size', 'weight_time', 'weight_acc', 'weight_conf']
  def stats(self, gen):
    return [gen,len(self.members)]+list(self.wt)

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
        v=self.accuracyRecent.dataDistribution().ppf(0.5)/config.accuracy_target
        if config.accuracy_target<0:
          v = 1.0/v
        return v
      elif config.timing_target is not None:
        return config.timing_target/self.timingRecent.dataDistribution().ppf(0.5)
    return 1.0

  def computeFitnessFunction(self):
    score = self.score()
    mult = config.threshold_multiplier_default
    if score<=0:
      mult=config.threshold_multiplier_max
    else:
      while score<.9:
        mult*=2
        score+=.1
    if score>2:
      mult=config.threshold_multiplier_min
    else:
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
    if config.timing_target is not None:
      return max(config.timing_target, 1.1*gettime(safe)), getacc(safe)*1.1
    else:
      return None, config.accuracy_target


  def needAccuracy(self):
    if config.accuracy_target is not None:
      return self.score()<.95
    if config.timing_target is not None:
      return self.score()>.95
    return False

  statsHeader = ['gen', 'elapsed', 'score',
                 'timing_last',    'accuracy_last',
                 'timing_rolling', 'accuracy_rolling',
                 'timing_total',   'accuracy_total']
  def stats(self, gen):
    return [gen, self.elapsed, self.score(),
            self.timingRecent.last(), self.accuracyRecent.last(),
            self.timingRecent.mean(), self.accuracyRecent.mean(),
            self.timing.mean(),       self.accuracy.mean()]
            

  def __str__(self):
    return str(self.score())

def createChoiceSiteMutatorsOnline(candidate, info, ac, weight):
  transform = info.name()
  number = ac['number']
  return [mutators.ShuffleAlgsChoiceSiteMutator(transform, number, weight=weight),
          mutators.ShuffleCutoffsChoiceSiteMutator(transform, number, weight=weight),
          mutators.ShuffleTopChoiceSiteMutator(transform, number, weight=weight),
          mutators.ShuffleBotChoiceSiteMutator(transform, number, weight=weight),
          mutators.AddLevelChoiceSiteMutator(transform, number, weight=weight),
          mutators.RemoveLevelChoiceSiteMutator(transform, number, weight=weight)]

def onlinelearnInner(benchmark):
  candidate, tester = sgatuner.init(benchmark, createChoiceSiteMutatorsOnline)
  pop = OnlinePopulation()
  objectives = ObjectiveTuner(pop)

  ''' mutators in the last time window that produced improved candidates, 
  ordered by descending fitness of the candidates'''
  mutatorLog = MutatorLog(name = "acc and time log")

  ostats = storagedirs.openCsvStats("onlinestats", ObjectiveTuner.statsHeader)
  pstats = storagedirs.openCsvStats("population", OnlinePopulation.statsHeader)
  clog = storagedirs.openCsvStats("onlinecandidates", ['gen',
                                                       'timesafe','accsafe','timeexp','accexp',
                                                       'safe','seed','experimental',
                                                       ])

    
  try:
    timers.total.start()

    '''seed first round'''
    p = candidate
    if config.online_baseline:
      c = None
    else:
      c = p.clone()
    if not tester.race(p, c):
      raise Exception()
    if not p.wasTimeout:
      pop.add(p)
    if c and not c.wasTimeout:
      pop.add(c)

    if not config.online_baseline:
      mlog = MutatorLogFile(c.mutators)

    '''now normal rounds'''  
    for gen in itertools.count(1):
      if config.max_time and objectives.elapsed>config.max_time:
        break
      if config.max_gen and gen>config.max_gen:
        break
      if gen%config.reweight_interval==0:
        pop.reweight()

      p = pop.select(objectives.fitness)
      #s = pop.choice(parentlimit(p), getacc)
      s = p

      if config.fixed_safe_alg:
        p = candidate

      if config.online_baseline:
        c = None
      else:
        if(objectives.needAccuracy()):
          mfilter = lambda x: x.accuracyHint
        else:
          mfilter = lambda x: True
        
        c = s.cloneAndMutate(tester.n,
                             adaptive = True,
                             mutatorLog = mutatorLog,
                             objectives = objectives,
                             mutatorFilter = mfilter)
      tlim, atarg = objectives.getlimits(p, s, c)
      if tester.race(p, c, tlim, atarg) and not (p.wasTimeout and c.wasTimeout):
        p.discardResults(config.max_trials)
        if c and not c.wasTimeout:
          pop.add(c)
          pop.prune()

        if c is None:
          c=p
        
        logging.debug("Child vs parent, better=%d, %f vs. %f" % (int(gettime(c) < gettime(p)), gettime(c), gettime(p)))
        clog.writerow([gen, lasttime(p), lastacc(p), lasttime(c), lastacc(c)]
                      +map(storagedirs.relpath,[p.cfgfile(), s.cfgfile(), c.cfgfile()]))

        dtime = gettime(c) - gettime(p)
        dacc = None if c.wasTimeout else (getacc(c) - getacc(p))

        if c is not None:          
          mutatorLog.add(c, dtime, dacc, gettime(c), None if c.wasTimeout else getacc(c));


        if not config.online_baseline:
          mlog.logPerformance(gen, gettime(c), "None" if c.wasTimeout else getacc(c), dtime, dacc, str(c.lastMutator));
          mlog.logScores(gen, c.mutatorScores)

        t,a = resultingTimeAcc(p, c)
        print "Generation", gen, "elapsed",objectives.elapsed,"time", t,"accuracy",a, getconf(p)
        print "Objectives", objectives
        if a is not None and t is not None:
          objectives.result(t,a)
        pop.output((p,c,s))
        ostats.writerow(objectives.stats(gen))
        pstats.writerow(pop.stats(gen))
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
  parser.add_option("--debug",
                    action="store_true", dest="debug", default=False,
                    help="enable debugging options")
  parser.add_option("-n", type="int", help="input size to train for")
  parser.add_option("--max_time",        type="float",  action="callback", callback=option_callback)
  parser.add_option("--max_gen",         type="int",  action="callback", callback=option_callback)
  parser.add_option("--output_dir",      type="string", action="callback", callback=option_callback)
  parser.add_option("--seed",            type="string", action="callback", callback=option_callback)
  parser.add_option("--offset",          type="int",    action="callback", callback=option_callback)
  parser.add_option("--recompile",       type="int",    action="callback", callback=option_callback)
  parser.add_option("--online_baseline", type="int",    action="callback", callback=option_callback)
  parser.add_option("--fixed_safe_alg",  type="int",    action="callback", callback=option_callback)
  parser.add_option("--race_split_ratio",type="float",  action="callback", callback=option_callback)
  parser.add_option("--name",            type="string", action="callback", callback=option_callback)
  parser.add_option("--accuracy_target", type="float",  action="callback", callback=option_callback)
  parser.add_option("--timing_target",   type="float",  action="callback", callback=option_callback)
  parser.add_option("--use_bandit",      type="int",    action="callback", callback=option_callback)
  parser.add_option("--window_size",     type="int",    action="callback", callback=option_callback)
  parser.add_option("--bandit_c",        type="float",  action="callback", callback=option_callback)
  parser.add_option("--os_method",       type="int",    action="callback", callback=option_callback)
  parser.add_option("--threads",         type="int",    action="callback", callback=option_callback)

  (options, args) = parser.parse_args()
  if len(args)!=1 or not options.n:
    parser.print_usage()
    sys.exit(1)
  if options.debug:
    tunerconfig.applypatch(tunerconfig.patch_debug)
  assert not (config.accuracy_target and config.timing_target)
  
  config.min_input_size = options.n
  config.max_input_size = options.n
  config.n              = options.n

  config.benchmark=args[0]
  sgatuner.recompile()
  onlinelearn(config.benchmark)

