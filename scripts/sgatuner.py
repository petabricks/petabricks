#!/usr/bin/python
import itertools, random, subprocess, os, sys, time
import pbutil, mutators
import logging
import storagedirs 
import candidatetester
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester
from mutators import MutateFailed
from traininginfo import TrainingInfo
from tunerconfig import config

mean = lambda x: sum(x)/float(len(x))

def mainname(cmd):
  cmd.append("--name")
  p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
  cmd.pop()
  os.waitpid(p.pid, 0)
  lines = p.stdout.readlines()
  return lines[-1].strip()

class Population:
  def __init__(self, initial, tester, baseline=None):
    self.members  = [initial]
    self.notadded = []
    self.testers  = [tester]
    self.baseline = baseline
    self.trialsLog = storagedirs.openCsvStats("avgnumtests", ("inputsize", "avg_added", "avg_not_added", "avg_all"))
  
  def test(self, count):
    '''test each member of the pop count times'''
    tests = []
    for z in xrange(count):
      tests.extend(self.members)
    random.shuffle(tests)
    for m in tests:
      self.testers[-1].test(m)
  
  def randomMutation(self, maxpopsize=None):  
    '''grow the population using cloning and random mutation'''
    originalPop = list(self.members)
    triedConfigs = set(map(lambda x: x.config, self.members))
    totalMutators = sum(map(lambda x: len(x.mutators), self.members))
    tries = float(totalMutators)*config.mutations_per_mutator
    while tries>0:
      tries-=1
      if maxpopsize and len(self.members) >= maxpopsize:
        break
      if config.multimutation:
        p=random.choice(self.members)
      else:
        p=random.choice(originalPop)
      c=p.clone()
      for z in xrange(config.mutate_retries):
        try:
          c.mutate(self.inputSize())
          break
        except MutateFailed:
          if z==config.mutate_retries-1:
            logging.debug("mutate failed, try %d of %d" % (z, config.mutate_retries-1))
          continue
      if c.config in triedConfigs:
        continue
      triedConfigs.add(c.config)
      for z in xrange(config.offspring_min_trials):
        self.testers[-1].test(c, limit=p.reasonableLimit(self.inputSize()))
      if self.birthFilter(p,c):
        self.members.append(c)
      else:
        c.rmcfgfile()
        self.notadded.append(c)
    if len(originalPop)<len(self.members):
      logging.debug("added "+', '.join(map(str,set(self.members)-set(originalPop))))
    return tries

  def inputSize(self, roundOffset=0):
    return self.testers[-1 - roundOffset].n

  def markBestN(self, population, n, metric = config.timing_metric_idx):
    '''shrink the population to popsize by removing low scoring candidates'''
    fastCmp = self.testers[-1].comparer(metric, 0.00, 0)
    fullCmp = self.testers[-1].comparer(metric, config.compare_confidence_pct, config.compare_max_trials)
    # a rough partitioning based on fastCmp
    population.sort(cmp=fastCmp)
    membersfast=list(population[0:n])
    membersslow=list(population[n:])
    # fully order membersfast
    membersfast.sort(cmp=fullCmp)
    # check if any of membersslow should make the cut
    cutoffAlg=membersfast[-1]
    membersfast.extend(filter(lambda x: fullCmp(cutoffAlg,x)>0, membersslow))
    # fully order membersfast again and store final population
    membersfast.sort(cmp=fullCmp)
    for m in membersfast[0:n]:
      m.keep=True

  def prune(self, popsize):
    for m in self.members:
      m.keep = False

    self.markBestN(self.members, popsize)

    testsIn = map(lambda m: m.numTests(self.inputSize()), self.members)
    testsOut = map(lambda m: m.numTests(self.inputSize()), self.notadded)
    self.trialsLog.writerow((self.inputSize(), mean(testsIn), mean(testsOut), mean(testsIn+testsOut)))

    killed = filter(lambda m: m.keep, self.members)
    self.members = filter(lambda m: m.keep, self.members)
    self.notadded = []

  def birthFilter(self, parent, child):
    '''called when considering adding child to population'''
    childCmp = self.testers[-1].comparer(0, config.offspring_confidence_pct, config.offspring_max_trials)
    return childCmp(parent, child) > 0

  def printPopulation(self):
    print "round n = %d"%self.inputSize()
    for m in self.members:
      if self.baseline is not None:
        self.testers[-1].testN(self.baseline, config.compare_min_trials)
      print "  * ", m, m.resultsStr(self.inputSize(), self.baseline)

  def generation(self):
    self.test(config.compare_min_trials)
    for z in xrange(config.rounds_per_level):
      self.randomMutation(config.population_high_size)
      self.prune(config.population_low_size)
      self.printPopulation()
    self.testers[-1].cleanup()
    self.testers.append(self.testers[-1].nextTester())

def addMutators(candidate, info, ignore=None, weight=1.0):
  if ignore is None:
    ignore=set()
  try:
    transform = info.name()
    if transform in ignore:
      return
    ignore.add(transform)
  except:
    transform = ""
  for ac in info.algchoices():
    logging.debug("added Mutator " + transform + "/" + ac['name'] + " => algchoice")
    candidate.addMutator(mutators.RandAlgMutator(transform, ac['number'], mutators.config.first_lvl, weight=weight))
    for a in info.rulesInAlgchoice(ac['number']):
      candidate.addMutator(mutators.AddAlgLevelMutator(transform, ac['number'], a, weight=weight))
  for ta in info.tunables():
    if ta['type'] in config.lognorm_tunable_types:
      logging.debug("added Mutator " + transform + "/" + ta['name'] + " => lognorm")
      candidate.addMutator(mutators.LognormRandCutoffMutator(ta['name'], weight=weight))
    elif ta['type'] in config.uniform_tunable_types:
      assert False
    elif ta['type'] in config.ignore_tunable_types:
      pass
    else:
      logging.warn("unknown tunable %s (%s)"%(ta['name'], ta['type']))
  
  for sub in info.calls():
    addMutators(candidate, sub, ignore, weight/2.0)

def autotune(benchmark):
  if config.debug:
    logging.basicConfig(level=logging.DEBUG)
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName(benchmark)
  pbutil.compileBenchmarks([benchmark])
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  main = mainname([pbutil.benchmarkToBin(benchmark)])
  tester = CandidateTester(benchmark, 1)
  try:
    roundtimes = storagedirs.openCsvStats("roundtimings", ("round", "input_size", "cumulative", "incremental"))
    candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml)
    baseline = None
    addMutators(candidate, infoxml.globalsec())
    addMutators(candidate, infoxml.transform(main))
    pop = Population(candidate, tester, baseline)
    t1 = time.time()
    t2 = t1
    for roundNumber in xrange(config.rounds):
      pop.generation()
      t3 = time.time()
      roundtimes.writerow((roundNumber, pop.inputSize(1), t3-t1, t3-t2))
      t2 = t3
  finally:
    tester.cleanup()

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser(usage="usage: sgatuner.py [options] Benchmark")
  (options, args) = parser.parse_args()
  if len(args)!=1:
    parser.print_usage()
    sys.exit(1)
  storagedirs.callWithLogDir(lambda: autotune(args[0]), "/home/jansel/output", False)

