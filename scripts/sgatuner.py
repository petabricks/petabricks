#!/usr/bin/python
import itertools, random, subprocess, os, sys
import pbutil, mutators
import logging
import candidatetester
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester
from mutators import MutateFailed
from traininginfo import TrainingInfo
from tunerconfig import config

def mainname(cmd):
  cmd.append("--name")
  p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
  cmd.pop()
  os.waitpid(p.pid, 0)
  lines = p.stdout.readlines()
  return lines[-1].strip()

class Population:
  def __init__(self, initial, tester, baseline=None):
    self.members=[initial]
    self.testers=[tester]
    self.baseline=baseline
  
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
          c.mutate(self.testers[-1].n)
          break
        except MutateFailed:
          if z==config.mutate_retries-1:
            logging.debug("mutate failed, try %d of %d" % (z, config.mutate_retries-1))
          continue
      if c.config in triedConfigs:
        continue
      triedConfigs.add(c.config)
      for z in xrange(config.offspring_min_trials):
        self.testers[-1].test(c, limit=p.reasonableLimit(self.testers[-1].n))
      if self.birthFilter(p,c):
        self.members.append(c)
      else:
        c.rmcfgfile()
    if len(originalPop)<len(self.members):
      logging.debug("added "+', '.join(map(str,set(self.members)-set(originalPop))))
    return tries

  def pruneSingleMetric(self, population, popsize, metric):
    '''shrink the population to popsize by removing low scoring candidates'''
    if len(population)<=popsize:
      return
    fastCmp = self.testers[-1].comparer(metric, 0.00, 0)
    fullCmp = self.testers[-1].comparer(metric, config.compare_confidence_pct, config.compare_max_trials)
    # a rough partitioning based on fastCmp
    population.sort(cmp=fastCmp)
    membersfast=list(population[0:popsize])
    membersslow=list(population[popsize:])
    # fully order membersfast
    membersfast.sort(cmp=fullCmp)
    # check if any of membersslow should make the cut
    cutoffAlg=membersfast[-1]
    membersfast.extend(filter(lambda x: fullCmp(cutoffAlg,x)>0, membersslow))
    # fully order membersfast again and store final population
    membersfast.sort(cmp=fullCmp)
    pruned=set(population)-set(membersfast[0:popsize])
    del population[0:]
    population.extend(membersfast[0:popsize])
    if len(pruned):
      logging.debug("removed "+', '.join(map(str,pruned)))

  def prune(self, popsize):
    self.pruneSingleMetric(self.members, popsize, candidatetester.config.timing_metric_idx)

  def birthFilter(self, parent, child):
    '''called when considering adding child to population'''
    childCmp = self.testers[-1].comparer(0, config.offspring_confidence_pct, config.offspring_max_trials)
    return childCmp(parent, child) > 0

  def printPopulation(self):
    print "round n = %d"%self.testers[-1].n
    for m in self.members:
      if self.baseline is not None:
        self.testers[-1].testN(self.baseline, config.compare_min_trials)
      print "  * ", m, m.resultsStr(self.testers[-1].n, self.baseline)

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
    candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml)
    baseline = None
    addMutators(candidate, infoxml.globalsec())
    addMutators(candidate, infoxml.transform(main))
    pop = Population(candidate, tester, baseline)
    for x in xrange(config.rounds):
      pop.generation()
  finally:
    tester.cleanup()

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser(usage="usage: sgatuner.py [options] Benchmark")
  (options, args) = parser.parse_args()
  if len(args)!=1:
    parser.print_usage()
    sys.exit(1)
  candidatetester.callWithLogDir(lambda: autotune(args[0]), "/home/jansel/output", False)

