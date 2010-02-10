#!/usr/bin/python
import itertools, random, subprocess, os
import pbutil, mutators
import logging
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester
from mutators import MutateFailed
from traininginfo import TrainingInfo

class config:
  mutate_retries = 10
  compare_confidence_pct = 0.95
  compare_max_trials = 25
  compare_min_trials = 3
  offspring_confidence_pct = 0.95
  offspring_max_trials = 15
  offspring_min_trials = 3
  population_growth_attempts = 10
  population_high_size = 20
  population_low_size  = 1
  multimutation = True

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
    for z in xrange(count):
      for m in self.members:
        self.testers[-1].test(m)
  
  def grow(self, tries, maxpopsize=None):  
    '''grow the population using cloning and random mutation'''
    originalPop = list(self.members)
    triedConfigs = set(map(lambda x: x.config, self.members))
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
        self.testers[-1].test(c)
      if self.birthFilter(p,c):
        self.members.append(c)
    if len(originalPop)<len(self.members):
      logging.debug("added "+', '.join(map(str,set(self.members)-set(originalPop))))
    return tries

  def prune(self, popsize):
    '''shrink the population to popsize by removing low scoring candidates'''
    if len(self.members)<=popsize:
      return
    fastCmp = self.testers[-1].comparer(0, 0.00, 0)
    fullCmp = self.testers[-1].comparer(0, config.compare_confidence_pct, config.compare_max_trials)
    # a rough partitioning based on fastCmp
    self.members.sort(cmp=fastCmp)
    membersfast=list(self.members[0:popsize])
    membersslow=list(self.members[popsize:])
    # fully order membersfast
    membersfast.sort(cmp=fullCmp)
    # check if any of membersslow should make the cut
    cutoffAlg=membersfast[-1]
    membersfast.extend(filter(lambda x: fullCmp(cutoffAlg,x)>0, membersslow))
    # fully order membersfast again and store final population
    membersfast.sort(cmp=fullCmp)
    pruned=set(self.members)-set(membersfast[0:popsize])
    self.members = membersfast[0:popsize]
    if len(pruned):
      logging.debug("removed "+', '.join(map(str,pruned)))

  def birthFilter(self, parent, child):
    '''called when considering adding child to population'''
    childCmp = self.testers[-1].comparer(0, config.offspring_confidence_pct, config.offspring_max_trials)
    return childCmp(parent, child) > 0

  def printPopulation(self):
    print "round n = %d"%self.testers[-1].n
    for m in self.members:
      if self.baseline is None:
        print "  * ", m, "actual:", m.metrics[0][self.testers[-1].n]
      else:
        for x in xrange(config.compare_min_trials):
          self.testers[-1].test(self.baseline)
        print "  * ", m, "actual:", m.metrics[0][self.testers[-1].n], "baseline:", \
            self.baseline.metrics[0][self.testers[-1].n].strdelta(m.metrics[0][self.testers[-1].n])

  def generation(self):
    self.test(config.compare_min_trials)
    self.grow(config.population_growth_attempts, config.population_high_size)
    self.prune(config.population_low_size)
    self.printPopulation()
    self.testers.append(self.testers[-1].nextTester())

def testold():
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName('Sort')
  pbutil.compileBenchmarks([benchmark])
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  tester = CandidateTester(benchmark, 1)
  try:
    transform = "SortSubArray"
    candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml)
    candidate.addMutator(mutators.RandAlgMutator(transform, 0, mutators.config.first_lvl))
    for a in candidate.infoxml.transform(transform).rulesInAlgchoice(0):
      candidate.addMutator(mutators.AddAlgLevelMutator(transform, 0, a))
    pop = Population(candidate, tester, candidate)
    for x in xrange(25):
      pop.generation()
  finally:
    tester.cleanup()

def addMutators(candidate, info, ignore=None, weight=1.0):
  if ignore is None:
    ignore=set()
  if info.name() in ignore:
    return
  ignore.add(info.name())
  print info.name()
  print "  AlgChoices", info.algchoices()
  ta=info.tunables()
  print "  Tunables", set(map(lambda x: x['type'], ta))
  
  for sub in info.calls():
    addMutators(candidate, sub, ignore, weight/2.0)

def autotune(benchmark):
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName(benchmark)
  pbutil.compileBenchmarks([benchmark])
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  main = mainname([pbutil.benchmarkToBin(benchmark)])
  tester = CandidateTester(benchmark, 1)
  candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml)
  addMutators(candidate, infoxml.transform(main))
  tester.cleanup()

# tester = CandidateTester(benchmark, 1)
# candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml)
# try:
# candidate.addMutator(mutators.RandAlgMutator(transform, 0, mutators.config.first_lvl))
#   for a in candidate.infoxml.transform(transform).rulesInAlgchoice(0):
#     candidate.addMutator(mutators.AddAlgLevelMutator(transform, 0, a))
#   pop = Population(candidate, tester, candidate)
#   for x in xrange(25):
#     pop.generation()
# finally:
#   tester.cleanup()

if __name__ == "__main__":
  logging.basicConfig(level=logging.DEBUG)
  autotune("Sort")


