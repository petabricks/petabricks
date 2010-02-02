#!/usr/bin/python
import itertools, random
import pbutil, mutators
import logging
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester
from mutators import MutateFailed

class config:
  mutate_retries = 10
  compare_confidence_pct = 0.95
  compare_max_trials = 25
  compare_min_trials = 3
  offspring_confidence_pct = 0.95
  offspring_max_trials = 10
  offspring_min_trials = 3
  population_growth_attempts = 10
  population_high_size = 20
  population_low_size  = 1

class Population:
  def __init__(self, initial, tester):
    self.members=[initial]
    self.testers=[tester]
  
  def test(self, count):
    logging.debug("testing %d population members, %d times each" % (len(self.members), count))
    for z in xrange(count):
      for m in self.members:
        self.testers[-1].test(m)
  
  def grow(self, tries, maxpopsize=None):  
    '''grow the population using cloning and random mutation'''
    originalPop = list(self.members)
    for y in xrange(tries):
      if maxpopsize and len(self.members) >= maxpopsize:
        break
      p=random.choice(originalPop)
      c=p.clone()
      for z in xrange(config.mutate_retries):
        try:
          c.mutate(self.testers[-1].n)
          break
        except MutateFailed:
          logging.debug("mutate failed, try %d of %d" % (z, config.mutate_retries-1))
          continue
      for z in xrange(config.offspring_min_trials):
        self.testers[-1].test(c)
      if self.birthFilter(p,c):
        logging.debug("new candidate added to population")
        self.members.append(c)
      else:
        logging.debug("add attempt failed")

  def prune(self, popsize):
    '''shrink the population to popsize by removing low scoring candidates'''
    logging.debug("pruning %d of %d population members" % (max(0,len(self.members)-popsize), len(self.members)))
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
    self.members = membersfast[0:popsize]

  def birthFilter(self, parent, child):
    childCmp = self.testers[-1].comparer(0, config.offspring_confidence_pct, config.offspring_max_trials)
    return childCmp(parent, child) > 0

  def generation(self):
    self.test(config.compare_min_trials)
    self.grow(config.population_growth_attempts, config.population_high_size)
    self.prune(config.population_low_size)
    logging.debug("begin round n = %d"%self.testers[-1].n)
    for m in self.members:
      print "  > ", m, "=", m.metrics[0][self.testers[-1].n]
    self.testers.append(self.testers[-1].nextTester())

if __name__ == "__main__":
  logging.basicConfig(level=logging.DEBUG)
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName('Sort')
  pbutil.compileBenchmarks([benchmark])
  tester = CandidateTester(benchmark, 2)
  try:
    candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)))
    for a in xrange(7):
      candidate.addMutator(mutators.AddAlgLevelMutator("SortSubArray", 0, a))
      candidate.addMutator(mutators.SetAlgMutator("SortSubArray",0, mutators.config.first_lvl, a))
    pop = Population(candidate, tester)
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
    pop.generation()
  finally:
    tester.cleanup()


