#!/usr/bin/python
import itertools, random
import pbutil
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester

class config:
  fmt_cutoff = "%s_%d_lvl%d_cutoff"
  fmt_rule   = "%s_%d_lvl%d_rule"
  first_lvl = 1
  cutoff_max_val = 2**30
  mutate_retries = 10
  compare_confidence_pct = 0.95
  compare_max_trials     = 25

class MutateFailed(Exception):
  '''Exception thrown when a mutation can't be applied'''
  pass

class Mutator:
  '''mutates a candidate to create a new candidate, base class'''
  def __init__(self, weight=1.0):
    self.weight=weight
  def mutate(self, candidate, n):
    '''
    Must Perform the following actions:
    1) Modify the config file
    2) Clear results effected by the change
    3) [optional] add new mutators to modify the change made
    '''
    raise Exception('must be implemented in subclass')

class AddAlgLevelMutator(Mutator):
  '''add a new alg level to the target choice site'''
  def __init__(self, transform, choicesite, alg, weight=1.0):
    self.transform = transform
    self.choicesite = choicesite
    self.alg = alg
    Mutator.__init__(self, weight)
  def findOpenSite(self, candidate, newco):
    for lvl in itertools.count(config.first_lvl+1):
      kco     = config.fmt_cutoff % (self.transform, self.choicesite, lvl)
      krn     = config.fmt_rule   % (self.transform, self.choicesite, lvl)
      krnLast = config.fmt_rule   % (self.transform, self.choicesite, lvl-1)
      try:
        co = candidate.config[kco] 
      except:
        raise MutateFailed("no tunables for level "+str(lvl))
      if co >= config.cutoff_max_val:
        # found the top level
        if candidate.config[krnLast] == self.alg:
          raise MutateFailed("last level has same alg")
        return kco, krn
      if co >= newco:
        raise MutateFailed("higher levels already exist")
  def mutate(self, candidate, n):
    newco = 2*n/3
    kco, krn = self.findOpenSite(candidate, newco)
    candidate.clearResultsAbove(newco)
    candidate.config[kco] = newco
    candidate.config[krn] = self.alg

class Population:
  def __init__(self, initial, tester):
    self.members=[initial]
    self.testers=[tester]
  
  def test(self, n):
    pass

  def grow(self, popsize):
    '''grow the population to popsize using cloning and random mutation'''
    originalPop = list(self.members)
    while len(self.members) < popsize:
      c=random.choice(originalPop).clone()
      for z in xrange(config.mutate_retries):
        try:
          c.mutate(self.testers[-1].n)
          break
        except MutateFailed:
          continue
      self.members.append(c)

  def prune(self, popsize, n):
    '''shrink the population to popsize by removing low scoring candidates'''
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

if __name__ == "__main__":
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName('Sort')
  pbutil.compileBenchmarks([benchmark])
  tester = CandidateTester(benchmark, 768)
  try:
    candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)))
    candidate2 = candidate.clone()
    candidate3 = candidate.clone()
    m2 = AddAlgLevelMutator("SortSubArray",0, 2)
    m3 = AddAlgLevelMutator("SortSubArray",0, 3)
    m4 = AddAlgLevelMutator("SortSubArray",0, 4)
    m5 = AddAlgLevelMutator("SortSubArray",0, 5)
    m2.mutate(candidate3, 100)
    m3.mutate(candidate3, 200)
    m4.mutate(candidate3, 300)
    m5.mutate(candidate3, 400)
    print "CANDIDATE1"
    print candidate.config
    print "CANDIDATE2"
    print candidate2.config
    print "CANDIDATE3"
    print candidate3.config
  finally:
    tester.cleanup()


