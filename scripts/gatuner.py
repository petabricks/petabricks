#!/usr/bin/python
import itertools
import pbutil
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester

class config:
  fmt_cutoff = "%s_%d_lvl%d_cutoff"
  fmt_rule   = "%s_%d_lvl%d_rule"
  first_lvl = 1

class MutateFailed(Exception):
  '''Exception thrown when a mutation can't be applied'''
  pass

class Mutator:
  '''mutates a candidate to create a new candidate, base class'''
  def __init__(self, weight=1.0):
    self.weight=weight
  def mutate(self, candidate, n):
    raise Exception('must be implemented in subclass')

class AddAlgLevelMutator(Mutator):
  '''add a new alg level to the target choice site'''
  def __init__(self, transform, choicesite, alg, weight=1.0):
    self.transform = transform
    self.choicesite = choicesite
    self.alg = alg
    Mutator.__init__(self, weight)
  def mutate(self, candidate, n):
    newco = 2*n/3
    for lvl in itertools.count(config.first_lvl+1):
      kco     = config.fmt_cutoff % (self.transform, self.choicesite, lvl)
      krn     = config.fmt_rule   % (self.transform, self.choicesite, lvl)
      krnLast = config.fmt_rule   % (self.transform, self.choicesite, lvl-1)
      try:
        co = candidate.config[kco] 
      except:
        raise MutateFailed("no tunables for level "+str(lvl))
      if co >= 2**30:
        # found the top level
        if candidate.config[krnLast] == self.alg:
          raise MutateFailed("last level has same alg")
        candidate.config[kco] = newco
        candidate.config[krn] = self.alg
        return
      if co >= newco:
        raise MutateFailed("higher levels already exist")

class Population:

if __name__ == "__main__":
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName('Sort')
  pbutil.compileBenchmarks([benchmark])
  tester = CandidateTester(benchmark, 768, 5.0)
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
    m5.mutate(candidate3, 500)
    print "CANDIDATE1"
    print candidate.config
    print "CANDIDATE2"
    print candidate2.config
    print "CANDIDATE3"
    print candidate3.config
  finally:
    tester.cleanup()







