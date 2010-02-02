#!/usr/bin/python
import itertools, random
from scipy import stats

class config:
  fmt_cutoff = "%s_%d_lvl%d_cutoff"
  fmt_rule   = "%s_%d_lvl%d_rule"
  first_lvl = 1
  cutoff_max_val = 2**30

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
        return lvl, kco, krn
      if co >= newco:
        raise MutateFailed("higher levels already exist")
  def mutate(self, candidate, n):
    newco = 2*n/3
    lvl, kco, krn = self.findOpenSite(candidate, newco)
    candidate.clearResultsAbove(newco)
    candidate.config[kco] = newco
    candidate.config[krn] = self.alg
    candidate.addMutator(ExpRandomizeCutoffMutator(self.transform, self.choicesite, lvl))

class SetTunableMutator(Mutator):
  def __init__(self, tunable, val=None, weight=1.0):
    self.tunable = tunable
    self.val = val
    Mutator.__init__(self, weight)

  def invalidatesThreshold(self, candidate, oldVal, newVal):
    return 0

  def getVal(self, candidate, oldVal):
    assert self.val is not None
    return self.val
  
  def mutate(self, candidate, n):
    old = candidate.config[self.tunable]
    new = self.getVal(candidate, old)
    candidate.config[self.tunable] = new
    candidate.clearResultsAbove(self.invalidatesThreshold(candidate, old, new))

class SetAlgMutator(SetTunableMutator):
  def __init__(self, transform, choicesite, lvl, alg, weight=1.0):
    krn     = config.fmt_rule   % (transform, choicesite, lvl)
    kco     = config.fmt_cutoff % (transform, choicesite, lvl)
    if lvl > config.first_lvl:
      self.lowerCutoff = kco
    else:
      self.lowerCutoff = None
    SetTunableMutator.__init__(self, krn, alg, weight)
  def invalidatesThreshold(self, candidate, oldVal, newVal):
    if self.lowerCutoff is not None:
      return candidate.config[self.lowerCutoff]
    return 0

class ExpRandomizeCutoffMutator(SetTunableMutator):
  def __init__(self, transform, choicesite, lvl, weight=1.0):
    self.kcodown = config.fmt_cutoff % (transform, choicesite, lvl-1)
    self.kco     = config.fmt_cutoff % (transform, choicesite, lvl)
    self.kcoup   = config.fmt_cutoff % (transform, choicesite, lvl+1)
    SetTunableMutator.__init__(self, self.kco, None, weight)
  def invalidatesThreshold(self, candidate, oldVal, newVal):
    return min(oldVal, newVal)
  def getVal(self, candidate, oldVal):
    newVal = int(oldVal*stats.lognorm.rvs(1))
    down = 1
    up = config.cutoff_max_val
    try:
      down=self.config[self.kcodown]+1
    except:
      pass
    try:
      up=self.config[self.kcoup]-1
    except:
      pass
    return max(down,min(newVal, up))


