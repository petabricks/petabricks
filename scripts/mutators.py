#!/usr/bin/python
import itertools, random, math, logging
from scipy import stats
from tunerconfig import config

class MutateFailed(Exception):
  '''Exception thrown when a mutation can't be applied'''
  pass

class Mutator:
  '''mutates a candidate to create a new candidate, base class'''
  def __init__(self, weight=1.0):
    self.weight = weight
    self.accuracyHint = 0
  def mutate(self, candidate, n):
    '''
    Must Perform the following actions:
    1) Modify the config file
    2) Clear results effected by the change
    3) [optional] add new mutators to modify the change made
    '''
    raise Exception('must be implemented in subclass')
  
  
class LognormRandom:
  def random(self, start, minVal, maxVal):
    for z in xrange(config.rand_retries):
      v=int(start*stats.lognorm.rvs(1)+.5)
      if v>=minVal and v<=maxVal and start!=v:
        return v
    raise MutateFailed("lognorm random gen failed")

class UniformRandom:
  def random(self, start, minVal, maxVal):
    return stats.randint.rvs(minVal, maxVal+1)

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
    if newco <= 1:
      raise MutateFailed("n too small")
    lvl, kco, krn = self.findOpenSite(candidate, newco)
    candidate.clearResultsAbove(newco)
    candidate.config[kco] = newco
    candidate.config[krn] = self.alg
    candidate.addMutator(RandAlgMutator(self.transform, self.choicesite, lvl, self.weight))
    candidate.addMutator(LognormRandAlgCutoffMutator(self.transform, self.choicesite, lvl, self.weight))

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
    self.transform=transform
    self.choicesite=choicesite
    self.lvl=lvl
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

class RandAlgMutator(SetAlgMutator):
  def __init__(self, transform, choicesite, lvl, weight=1.0):
    SetAlgMutator.__init__(self, transform, choicesite, lvl, None, weight)
  def getVal(self, candidate, oldVal):
    return random.choice(candidate.infoxml.transform(self.transform).rulesInAlgchoice(self.choicesite))

class LognormRandCutoffMutator(SetTunableMutator, UniformRandom):
  '''randomize cutoff using lognorm distribution'''
  def invalidatesThreshold(self, candidate, oldVal, newVal):
    return min(oldVal, newVal)
  def getVal(self, candidate, oldVal):
    return self.random(oldVal, 1, config.cutoff_max_val)

class UniformRandMutator(SetTunableMutator, UniformRandom):
  '''randomize cutoff using uniform distribution'''
  def __init__(self, tunable, minVal, maxVal, weight):
    self.minVal = minVal
    self.maxVal = maxVal
    SetTunableMutator.__init__(self, tunable, None, weight)
  def getVal(self, candidate, oldVal):
    return self.random(oldVal, self.minVal, self.maxVal+1)

class LognormRandAlgCutoffMutator(LognormRandCutoffMutator, LognormRandom):
  '''randomize alg cutoff using lognorm distribution'''
  def __init__(self, transform, choicesite, lvl, weight=1.0):
    self.kcodown = config.fmt_cutoff % (transform, choicesite, lvl-1)
    self.kco     = config.fmt_cutoff % (transform, choicesite, lvl)
    self.kcoup   = config.fmt_cutoff % (transform, choicesite, lvl+1)
    LognormRandCutoffMutator.__init__(self, self.kco, None, weight)
  def getVal(self, candidate, oldVal):
    '''threshold the random value'''
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
    return self.random(oldVal, down, up)


class TunableArrayMutator(Mutator):
  def __init__(self, tunable, minVal, maxVal, weight=1.0):
    self.tunable = tunable
    self.minVal = minVal
    self.maxVal = maxVal
    Mutator.__init__(self, weight)
  def getVal(self, candidate, oldVal):
    return self.random(oldVal, self.minVal, self.maxVal)
  def mutate(self, candidate, n):
    i = int(math.log(n, 2))
    candidate.clearResultsAbove(min(n, 2**i-1))
    old = candidate.config[config.fmt_bin % (self.tunable, i)]
    new = self.getVal(candidate, old)
    print str(candidate),self.tunable, old, new
    ks = set(candidate.config.keys())
    assert config.fmt_bin%(self.tunable, i) in ks
    while config.fmt_bin%(self.tunable, i) in ks:
      candidate.config[config.fmt_bin % (self.tunable, i)] = new
      i+=1
  def reset(self, candidate):
    candidate.clearResults()
    i = 0
    ks = set(candidate.config.keys())
    assert config.fmt_bin%(self.tunable, i) in ks
    while config.fmt_bin%(self.tunable, i) in ks:
      if candidate.config[config.fmt_bin % (self.tunable, i)]<self.minVal:
        candidate.config[config.fmt_bin % (self.tunable, i)] = self.minVal
      i+=1

class LognormTunableArrayMutator(TunableArrayMutator, LognormRandom):
  pass

class UniformTunableArrayMutator(TunableArrayMutator, UniformRandom):
  pass

