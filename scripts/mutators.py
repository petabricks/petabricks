#!/usr/bin/python
import itertools, random, math, logging, csv
import storagedirs
import numpy
from scipy import stats
from tunerconfig import config

class MutateFailed(Exception):
  '''Exception thrown when a mutation can't be applied'''
  pass

class Mutator:
  nextId=0

  '''mutates a candidate to create a new candidate, base class'''
  def __init__(self, weight=1.0):
    self.weight = weight
    self.accuracyHint = 0
    self.mid = Mutator.nextId
    self.logfile = None
    Mutator.nextId += 1
    self.score=0.0
    self.results = {'better': 0.0,
                    'worse':  0.0,
                    'same':   0.0,
                    'fail':   0.0}
    self.timesSelected = 0  # total number of times this operator has been selected


  ''' mutatorLog is assumed to be sorted best-first according to the current evaluation metric'''
  def computeRocScore(self, mutatorLog):

    integral = 0
    x = 0 # x position in the ROC curve
    y = 0 # y position in the ROC curve
    for entry in mutatorLog.log:
      new_x = 0
      new_y = 0

      if entry.mutator == self:
        new_x = x
        new_y = y + 1
      else:
        new_x = x + 1
        new_y = y;

      integral += (new_x-x)*y + (new_x-x)*(new_y - y) / 2
      x = new_x
      y = new_y

    return integral
              
        
      

  def uniquename(self):
    return self.__class__.__name__+'_'+str(self.mid)

  def mutate(self, candidate, n):
    '''
    Must Perform the following actions:
    1) Modify the config file
    2) Clear results affected by the change
    3) [optional] add new mutators to modify the change made
    '''
    raise Exception('must be implemented in subclass')

  def result(self, r):
    self.timesSelected += 1
    self.results[r] += 1
    self.score=self.score*config.score_decay + int(r=='better')

  def __str__(self):
    return self.__class__.__name__

  def writelog(self, roundNumber, inputSize):
    if self.logfile is None:
      self.logfile = csv.writer(open(storagedirs.mutatorlog(self), 'w'))
      self.logfile.writerow(['#description',
                             'roundNumber',
                             'inputSize',
                             'score']
                             +self.results.keys())
    self.logfile.writerow([str(self),
                           roundNumber,
                           inputSize,
                           "%.2f"%self.score]
                           +self.results.values())



class LognormRandom:
  def random(self, start, minVal, maxVal):
    for z in xrange(config.rand_retries):
      v=int(start*stats.lognorm.rvs(1)+.5)
      #logging.debug("lognorm: start=%d, v=%d", start, v)
      if v>=minVal and v<=maxVal and start!=v:
        return v
    raise MutateFailed("lognorm random gen failed")

class UniformRandom:
  def random(self, start, minVal, maxVal):
    for z in xrange(config.rand_retries):
      v=int(stats.randint.rvs(minVal, maxVal+1))
      #logging.debug("uniform: start=%d, v=%d", start, v)
      if v>=minVal and v<=maxVal and start!=v:
        return v
    raise MutateFailed("lognorm random gen failed")

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
      krnlast = config.fmt_rule   % (self.transform, self.choicesite, lvl-1)
      try:
        co = candidate.config[kco] 
      except:
        raise MutateFailed("no tunables for level "+str(lvl))
      if co >= config.cutoff_max_val:
        # found the top level
        if candidate.config[krnlast] == self.alg:
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
  
  def __str__(self):
    return Mutator.__str__(self)+" %s,alg%d"%(self.choicesite, self.alg)

class SetTunableMutator(Mutator):
  def __init__(self, tunable, val=None, weight=1.0):
    self.tunable = tunable
    self.val = val
    Mutator.__init__(self, weight)

  def invalidatesThreshold(self, candidate, oldVal, newVal):
    return 0

  def getVal(self, candidate, oldVal, inputSize):
    assert self.val is not None
    return self.val
  
  def mutate(self, candidate, n):
    old = candidate.config[self.tunable]
    new = self.getVal(candidate, old, n)
    candidate.config[self.tunable] = new
    candidate.clearResultsAbove(self.invalidatesThreshold(candidate, old, new))
  
  def __str__(self):
    return Mutator.__str__(self)+' '+self.tunable

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
  def getVal(self, candidate, oldVal, inputSize):
    return random.choice(candidate.infoxml.transform(self.transform).rulesInAlgchoice(self.choicesite))

class LognormRandCutoffMutator(SetTunableMutator, LognormRandom):
  '''randomize cutoff using lognorm distribution'''
  def invalidatesThreshold(self, candidate, oldVal, newVal):
    return min(oldVal, newVal)
  def getVal(self, candidate, oldVal, inputSize):
    return self.random(oldVal, 1, min(inputSize*1.5, config.cutoff_max_val))

class UniformRandMutator(SetTunableMutator, UniformRandom):
  '''randomize cutoff using uniform distribution'''
  def __init__(self, tunable, minVal, maxVal, weight):
    self.minVal = minVal
    self.maxVal = maxVal
    SetTunableMutator.__init__(self, tunable, None, weight)
  def getVal(self, candidate, oldVal, inputSize):
    return self.random(oldVal, self.minVal, self.maxVal+1)

class LognormRandAlgCutoffMutator(LognormRandCutoffMutator, LognormRandom):
  '''randomize alg cutoff using lognorm distribution'''
  def __init__(self, transform, choicesite, lvl, weight=1.0):
    self.kcodown = config.fmt_cutoff % (transform, choicesite, lvl-1)
    self.kco     = config.fmt_cutoff % (transform, choicesite, lvl)
    self.kcoup   = config.fmt_cutoff % (transform, choicesite, lvl+1)
    self.lvl = lvl
    LognormRandCutoffMutator.__init__(self, self.kco, None, weight)
  def getVal(self, candidate, oldVal, inputSize):
    '''threshold the random value'''
    down = 1
    #up = config.cutoff_max_val
    up = inputSize
    try:
      down=candidate.config[self.kcodown]+1
    except KeyError:
      pass
    try:
      up=candidate.config[self.kcoup]-1
    except KeyError:
      pass

    v=self.random(oldVal, down, up)
    assert v>=down and v<=up
    return v

class TunableArrayMutator(Mutator):
  def __init__(self, tunable, minVal, maxVal, weight=1.0):
    self.tunable = tunable
    self.minVal = minVal
    self.maxVal = maxVal
    Mutator.__init__(self, weight)
  def getVal(self, candidate, oldVal, inputSize):
    return self.random(oldVal, self.minVal, self.maxVal)
  def mutate(self, candidate, n):
    i = int(math.log(n, 2))
    candidate.clearResultsAbove(min(n, 2**i-1))
    old = candidate.config[config.fmt_bin % (self.tunable, i)]
    new = self.getVal(candidate, old, n)
    assert new >= self.minVal
    assert new <= self.maxVal
    #print str(candidate),self.tunable, old, new
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
        candidate.config[config.fmt_bin % (self.tunable, i)] = self.minVal+0
      i+=1

class LognormTunableArrayMutator(TunableArrayMutator, LognormRandom):
  pass

class UniformTunableArrayMutator(TunableArrayMutator, UniformRandom):
  pass

class IncrementTunableArrayMutator(TunableArrayMutator):
  def __init__(self, tunable, minVal, maxVal, inc, weight=1.0):
    self.inc = inc
    TunableArrayMutator.__init__(self, tunable, minVal, maxVal, weight)
  def random(self, oldVal, minVal, maxVal):
    return min(maxVal, max(minVal, oldVal+self.inc))

class ScaleTunableArrayMutator(TunableArrayMutator):
  def __init__(self, tunable, minVal, maxVal, inc, weight=1.0):
    self.inc = inc
    TunableArrayMutator.__init__(self, tunable, minVal, maxVal, weight)
  def random(self, oldVal, minVal, maxVal):
    return min(maxVal, max(minVal, oldVal*self.inc))

class MultiMutator(Mutator):
  def __init__(self, count=3, weight=1.0):
    self.count = count 
    Mutator.__init__(self, weight)

  def invalidatesThreshold(self, candidate, oldVal, newVal):
    return 0

  def mutate(self, candidate, n):
    for x in xrange(self.count):
      candidate.mutate(n)
    candidate.lastMutator=self
  def __str__(self):
    return Mutator.__str__(self)+' '+str(self.count)

class ChoiceSiteMutator(Mutator):
  '''add a new alg level to the target choice site'''
  def __init__(self, transform, choicesite, weight=1.0):
    self.transform = transform
    self.choicesite = choicesite
    Mutator.__init__(self, weight)

  def getCutoffs(self, candidate):
    rv = []
    last = 0
    for lvl in itertools.count(config.first_lvl+1):
      kco = config.fmt_cutoff % (self.transform, self.choicesite, lvl)
      try:
        co = candidate.config[kco] 
      except:
        break
      if co >= config.cutoff_max_val:
        break
      rv.append(co-last)
      last=co
    return rv

  def setCutoffs(self, candidate, cutoffs):
    last = 0
    for i, lvl in enumerate(itertools.count(config.first_lvl+1)):
      kco = config.fmt_cutoff % (self.transform, self.choicesite, lvl)
      if len(cutoffs)>i:
        last += cutoffs[i]
      else:
        last = config.cutoff_max_val
      if last > config.cutoff_max_val:
        last = config.cutoff_max_val
      try:
        candidate.config[kco] = last
      except:
        break

  def rescaleCutoffs(self, cutoffs, n):
    if sum(cutoffs)>n:
      scale = sum(cutoffs)/float(3*n/4.0)
      cutoffs = map(lambda x: int(x*scale), cutoffs)
    return cutoffs

  def randomAlg(self, candidate):
    return random.choice(candidate.infoxml.transform(self.transform).rulesInAlgchoice(self.choicesite))

  def getAlgs(self, candidate, n=None):
    rv = []
    for lvl in itertools.count(config.first_lvl):
      krn = config.fmt_rule % (self.transform, self.choicesite, lvl)
      try:
        rn = candidate.config[krn] 
      except KeyError:
        break
      rv.append(rn)
      if n is not None and len(rv)>n:
        break
    return rv
  
  def setAlgs(self, candidate, algs):
    for alg, lvl in zip(algs, itertools.count(config.first_lvl)):
      krn = config.fmt_rule % (self.transform, self.choicesite, lvl)
      try:
        candidate.config[krn] = alg
      except KeyError:
        break

  def mutate(self, candidate, n):
    coOld   = self.getCutoffs(candidate)
    algsOld = self.getAlgs(candidate, len(coOld))
    co, algs = self.mutateInner(candidate, n, list(coOld), list(algsOld))
    co = self.rescaleCutoffs(co, n)
    if co==coOld and algs==algsOld:
      raise MutateFailed()
    self.setCutoffs(candidate, co)
    self.setAlgs(candidate, algs)
    candidate.clearResults()
  
  def __str__(self):
    return Mutator.__str__(self)+" %s"%(self.choicesite)

class ShuffleAlgsChoiceSiteMutator(ChoiceSiteMutator):
  def mutateInner(self, candidate, n, co, algs):
    legalalgs = candidate.infoxml.transform(self.transform).rulesInAlgchoice(self.choicesite)
    algs = map(lambda x: random.choice(legalalgs), algs)
    return co, algs


class ShuffleCutoffsChoiceSiteMutator(ChoiceSiteMutator, LognormRandom):
  def mutateInner(self, candidate, n, co, algs):
    if len(co)==0:
      algs = [self.randomAlg(candidate), self.randomAlg(candidate), self.randomAlg(candidate), self.randomAlg(candidate)]
      co   = [64, 256, 1024]
    co = map(lambda x: self.random(x, 0, n), co)
    return co, algs

class ShuffleBotChoiceSiteMutator(ChoiceSiteMutator, LognormRandom):
  def mutateInner(self, candidate, n, co, algs):
    p = 1.0
    for i in xrange(len(co)):
      if random.random()<p:
        co[i] = self.random(co[i], 0, n)
      p /= 2.0
    p = 0.5
    for i in xrange(len(algs)):
      if random.random()<p:
        algs[i] = self.randomAlg(candidate)
      p /= 2.0
    return co, algs

class ShuffleTopChoiceSiteMutator(ChoiceSiteMutator, LognormRandom):
  def mutateInner(self, candidate, n, co, algs):
    p = 1.0
    for i in reversed(range(len(co))):
      if random.random()<p:
        co[i] = self.random(co[i], 0, n)
      p /= 2.0
    p = 0.5
    for i in reversed(range(len(algs))):
      if random.random()<p:
        algs[i] = self.randomAlg(candidate)
      p /= 2.0
    return co, algs
 
class AddLevelChoiceSiteMutator(ChoiceSiteMutator, LognormRandom):
  def mutateInner(self, candidate, n, co, algs):
    if len(co)>0:
      newco = co[-1]*2
    else:
      newco = 128
    return co+[newco], algs+[self.randomAlg(candidate)]
 
class RemoveLevelChoiceSiteMutator(ChoiceSiteMutator, LognormRandom):
  def mutateInner(self, candidate, n, co, algs):
    return co[0:-1], algs[0:-1]
  



