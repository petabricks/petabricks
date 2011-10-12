#!/usr/bin/python
import progress 
import itertools, random, subprocess, os, sys, time, warnings
import pbutil, mutators
import logging
import storagedirs 
import candidatetester
import shutil 
import tunerconfig
import configtool 
import re
import math
import time
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester
from mutators import MutateFailed
from traininginfo import TrainingInfo
from tunerconfig import config, option_callback
from tunerwarnings import InitialProgramCrash,ExistingProgramCrash,NewProgramCrash,TargetNotMet
from storagedirs import timers
import tunerwarnings
from pprint import pprint
import pdb

class TrainingTimeout(Exception):
  pass
  
def check_timeout():
  if time.time() > config.end_time:
    raise TrainingTimeout()

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
    self.best     = None
    self.notadded = []
    self.removed  = []
    self.failed   = set()
    self.testers  = [tester]
    self.baseline = baseline
    self.roundNumber = -1
    self.firstRound = True
    if config.candidatelog:
      self.candidatelog     = storagedirs.openCsvStats("candidatelog",
                                                       ['time',
                                                        'candidates',
                                                        'tests_complete',
                                                        'tests_timeout',
                                                        'tests_crashed',
                                                        'config_path',
                                                        'input_size',
                                                        'end_of_round',
                                                        'round_number'])
      self.candidateloglast = None
    self.starttime = time.time()
    self.onMembersChanged(True)
  
  def test(self, count):
    '''test each member of the pop count times'''
    self.failed=set()
    tests = []
    for z in xrange(count):
      tests.extend(self.members)
    random.shuffle(tests)
    for m in tests:
      check_timeout()
      if m not in self.failed and m.numTests(self.inputSize())<config.max_trials:
        try:
          self.testers[-1].test(m)
        except candidatetester.CrashException, e:
          if m.numTotalTests()==0:
            warnings.warn(InitialProgramCrash(e))
          else:
            warnings.warn(ExistingProgramCrash(e))
          self.failed.add(m)
          self.members.remove(m)

  def countMutators(self, mutatorFilter=lambda m: True):
    return sum(map(lambda x: len(filter(mutatorFilter, x.mutators)), self.members))

  def randomMutation(self, maxpopsize=None, mutatorFilter=lambda m: True):
    '''grow the population using cloning and random mutation'''
    originalPop = list(self.members)
    totalMutators = self.countMutators(mutatorFilter)
    tries = float(totalMutators)*config.mutations_per_mutator
    while tries>0:
      progress.remaining(tries)
      check_timeout()
      tries-=1
      if maxpopsize and len(self.members) >= maxpopsize:
        break
      if config.multimutation:
        p=random.choice(self.members)
      else:
        p=random.choice(originalPop)
      try:
        c=p.cloneAndMutate(self.inputSize(), mutatorFilter=mutatorFilter)
      except candidatetester.NoMutators:
        if self.countMutators(mutatorFilter)>0:
          continue
        else:
          return tries

      if c.config in self.triedConfigs and c.lastMutator:
        c.lastMutator.result('fail')
        continue
      self.triedConfigs.add(c.config)
      try:
        self.testers[-1].testN(c, config.min_trials, limit=p.reasonableLimit(self.inputSize()))
        if self.birthFilter(p,c):
          self.members.append(c)
          self.onMembersChanged(False)
        else:
          c.rmfiles()
          self.notadded.append(c)
      except candidatetester.CrashException, e:
        c.rmfiles()
        if c.lastMutator:
          c.lastMutator.result('fail')
        warnings.warn(NewProgramCrash(e))
    if len(originalPop)<len(self.members):
      logging.info("added "+', '.join(map(str,set(self.members)-set(originalPop))))
    return tries

  def guidedMutation(self):
    if config.print_log:
      print "GUIDED MUTATION"
    self.randomMutation(None, lambda m: m.accuracyHint)
    
  
  def birthFilter(self, parent, child):
    '''called when considering adding child to population'''
    same=True
    for m in xrange(len(config.metrics)):
      if config.accuracy_metric_idx == m and not self.isVariableAccuracy():
        continue
      childCmp = self.testers[-1].comparer(m, config.confidence_pct, config.max_trials)
      if childCmp(parent, child) > 0:
        logging.debug("adding %s through metric %d"%(str(child), m))
        child.lastMutator.result('better')
        return True
      if childCmp(parent, child) < 0:
        same=False
    if child.lastMutator:
      if same:
        child.lastMutator.result('same')
      else:
        child.lastMutator.result('worse')
    return False
  
  def inputSize(self, roundOffset=0):
    return self.testers[-1 - roundOffset].n

  def onMembersChanged(self, endOfRound):
    if config.candidatelog and len(self.members):
      fastCmp = self.testers[-1].comparer(config.timing_metric_idx, 0.00, 0)
      if len(self.members) > 1:
        m = list(self.members)
        m.sort(cmp=fastCmp)
        best = m[0]
      else:
        best = self.members[0]
      if best != self.candidateloglast or endOfRound:
        self.candidateloglast = best
        testCount = sum(map(lambda x: x.testCount, self.testers))
        timeoutCount = sum(map(lambda x: x.timeoutCount, self.testers))
        crashCount = sum(map(lambda x: x.crashCount, self.testers))
        self.candidatelog.writerow(("%.10f"%(time.time()-self.starttime),
                                    Candidate.nextCandidateId,
                                    testCount-timeoutCount-crashCount,
                                    timeoutCount,
                                    crashCount,
                                    storagedirs.relpath(best.cfgfile()),
                                    self.inputSize(),
                                    endOfRound,
                                    self.roundNumber))

  def markBestN(self, population, n, metric = config.timing_metric_idx):
    '''shrink the population to popsize by removing low scoring candidates'''
    fastCmp = self.testers[-1].comparer(metric, 0.00, 0)
    fullCmp = self.testers[-1].comparer(metric, config.confidence_pct, config.max_trials)
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
    return membersfast[0:n]

  def isVariableAccuracy(self):
    return self.members[0].infoxml.isVariableAccuracy()

  def accuracyTargets(self):
    if self.isVariableAccuracy():
      return map(lambda x: x.accuracyTarget(), self.members[0].infoxml.instances)
    else:
      return []

  def prune(self, popsize, isLast):
    for m in self.members:
      m.keep = False

    if config.accuracy_target is not None:
      t = filter(lambda x: x.hasAccuracy(self.inputSize(), config.accuracy_target), self.members)
      if len(t):
        best=self.markBestN(t, popsize)
        if isLast:
          storagedirs.cur.markBest(best[0].cid, self.inputSize(), None)
          self.best = best[0]
          best[0].writestats(self.inputSize(), storagedirs.cur.results())
    elif isLast and len(self.members):
      best=self.markBestN(self.members, popsize, config.timing_metric_idx)
      storagedirs.cur.markBest(best[0].cid, self.inputSize(), None)
      self.best = best[0]
      best[0].writestats(self.inputSize(), storagedirs.cur.results())

    for accLevel,accTarg in enumerate(self.accuracyTargets()):
      t = filter(lambda x: x.hasAccuracy(self.inputSize(), accTarg), self.members)
      if len(t):
        best=self.markBestN(t, popsize)
        if isLast:
          storagedirs.cur.markBest(best[0].cid, self.inputSize(), accLevel)
          best[0].writestats(self.inputSize(), storagedirs.cur.results(accLevel))
      else:
        warnings.warn(TargetNotMet(self.inputSize(), accTarg))


    if len(filter(lambda m: m.keep, self.members)) == 0:
      self.markBestN(self.members, popsize, config.timing_metric_idx)
      self.markBestN(self.members, popsize, config.accuracy_metric_idx)

    self.removed  += filter(lambda m: not m.keep, self.members)
    self.members  = filter(lambda m: m.keep, self.members)
    self.onMembersChanged(True)

  def accuracyTargetsMet(self):
    if not self.isVariableAccuracy():
      return True
    accTarg=max(self.accuracyTargets())
    t = filter(lambda x: x.hasAccuracy(self.inputSize(), accTarg), self.members)
    return len(t)

  def printPopulation(self):
    print "round n = %d"%self.inputSize()
    for m in self.members:
      if self.baseline is not None:
        self.testers[-1].testN(self.baseline, config.min_trials)
      print "  * ", m, m.resultsStr(self.inputSize(), self.baseline)

  def generation(self):
    progress.push()
    try:
      self.roundNumber += 1
      self.triedConfigs = set(map(lambda x: x.config, self.members))
      self.removed=[]
      self.notadded=[]
      self.test(config.max_trials)
      if len(self.members):
        for z in xrange(config.rounds_per_input_size):
          progress.subtask(config.rounds_per_input_size-z,
                           lambda: self.randomMutation(config.population_high_size))
          if not self.accuracyTargetsMet():
            self.guidedMutation()
          self.prune(config.population_low_size, False)
        self.prune(config.population_low_size, True)

        self.firstRound=False
      elif self.firstRound and len(self.failed) and config.min_input_size_nocrash>=self.inputSize():
        self.members = list(self.failed)
        if config.print_log:
          print "skip generation n = ",self.inputSize(),"(program run failed)"
      else:
        warnings.warn(tunerwarnings.AlwaysCrashes())
        
      if config.print_log:
        self.printPopulation()
      
      if not config.delete_output_dir:
        for m in self.members+self.removed:
          m.writestats(self.inputSize())
        if config.mutatorlog:
          mutators = set()
          for c in self.members:
            mutators |= set(c.mutators)
          for m in mutators:
            m.writelog(self.roundNumber, self.inputSize())

    except candidatetester.InputGenerationException, e:
      if e.testNumber==0 and self.inputSize()<=config.min_input_size_nocrash:
        if config.print_log:
          print "skip generation n = ",self.inputSize(),"(input generation failure)"
      else:
        warnings.warn(tunerwarnings.AlwaysCrashes())
    finally:
      progress.pop()

  def nextInputSize(self):
    self.testers[-1].cleanup()
    self.testers.append(self.testers[-1].nextTester())

  def statsHeader(self):
    return '#pop','#removed','#notadded', 'pop_trials_avg','removed_trials_avg','notadded_trials_avg', \
           '#total_trials', '#timeout_trials', '#crash_trials'

  def stats(self):
    def mean(x):
      if len(x):
        return sum(x)/float(len(x))
      else:
        return 0
    t1 = map(lambda m: m.numTests(self.inputSize()), self.members)
    t2 = map(lambda m: m.numTests(self.inputSize()), self.removed)
    t3 = map(lambda m: m.numTests(self.inputSize()), self.notadded)
    
    return len(t1),len(t2),len(t3),mean(t1),mean(t2),mean(t3),\
           self.testers[-1].testCount, self.testers[-1].timeoutCount, self.testers[-1].crashCount

def intorfloat(v):
  try:
    return int(v)
  except:
    return float(v)

def createTunableMutators(candidate, ta, weight):
  name = ta['name']
  if type(ta['min']) == type([]):
    l=map(intorfloat, ta['min'])
    h=map(intorfloat, ta['max'])
  else:
    l=intorfloat(ta['min'])
    h=intorfloat(ta['max'])
  size=intorfloat(ta['size'])
 # if 'accuracy' in ta['type']:
 #   #hack to support what the old autotuner did
 #   l+=1

  if ta['type'] in config.lognorm_tunable_types:
    return [mutators.LognormRandCutoffMutator(name, weight=weight)]
  elif ta['type'] in config.uniform_tunable_types:
    return [mutators.UniformRandMutator(name, l, h, weight=weight)]
  elif ta['type'] in config.autodetect_tunable_types:
    if l <= 1 and h > 2**16:
      return [mutators.LognormRandCutoffMutator(name, weight=weight)]
    else:
      return [mutators.UniformRandMutator(name, l, h, weight=weight)]
  elif ta['type'] in config.lognorm_sizespecific_tunable_types:
    ms = [mutators.LognormTunableSizeSpecificMutator(name, l, h, weight=weight),
          mutators.IncrementTunableSizeSpecificMutator(name, l, h, 8, weight=weight),
          mutators.ScaleTunableSizeSpecificMutator(name, l, h, 2, weight=weight),
          ]
    ms[-1].reset(candidate)
    return ms
  elif ta['type'] in config.optimize_tunable_types:
    # invoke special handling for nwkde benchmark
    if ta['tname'][0:7] == 'NWKDEVA':
      nwkdeFlag = True
    else:
      nwkdeFlag = False
    ms = [mutators.OptimizeTunableMutator(ta, weight=weight, nwkdeFlag=nwkdeFlag, maxiter=1), \
          mutators.OptimizeTunableMutator(ta, weight=weight, nwkdeFlag=nwkdeFlag, maxiter=2), \
          mutators.OptimizeTunableMutator(ta, weight=weight, nwkdeFlag=nwkdeFlag, maxiter=4), \
          mutators.LogNormFloatTunableMutator(ta, weight=weight, nwkdeFlag=nwkdeFlag)]
    return ms
  elif ta['type'] in config.ignore_tunable_types:
    pass
  else:
    warnings.warn(tunerwarnings.UnknownTunableType(name, ta['type']))
  return []

def createChoiceSiteMutators(candidate, info, ac, weight):
  transform = info.name()
  ms = []
  ms.append(mutators.RandAlgMutator(transform, ac['number'], mutators.config.first_lvl, weight=weight))
  for a in info.rulesInAlgchoice(ac['number']):
    ms.append(mutators.AddAlgLevelMutator(transform, ac['number'], a, weight=weight))
  #ms.append(mutators.ShuffleAlgsChoiceSiteMutator(transform, ac['number'], weight=weight))
  #ms.append(mutators.ShuffleCutoffsChoiceSiteMutator(transform, ac['number'], weight=weight))
  return ms

# Identify tunables that should be grouped into arrays for mutators that act
# on arrays of values rather than scalars
def groupTunables(transformName, tunables):
  names = map(lambda x: x['name'], tunables)
  tunableSets = [] # return value
  tunableIndex = {} # index to unique tunables by variable name
  for name, tunable in zip(names, tunables):
    tunable['sizeSpecificFlag'] = tunable['type'] in config.sizespecific_tunable_types
    m = re.match('^%s_i(\d+)_(\w+)$' % transformName, name)
    # for now, only group tunables that are optimizable into an array
    if m and tunable['type'] in config.optimize_tunable_types:
      (index, varName) = m.group(1, 2)
      index = int(index)
#      print "Matched array tunable: %s[%d]" % (varName, index)
#      print "  Size-specific: %s" % tunable['sizeSpecificFlag']
      if not varName in tunableIndex:
        assert(index == 0)
        tunableIndex[varName] = tunable
        tunable['arrayFlag'] = True
        tunable['tname'] = transformName
        tunable['vname'] = varName
        tunable['size'] = 1
        # convert min and max to arrays
        tunable['min'] = [tunable['min']]
        tunable['max'] = [tunable['max']]
        tunableSets.append(tunable)
      else:
        # arrayTunable gets the tunable to be returned
        arrayTunable = tunableIndex[varName]
        assert(arrayTunable['size'] == index)
#        print "Updating tunable 'size' from %d to %d" % (arrayTunable['size'], arrayTunable['size'] + 1)
        arrayTunable['size'] += 1
        arrayTunable['min'].append(tunable['min'])
        arrayTunable['max'].append(tunable['max'])
    else:
#      print "Adding non-array tunable: %s" % name
      tunable['arrayFlag'] = False
      tunable['size'] = 1
      tunableSets.append(tunable)
  return tunableSets

def addMutators(candidate, info, acf, taf, ignore=None, weight=1.0):
  '''seed the pool of mutators from the .info file'''
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
    ms = acf(candidate, info, ac, weight)
    for m in ms:
      logging.info("added Mutator " + transform + "/AlgChoice" + str(ac['number']) + " => " + str(m))
      candidate.addMutator(m)

  tunableSets = groupTunables(info.name(), info.tunables())
  for ta in tunableSets:
    ms = taf(candidate, ta, weight)
    for m in ms:
      if 'accuracy' in ta['type'] or 'double' in ta['type']:
        m.accuracyHint = 1
      logging.info("added Mutator " + transform + "/" + ta['name'] + " => " + str(m))
      candidate.addMutator(m)
  
  for sub in info.calls():
    addMutators(candidate, sub, acf, taf, ignore, weight/2.0)

def init(benchmark, acf=createChoiceSiteMutators, taf=createTunableMutators):
  if config.debug:
    logging.basicConfig(level=logging.DEBUG)
    config.pause_on_crash = True
  if not config.threads:
    config.threads = pbutil.cpuCount()
  for k in filter(len, config.abort_on.split(',')):
    warnings.simplefilter('error', getattr(tunerwarnings,k))
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  if not config.main:
    config.main = mainname([pbutil.benchmarkToBin(benchmark)])
  tester = CandidateTester(benchmark, config.min_input_size)
  if config.seed is None:
    cfg = defaultConfigFile(pbutil.benchmarkToBin(tester.app))
  else:
    cfg = configtool.ConfigFile(config.seed)
  candidate = Candidate(cfg, infoxml.transform(config.main))
  addMutators(candidate, infoxml.globalsec(), acf, taf)
  addMutators(candidate, infoxml.transform(config.main), acf, taf)
  candidate.addMutator(mutators.MultiMutator(2))
  if not config.delete_output_dir:
    storagedirs.cur.dumpConfig()
    storagedirs.cur.dumpGitStatus()
    storagedirs.cur.saveFile(pbutil.benchmarkToInfo(benchmark))
    storagedirs.cur.saveFile(pbutil.benchmarkToBin(benchmark))
  return candidate, tester

def autotuneInner(benchmark, returnBest=None):
  """Function running the autotuning process.
If returnBest is specified, it should be a list. The best candidate found will 
be added to that list"""
  progress.push()
  config.benchmark = benchmark
  candidate, tester = init(benchmark)
  try:
    pop = Population(candidate, tester, None)
    candidate.pop = pop
    
    if not pop.isVariableAccuracy() and config.accuracy_target:
      logging.info("clearing accuracy_target")
      config.accuracy_target = None

    stats = storagedirs.openCsvStats("roundstats", 
        ("round",
         "input_size",
         "cumulative_sec",
         "incremental_sec",
         "testing_sec",
         "inputgen_sec")+pop.statsHeader())
    timers.total.start()
    config.end_time = time.time() + config.max_time
    try:
      progress.remaining(config.max_input_size*(1+config.final_rounds))
      while pop.inputSize() < config.max_input_size:
        progress.status("autotuning %s: input %d of %d" % (config.benchmark, pop.inputSize(), config.max_input_size))
        pop.generation()
        stats.writerow((pop.roundNumber,
                        pop.inputSize(),
                        timers.total.total(),
                        timers.total.lap(),
                        timers.testing.lap(),
                        timers.inputgen.lap())+pop.stats())
        pop.nextInputSize()
        progress.remaining(config.max_input_size - pop.inputSize() + config.max_input_size*config.final_rounds)
      for z in xrange(config.final_rounds):
        pop.generation()
        stats.writerow((pop.roundNumber,
                        pop.inputSize(),
                        timers.total.total(),
                        timers.total.lap(),
                        timers.testing.lap(),
                        timers.inputgen.lap())+pop.stats())
        progress.remaining((config.final_rounds - z)*config.max_input_size)
    except TrainingTimeout:
      pass
    timers.total.stop()

    #check to make sure we did something:
    if pop.firstRound:
      warnings.warn(tunerwarnings.AlwaysCrashes())
      
    logging.info("TODO: using acc target: "+str(config.accuracy_target))
    return pop.best
  finally:
    if pop.best and config.output_cfg:
      print pop.best.cfgfile(),"=>" , config.output_cfg
      shutil.copyfile(pop.best.cfgfile(), config.output_cfg)
    if pop.best and returnBest is not None:
      returnBest.append(pop.best)
    at = storagedirs.getactivetimers()
    if len(at):
      storagedirs.openCsvStats("timers", at.keys()).writerow(at.values())
    if tester:
      tester.cleanup()
    progress.pop()

def autotune(benchmark, returnBest=None):
  return storagedirs.callWithLogDir(lambda: autotuneInner(benchmark, returnBest),
                                    config.output_dir,
                                    config.delete_output_dir)

def regression_check(benchmark):
  tunerconfig.applypatch(tunerconfig.patch_regression)
  storagedirs.callWithLogDir(lambda: autotuneInner(benchmark),
                             config.output_dir,
                             config.delete_output_dir)

def recompile():
  pbutil.chdirToPetabricksRoot();
  config.benchmark = pbutil.normalizeBenchmarkName(config.benchmark)
  config.output_cfg = pbutil.benchmarkToCfg(config.benchmark) 
  if config.recompile:
    pbutil.compilePetabricks();
    pbutil.compileBenchmarks([config.benchmark])

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser(usage="usage: sgatuner.py [options] Benchmark")
  parser.add_option("--check",
                    action="store_true", dest="check", default=False,
                    help="check for correctness")
  parser.add_option("--debug",
                    action="store_true", dest="debug", default=False,
                    help="enable debugging options")
  parser.add_option("-n", type="int", help="input size to train for")
  parser.add_option("--max_time",              type="float",  action="callback", callback=option_callback)
  parser.add_option("--rounds_per_input_size", type="int",    action="callback", callback=option_callback)
  parser.add_option("--mutations_per_mutator", type="int",    action="callback", callback=option_callback)
  parser.add_option("--output_dir",            type="string", action="callback", callback=option_callback)
  parser.add_option("--population_high_size",  type="int",    action="callback", callback=option_callback)
  parser.add_option("--population_low_size",   type="int",    action="callback", callback=option_callback)
  parser.add_option("--min_input_size",        type="int",    action="callback", callback=option_callback)
  parser.add_option("--offset",                type="int",    action="callback", callback=option_callback)
  parser.add_option("--threads",               type="int",    action="callback", callback=option_callback)
  parser.add_option("--name",                  type="string", action="callback", callback=option_callback)
  parser.add_option("--abort_on",              type="string", action="callback", callback=option_callback)
  parser.add_option("--accuracy_target",       type="float",  action="callback", callback=option_callback)
  (options, args) = parser.parse_args()
  if len(args)!=1:
    parser.print_usage()
    sys.exit(1)
  if options.check:
    tunerconfig.applypatch(tunerconfig.patch_check)
  if options.debug:
    tunerconfig.applypatch(tunerconfig.patch_debug)
  if options.n:
    tunerconfig.applypatch(tunerconfig.patch_n(options.n))
  config.benchmark=args[0]
  recompile()
  autotune(config.benchmark)




