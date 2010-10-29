#!/usr/bin/python
import itertools, random, subprocess, os, sys, time, warnings
import pbutil, mutators
import logging
import storagedirs 
import candidatetester
import tunerconfig
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester
from mutators import MutateFailed
from traininginfo import TrainingInfo
from tunerconfig import config
from tunerwarnings import InitialProgramCrash,ExistingProgramCrash,NewProgramCrash,TargetNotMet
from storagedirs import timers
import tunerwarnings
from pprint import pprint

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

  def countMutators(self, minscore):
    if minscore is None:
      return sum(map(lambda x: len(x.mutators), self.members))
    else:
      return sum(map(lambda x: len(filter(lambda m: m.score>minscore, x.mutators)), self.members))

  def randomMutation(self, maxpopsize=None, minscore=None):
    '''grow the population using cloning and random mutation'''
    originalPop = list(self.members)
    totalMutators = self.countMutators(minscore)
    tries = float(totalMutators)*config.mutations_per_mutator
    while tries>0:
      check_timeout()
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
          c.mutate(self.inputSize(), minscore)
          break
        except MutateFailed:
          if z==config.mutate_retries-1:
            warnings.warn(tunerwarnings.MutateFailed(p, z, self.inputSize()))
          continue
        except candidatetester.NoMutators:
          if self.countMutators(minscore)>0:
            continue
          else:
            return tries

      if c.config in self.triedConfigs:
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
        c.lastMutator.result('fail')
        warnings.warn(NewProgramCrash(e))
    if len(originalPop)<len(self.members):
      logging.info("added "+', '.join(map(str,set(self.members)-set(originalPop))))
    return tries
  
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
    if same:
      child.lastMutator.result('same')
    else:
      child.lastMutator.result('worse')
    return False
  
  def inputSize(self, roundOffset=0):
    return self.testers[-1 - roundOffset].n

  def onMembersChanged(self, endOfRound):
    if config.candidatelog:
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

    best=self.markBestN(self.members, popsize, config.timing_metric_idx)
    if isLast:
      storagedirs.cur.markBest(best[0].cid, self.inputSize(), None)
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
    try:
      self.roundNumber += 1
      self.triedConfigs = set(map(lambda x: x.config, self.members))
      self.removed=[]
      self.notadded=[]
      self.test(config.min_trials)
      if len(self.members):
        for z in xrange(config.rounds_per_input_size):
          self.randomMutation(config.population_high_size)
          self.prune(config.population_low_size, False)
        while self.countMutators(config.bonus_round_score)>0:
          logging.info("bonus round triggered")
          self.randomMutation(config.population_high_size, config.bonus_round_score)
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

def addMutators(candidate, info, ignore=None, weight=1.0):
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
    logging.info("added Mutator " + transform + "/" + ac['name'] + " => AlgChoice")
    candidate.addMutator(mutators.RandAlgMutator(transform, ac['number'], mutators.config.first_lvl, weight=weight))
    for a in info.rulesInAlgchoice(ac['number']):
      candidate.addMutator(mutators.AddAlgLevelMutator(transform, ac['number'], a, weight=weight))
  for ta in info.tunables():
    name = ta['name']
    l=int(ta['min'])
    h=int(ta['max'])
    ms=[]
    if 'accuracy' in ta['type']:
      #hack to support what the old autotuner did
      l+=1

    if ta['type'] in config.lognorm_tunable_types:
      ms.append(mutators.LognormRandCutoffMutator(name, weight=weight))
    elif ta['type'] in config.uniform_tunable_types:
      ms.append(mutators.UniformRandMutator(name, l, h, weight=weight))
    elif ta['type'] in config.autodetect_tunable_types:
      if l <= 1 and h > 2**16:
        ms.append(mutators.LognormRandCutoffMutator(name, weight=weight))
      else:
        ms.append(mutators.UniformRandMutator(name, l, h, weight=weight))
    elif ta['type'] in config.lognorm_array_tunable_types:
      ms.append(mutators.LognormTunableArrayMutator(name, l, h, weight=weight))
      ms[-1].reset(candidate)
    elif ta['type'] in config.ignore_tunable_types:
      pass
    else:
      warnings.warn(tunerwarnings.UnknownTunableType(name, ta['type']))
    
    for m in ms:
      if 'accuracy' in ta['type']:
        m.accuracyHint = 1
      logging.info("added Mutator " + transform + "/" + name + " => " + m.__class__.__name__)
      candidate.addMutator(m)
  
  for sub in info.calls():
    addMutators(candidate, sub, ignore, weight/2.0)

def autotuneInner(benchmark):
  if config.debug:
    logging.basicConfig(level=logging.DEBUG)
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  main = mainname([pbutil.benchmarkToBin(benchmark)])
  tester = CandidateTester(benchmark, 1)
  try:
    candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml.transform(main))
    baseline = None
    addMutators(candidate, infoxml.globalsec())
    addMutators(candidate, infoxml.transform(main))
    candidate.addMutator(mutators.MultiMutator(2))
    pop = Population(candidate, tester, baseline)

    if not config.delete_output_dir:
      storagedirs.cur.dumpConfig()
      storagedirs.cur.dumpGitStatus()
      storagedirs.cur.saveFile(pbutil.benchmarkToInfo(benchmark))
      storagedirs.cur.saveFile(pbutil.benchmarkToBin(benchmark))

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
      while pop.inputSize() < config.max_input_size:
        pop.generation()
        stats.writerow((pop.roundNumber,
                        pop.inputSize(),
                        timers.total.total(),
                        timers.total.lap(),
                        timers.testing.lap(),
                        timers.inputgen.lap())+pop.stats())
        pop.nextInputSize()
      for z in xrange(config.final_rounds):
        pop.generation()
        stats.writerow((pop.roundNumber,
                        pop.inputSize(),
                        timers.total.total(),
                        timers.total.lap(),
                        timers.testing.lap(),
                        timers.inputgen.lap())+pop.stats())
    except TrainingTimeout:
      pass
    timers.total.stop()

    #check to make sure we did something:
    if pop.firstRound:
      warnings.warn(tunerwarnings.AlwaysCrashes())
  finally:
    at = storagedirs.getactivetimers()
    if len(at):
      storagedirs.openCsvStats("timers", at.keys()).writerow(at.values())
    tester.cleanup()

def autotune(benchmark):
  storagedirs.callWithLogDir(lambda: autotuneInner(benchmark),
                             config.output_dir,
                             config.delete_output_dir)

def regression_check(benchmark):
  tunerconfig.applypatch(tunerconfig.patch_regression)
  storagedirs.callWithLogDir(lambda: autotuneInner(benchmark),
                             config.output_dir,
                             config.delete_output_dir)

def option_callback(option, opt, value, parser):
  opt=str(option).split('/')[0]
  while opt[0]=='-':
    opt=opt[1:]
  assert hasattr(config, opt)
  setattr(config, opt, value)

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
  parser.add_option("--offset",                type="int",    action="callback", callback=option_callback)
  parser.add_option("--name",                  type="string", action="callback", callback=option_callback)
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
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  config.benchmark=pbutil.normalizeBenchmarkName(config.benchmark)
  pbutil.compileBenchmarks([config.benchmark])
  autotune(config.benchmark)




