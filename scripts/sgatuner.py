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

class TrainingTimeout:
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
    self.firstRound = True
  
  def test(self, count):
    '''test each member of the pop count times'''
    self.failed=set()
    tests = []
    for z in xrange(count):
      tests.extend(self.members)
    random.shuffle(tests)
    for m in tests:
      check_timeout()
      if m not in self.failed:
        try:
          self.testers[-1].test(m)
        except pbutil.TimingRunFailed:
          self.failed.add(m)
          self.members.remove(m)

  def randomMutation(self, maxpopsize=None):  
    '''grow the population using cloning and random mutation'''
    self.notadded=[]
    originalPop = list(self.members)
    triedConfigs = set(map(lambda x: x.config, self.members))
    totalMutators = sum(map(lambda x: len(x.mutators), self.members))
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
          c.mutate(self.inputSize())
          break
        except MutateFailed:
          if z==config.mutate_retries-1:
            logging.debug("mutate failed, try %d of %d" % (z, config.mutate_retries-1))
          continue
      if c.config in triedConfigs:
        continue
      triedConfigs.add(c.config)
      for z in xrange(config.offspring_min_trials):
        self.testers[-1].test(c, limit=p.reasonableLimit(self.inputSize()))
      if self.birthFilter(p,c):
        self.members.append(c)
      else:
        c.rmcfgfile()
        self.notadded.append(c)
    if len(originalPop)<len(self.members):
      logging.debug("added "+', '.join(map(str,set(self.members)-set(originalPop))))
    return tries

  def inputSize(self, roundOffset=0):
    return self.testers[-1 - roundOffset].n

  def markBestN(self, population, n, metric = config.timing_metric_idx):
    '''shrink the population to popsize by removing low scoring candidates'''
    fastCmp = self.testers[-1].comparer(metric, 0.00, 0)
    fullCmp = self.testers[-1].comparer(metric, config.compare_confidence_pct, config.compare_max_trials)
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

  def prune(self, popsize):
    for m in self.members:
      m.keep = False

    self.markBestN(self.members, popsize)

    for accTarg in map(lambda x: x.accuracyTarget(), self.members[0].infoxml.instances):
      t = filter(lambda x: x.hasAccuracy(self.inputSize(), accTarg), self.members)
      if len(t):
        self.markBestN(t, popsize)
      else:
        warnings.warn("no algorithm has accuracy "+str(accTarg)+" for input "+str(self.inputSize()))

    self.removed  = filter(lambda m: not m.keep, self.members)
    self.members  = filter(lambda m: m.keep, self.members)

  def birthFilter(self, parent, child):
    '''called when considering adding child to population'''
    for m in xrange(len(config.metrics)):
      childCmp = self.testers[-1].comparer(m, config.offspring_confidence_pct, config.offspring_max_trials)
      if childCmp(parent, child) > 0:
        return True

  def printPopulation(self):
    print "round n = %d"%self.inputSize()
    for m in self.members:
      if self.baseline is not None:
        self.testers[-1].testN(self.baseline, config.compare_min_trials)
      print "  * ", m, m.resultsStr(self.inputSize(), self.baseline)

  def generation(self):
    try:
      self.test(config.compare_min_trials)
      if len(self.members):
        self.randomMutation(config.population_high_size)
        self.prune(config.population_low_size)
        if config.print_log:
          self.printPopulation()
        self.firstRound=False
      elif self.firstRound and len(self.failed):
        self.members = list(self.failed)
        if config.print_log:
          print "skip generation n = ",self.inputSize(),"(program run failed)"
      else:
        assert False
    except candidatetester.InputGenerationException, e:
      if e.testNumber==0 and self.inputSize()<=16:
        if config.print_log:
          print "skip generation n = ",self.inputSize(),"(input generation failure)"
      else:
        raise

  def nextInputSize(self):
    self.testers[-1].cleanup()
    self.testers.append(self.testers[-1].nextTester())

  def statsHeader(self):
    return '#pop','#removed','#notadded', 'pop_trials_avg','removed_trials_avg','notadded_trials_avg', 

  def stats(self):
    def mean(x):
      if len(x):
        return sum(x)/float(len(x))
      else:
        return 0
    t1 = map(lambda m: m.numTests(self.inputSize()), self.members)
    t2 = map(lambda m: m.numTests(self.inputSize()), self.removed)
    t3 = map(lambda m: m.numTests(self.inputSize()), self.notadded)
    return len(t1),len(t2),len(t3),mean(t1),mean(t2),mean(t3)

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
    logging.debug("added Mutator " + transform + "/" + ac['name'] + " => algchoice")
    candidate.addMutator(mutators.RandAlgMutator(transform, ac['number'], mutators.config.first_lvl, weight=weight))
    for a in info.rulesInAlgchoice(ac['number']):
      candidate.addMutator(mutators.AddAlgLevelMutator(transform, ac['number'], a, weight=weight))
  for ta in info.tunables():
    if ta['type'] in config.lognorm_tunable_types:
      logging.debug("added Mutator " + transform + "/" + ta['name'] + " => lognorm")
      candidate.addMutator(mutators.LognormRandCutoffMutator(ta['name'], weight=weight))
    elif ta['type'] in config.uniform_tunable_types:
      logging.debug("added Mutator " + transform + "/" + ta['name'] + " => randint")
      candidate.addMutator(mutators.UniformRandMutator(ta['name'], int(ta['min']), int(ta['max']), weight=weight))
    elif ta['type'] in config.autodetect_tunable_types:
      logging.debug("added Mutator " + transform + "/" + ta['name'] + " => autodetect")
      if int(ta['min']) <= 1 and int(ta['max']) > 2**16:
        candidate.addMutator(mutators.LognormRandCutoffMutator(ta['name'], weight=weight))
      else:
        candidate.addMutator(mutators.UniformRandMutator(ta['name'], int(ta['min']), int(ta['max']), weight=weight))
    elif ta['type'] in config.ignore_tunable_types:
      pass
    else:
      logging.warn("unknown tunable %s (%s)"%(ta['name'], ta['type']))
  
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
    pop = Population(candidate, tester, baseline)
    stats = storagedirs.openCsvStats("roundstats", ("round", "input_size", "cumulative_sec", "incremental_sec")+pop.statsHeader())
    t1 = time.time()
    t2 = t1
    config.end_time = t1 + config.max_time
    try:
      for roundNumber in xrange(config.max_rounds):
        pop.generation()
        t3 = time.time()
        stats.writerow((roundNumber, pop.inputSize(), t3-t1, t3-t2)+pop.stats())
        t2 = t3
        pop.nextInputSize()
    except TrainingTimeout:
      pass
  finally:
    tester.cleanup()

def autotune(benchmark):
  storagedirs.callWithLogDir(lambda: autotuneInner(benchmark),
                             config.output_dir,
                             config.delete_output_dir)

def regression_check(benchmark):
  tunerconfig.copycfg(tunerconfig.config_regression_check_fast, config)
  storagedirs.callWithLogDir(lambda: autotuneInner(benchmark),
                             config.output_dir,
                             config.delete_output_dir)

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser(usage="usage: sgatuner.py [options] Benchmark")
  parser.add_option("--check",
                    action="store_true", dest="check_fast", default=False,
                    help="check for correctness")
  (options, args) = parser.parse_args()
  if len(args)!=1:
    parser.print_usage()
    sys.exit(1)
  if options.check_fast:
    tunerconfig.copycfg(tunerconfig.config_regression_check_fast, config)
  benchmark=args[0]
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName(benchmark)
  pbutil.compileBenchmarks([benchmark])
  autotune(benchmark)




