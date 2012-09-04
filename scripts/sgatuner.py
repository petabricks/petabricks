#!/usr/bin/env python
from __future__ import with_statement
import progress
import itertools, random, subprocess, os, sys, time, warnings
import pbutil, mutators
import logging
import storagedirs
import candidatetester
import shutil
import tunerconfig
import configtool
import traceback
import re
import math
import time
from configtool import defaultConfigFile
from candidatetester import Candidate, CandidateTester
from mutators import MutateFailed
from traininginfo import TrainingInfo
from tunerconfig import config, option_callback
from tunerwarnings import ExistingProgramCrash,NewProgramCrash,TargetNotMet,SmallInputProgramCrash
from storagedirs import timers
from highlevelconfig import HighLevelConfig
import tunerwarnings
from pprint import pprint
import pdb

first = lambda x: x[0]
second = lambda x: x[1]

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

class MutationLogItem:
  def __init__(self, n):
    self.name = n
  def __str__(self):
    return self.name
  @property
  def short(self):
    return self.name[0].upper()

class MutationLog:
  random    = MutationLogItem("random")
  crossover = MutationLogItem("crossover")
  mutate    = MutationLogItem("mutate")
  elitism   = MutationLogItem("elitism")
  hillclimb = MutationLogItem("hillclimb")
  seed      = MutationLogItem("seed")

class Population:
  def __init__(self, initial, tester, hlconfig):
    self.initial  = initial
    self.members  = [initial.clone()]
    self.best     = None
    self.notadded = []
    self.removed  = []
    self.failed   = set()
    self.testers  = [tester]
    self.roundNumber = -1
    self.firstRound = True
    self.hlconfig = hlconfig
    if config.candidatelog:
      self.candidatelog = storagedirs.openCsvStats("candidatelog",
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
    self.members[0].log_mutation(MutationLog.seed)

  def test(self, count):
    '''test each member of the pop count times'''
    self.failed=set()
    tests = []
    for z in xrange(count):
      tests.extend(self.members)
    #random.shuffle(tests)
    with progress.Scope(cnt=len(tests)) as pr:
      for m in tests:
        check_timeout()
        if m not in self.failed and m.numTests(self.inputSize())<config.max_trials:
          try:
            self.testers[-1].test(candidate=m, limit=self.reasonableLimit())
          except candidatetester.CrashException, e:
            if m.numTotalTests()==0:
              if self.inputSize() < config.min_input_size_nocrash:
                warnings.warn(SmallInputProgramCrash(e))
              else:
                warnings.warn(NewProgramCrash(e))
            else:
              warnings.warn(ExistingProgramCrash(e))
            self.failed.add(m)
            self.members.remove(m)
          pr()

  def reasonableLimit(self):
    if config.accuracy_target is not None:
      members = filter(lambda x: x.hasAccuracy(self.inputSize(), config.accuracy_target), self.members)
    else:
      members = list(self.members)
    limits = filter(lambda x: x is not None,
                map(lambda x: x.reasonableLimit(self.inputSize()), members))
    if len(limits):
      return min(limits)
    else:
      return None
   
  def countMutators(self, mutatorFilter=lambda m: True):
    return sum(map(lambda x: len(filter(mutatorFilter, x.mutators)), self.members))

  def genConfigRandom(self):
    c=self.members[0].clone()
    c.reset_mutation_log()
    self.hlconfig.randomize(c.config, self.inputSize())
    c.log_mutation(MutationLog.random)
    return c

  def genConfigCrossover(self):
    a=self.tournament_select()
    b=self.tournament_select()
    c=a.clone()
    self.hlconfig.crossover(a.config, b.config, c.config)
    c=c.clone() # snapshot config to support hillclimb
    self.hlconfig.mutateChance(config.mutation_rate, c.config, self.inputSize())
    c.log_mutation(MutationLog.crossover)
    return c

  def genConfigMutate(self):
    c=self.tournament_select().clone()
    self.hlconfig.mutateChance(config.mutation_rate, c.config, self.inputSize())
    c.log_mutation(MutationLog.mutate)
    return c

  def genConfigHillclimb(self, p):
    c = p.clone()
    self.hlconfig.hillclimb(p.startconfig, p.config, c.config, self.inputSize())
    c.log_mutation(MutationLog.hillclimb)
    return c
  
  def randomMutation(self, maxpopsize=None, mutatorFilter=lambda m: True):
    '''grow the population using cloning and random mutation'''
    originalPop = list(self.members)
    totalMutators = self.countMutators(mutatorFilter)
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
      try:
        c=p.cloneAndMutate(self.inputSize(), mutatorFilter=mutatorFilter)
      except candidatetester.NoMutators:
        if self.countMutators(mutatorFilter)>0:
          continue
        else:
          return tries
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
      
  def fitness(self, x):
    n = self.inputSize()
    if x.hasAccuracy(n, config.accuracy_target):
      return False, x.performance(n), -x.accuracy(n),
    else:
      return True, -x.accuracy(n), x.performance(n)

  def sortMembers(self):
    self.members.sort(key=self.fitness)
    if self.members:
      self.best = self.members[0]
    else:
      self.best = None
      
  def onMembersChanged(self, endOfRound):
    if config.candidatelog and len(self.members) and 0:    # Connelly: Disable this logging method
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
    if config.accuracy_target is None:
      return True
    accTarg=max(self.accuracyTargets())
    t = filter(lambda x: x.hasAccuracy(self.inputSize(), accTarg), self.members)
    return len(t)

  def printPopulation(self):
    print "round n = %d"%self.inputSize()
    if self.members:
      row = self.members[0].resultsTable(self.inputSize())
      headers = map(first, self.members[0].resultsTable(self.inputSize()))
      row = map(second, row)
      lens = map(max, zip(map(len, row), map(len, headers)))
      rows = [row]
      for m in self.members[1:]:
        row = map(second, m.resultsTable(self.inputSize()))
        rows.append(row)
        lens = map(max, zip(map(len, row), lens))
      print ' '.join(map(lambda x: x[0].ljust(x[1]), zip(headers,lens)))
      for row in rows:
        print ' '.join(map(lambda x: x[0].ljust(x[1]), zip(row,lens)))


  def next_population(self):
    n = config.population_size
    zz = lambda pct: int(round(pct*n))
    pop = list()

    pop.extend(self.members[0:zz(config.pop_elitism_pct)])

    for z in xrange(zz(config.pop_mutated_pct)):
      pop.append(self.genConfigMutate())

    for z in xrange(zz(config.pop_crossover_pct)):
      pop.append(self.genConfigCrossover())

    z = zz(config.pop_hillclimb_pct)
    for m in self.members:
      if z>0 and m.mutationlog[-1] in (MutationLog.mutate, MutationLog.crossover, MutationLog.hillclimb):
        pop.append(self.genConfigHillclimb(m))
        z -= 1
    
    while len(pop)<n:
      pop.append(self.genConfigRandom())

    for c in pop[0:zz(config.pop_elitism_pct)]:
      c.log_mutation(MutationLog.elitism)

    hashes=set()
    popunique = list()
    for m in list(pop):
      h = hash(m.config)
      if h not in hashes:
        hashes.add(h)
        popunique.append(m)
    return popunique

  def tournament_select(self):
    if len(self.members)<=config.tournament_size:
      return self.members[0]
    return self.members[min(random.sample(range(len(self.members)), config.tournament_size))]

  def generation(self):
    with progress.Scope('autotuning %s generation %d, input size %d' % (config.benchmark, self.roundNumber+1, self.inputSize())):
      try:
        #os.system("find /tmp -name 'OCL*.so' -user mangpo -maxdepth 1 -delete")
        self.roundNumber += 1
        self.removed  = []
        self.notadded = []
        self.failed   = set()
    
        self.members = self.next_population()

        self.test(config.min_trials)
        self.failed = self.failed.union(set(filter(lambda x: x.numTests(self.inputSize())<=x.numTimeouts(self.inputSize()),
                                  self.members)))
        self.members = filter(lambda x: x.numTests(self.inputSize())> x.numTimeouts(self.inputSize()), self.members)

        if len(self.members):
          self.firstRound=False
        elif self.firstRound and len(self.failed) and config.min_input_size_nocrash>=self.inputSize():
          self.members = list(self.failed)
          if config.print_log:
            print "skip generation n = ",self.inputSize(),"(program run failed)"
        else:
          warnings.warn(tunerwarnings.AlwaysCrashes())
        
        self.sortMembers()

        if config.print_log:
          self.printPopulation()
        
        if not config.delete_output_dir:
          for m in self.members+self.removed:
            m.writestats(self.inputSize())

      except candidatetester.InputGenerationException, e:
        if e.testNumber==0 and self.inputSize()<=config.min_input_size_nocrash:
          if config.print_log:
            print "skip generation n = ",self.inputSize(),"(input generation failure)"
            print e
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

# # Identify tunables that should be grouped into arrays for mutators that act
# # on arrays of values rather than scalars
# def groupTunables(transformName, tunables):
#   names = map(lambda x: x['name'], tunables)
#   tunableSets = [] # return value
#   tunableIndex = {} # index to unique tunables by variable name
#   for name, tunable in zip(names, tunables):
#     tunable['sizeSpecificFlag'] = tunable['type'] in config.sizespecific_tunable_types
#     m = re.match('^%s_i(\d+)_(\w+)$' % transformName, name)
#     # for now, only group tunables that are optimizable into an array
#     if m and tunable['type'] in config.optimize_tunable_types:
#       (index, varName) = m.group(1, 2)
#       index = int(index)
# #      print "Matched array tunable: %s[%d]" % (varName, index)
# #      print "  Size-specific: %s" % tunable['sizeSpecificFlag']
#       if not varName in tunableIndex:
#         assert(index == 0)
#         tunableIndex[varName] = tunable
#         tunable['arrayFlag'] = True
#         tunable['tname'] = transformName
#         tunable['vname'] = varName
#         tunable['size'] = 1
#         # convert min and max to arrays
#         tunable['min'] = [tunable['min']]
#         tunable['max'] = [tunable['max']]
#         tunableSets.append(tunable)
#       else:
#         # arrayTunable gets the tunable to be returned
#         arrayTunable = tunableIndex[varName]
#         assert(arrayTunable['size'] == index)
# #        print "Updating tunable 'size' from %d to %d" % (arrayTunable['size'], arrayTunable['size'] + 1)
#         arrayTunable['size'] += 1
#         arrayTunable['min'].append(tunable['min'])
#         arrayTunable['max'].append(tunable['max'])
#     else:
# #      print "Adding non-array tunable: %s" % name
#       tunable['arrayFlag'] = False
#       tunable['size'] = 1
#       tunableSets.append(tunable)
#   return tunableSets

def init(benchmark, tester_lambda=None, pop_lambda=None, hlconfig_lambda=None, config_lambda=None):
  if config.debug:
    logging.basicConfig(level=logging.DEBUG)
    config.pause_on_crash = True
  if not config.threads:
    config.threads = pbutil.cpuCount()
  for k in filter(len, config.abort_on.split(',')):
    warnings.simplefilter('error', getattr(tunerwarnings,k))
  if hlconfig_lambda is not None:
    hlconfig = hlconfig_lambda()
  else:
    infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
    hlconfig = HighLevelConfig(infoxml)
  if not config.main:
    if tester_lambda is None and pop_lambda is None and hlconfig_lambda is None:
      config.main = mainname([pbutil.benchmarkToBin(benchmark)])
  if tester_lambda is not None:
    tester = tester_lambda(benchmark, config.min_input_size)
  else:
    tester = CandidateTester(benchmark, config.min_input_size)
  if config_lambda is not None:
    cfg = config_lambda()
  else:
    if config.seed is None:
      cfg = defaultConfigFile(pbutil.benchmarkToBin(tester.app))
    else:
      cfg = configtool.ConfigFile(config.seed)
  candidate = Candidate(cfg)
  if hlconfig_lambda is None:
    if not config.delete_output_dir:
      storagedirs.cur.dumpConfig()
      storagedirs.cur.dumpGitStatus()
      storagedirs.cur.saveFile(pbutil.benchmarkToInfo(benchmark))
      storagedirs.cur.saveFile(pbutil.benchmarkToBin(benchmark))
    if not infoxml.transform(config.main).isVariableAccuracy() and config.accuracy_target:
      logging.info("clearing accuracy_target")
      config.accuracy_target = None
  return candidate, tester, hlconfig

def autotuneInner(benchmark, returnBest=None, tester_lambda=None, pop_lambda=None, hlconfig_lambda=None, config_lambda=None):
  """Function running the autotuning process.
If returnBest is specified, it should be a list. The best candidate found will 
be added to that list"""
  with progress.Scope("autotuning "+benchmark,
     config.rounds_per_input_size*math.log(config.max_input_size,2)+config.final_rounds) as pr:
    config.benchmark = benchmark
    candidate, tester, hlconfig = init(benchmark, tester_lambda, pop_lambda, hlconfig_lambda, config_lambda)
    try:
      if pop_lambda is not None:
        pop = pop_lambda(candidate, tester, hlconfig)
      else:
        pop = Population(candidate, tester, hlconfig)

      stats = storagedirs.openCsvStats("roundstats", 
          ("round",
           "input_size",
           "cumulative_sec",
           "incremental_sec",
           "testing_sec",
           "inputgen_sec")+pop.statsHeader())
      timers.total.start()
      config.end_time = time.time() + config.max_time
      def generation():
        pop.generation()
        pr()
        stats.writerow((pop.roundNumber,
                        pop.inputSize(),
                        timers.total.total(),
                        timers.total.lap(),
                        timers.testing.lap(),
                        timers.inputgen.lap())+pop.stats())
      try:
        while pop.inputSize() < config.max_input_size:
          for z in xrange(config.rounds_per_input_size):
            generation()
          pop.nextInputSize()
        for z in xrange(config.final_rounds):
          generation()
      except TrainingTimeout:
        pass
      timers.total.stop()

      #check to make sure we did something:
      if pop.firstRound:
        warnings.warn(tunerwarnings.AlwaysCrashes())
        
      logging.info("TODO: using acc target: "+str(config.accuracy_target))
      return pop.best
    except:
      traceback.print_exc()     # Connelly: Print exceptions (are not otherwise displayed...not sure why)
      raise
    finally:
      if pop.best and config.output_cfg:
        print pop.best.cfgfile(),"=>" , config.output_cfg
        shutil.copyfile(pop.best.cfgfile(), config.output_cfg)
      if pop.best and returnBest is not None:
        returnBest.append(pop.best)
      at = storagedirs.getactivetimers()
      if len(at):
        storagedirs.openCsvStats("timers", at.keys()).writerow(at.values())
      if tester and hasattr(tester, 'cleanup'):     # Connelly: only call if has cleanup attr
        tester.cleanup()

def autotune(benchmark, returnBest=None, tester_lambda=None, pop_lambda=None, hlconfig_lambda=None, config_lambda=None):
  return storagedirs.callWithLogDir(lambda: autotuneInner(benchmark, returnBest, tester_lambda, pop_lambda, hlconfig_lambda, config_lambda),
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

def main(tester_lambda=None, pop_lambda=None, hlconfig_lambda=None, config_lambda=None):
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
  parser.add_option("--population_size",       type="int",    action="callback", callback=option_callback)
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
  if tester_lambda is None and pop_lambda is None and hlconfig_lambda is None:
    recompile()
  autotune(config.benchmark, None, tester_lambda, pop_lambda, hlconfig_lambda, config_lambda)

if __name__ == '__main__':
  main()
  
