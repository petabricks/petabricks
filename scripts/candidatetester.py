#!/usr/bin/python
from configtool import ConfigFile, defaultConfigFile
import pbutil
import tempfile, os, math, warnings, random, sys, subprocess, time
import shutil
import storagedirs
import tunerwarnings 
import platform
import numpy
from storagedirs import timers
from scipy import stats
from tunerconfig import config
from tunerwarnings import ComparisonFailed, InconsistentOutput
warnings.simplefilter('ignore', DeprecationWarning)

def getMemoryLimitArgs():
  limit = []
  if platform.machine() == 'x86_64' and platform.system() == 'Linux':
    mi = filter(lambda x: x[0:9]=="MemTotal:", open("/proc/meminfo"))
    assert len(mi)==1
    v,u = mi[0][9:].strip().split(' ')
    v=int(v)
    assert u=="kB"
    limit=['--max-memory=%d'%int(v*1024*config.memory_limit_pct)]
  global getMemoryLimitArgs
  getMemoryLimitArgs = lambda: limit
  return limit

class NoMutators(Exception):
  '''Exception thrown when a mutation doesn't exist'''
  pass

class InputGenerationException(Exception):
  def __init__(self, testNumber):
    self.testNumber=testNumber

class CrashException(Exception):
  def __init__(self, testNumber, n, candidate, cmd):
    self.testNumber=testNumber
    self.n = n
    self.candidate=candidate
    self.cmd=cmd
  def debugpause(self):
    if config.pause_on_crash:
      print '-'*60
      print 'ERROR: Candidate algorithm crashed!'
      print '-'*60
      print "%s crashed on test %d for input size %d" % (str(self.candidate),self.testNumber, self.n)
      print 'cmd:', ' '.join(self.cmd)
      print 'set config.pause_on_crash=False to disable this message'
      print '-'*60
      print
      raw_input('press any key to continue')

def debug_logcmd(cmd):
  #print ' '.join(cmd)
  pass

class Results:
  '''stores a list of (timing|accuracy) test results and collects statistics'''
  def __init__(self, results=[]):
    self.realResults=[]         #listof(float)
    self.timeoutResults=[]      #listof(float)
    self.interpolatedResults=[] #listof(float)
    self.distribution = None

  def __repr__(self):
    v=[]
    v.extend(map(lambda x: "%.6f"%x,  self.realResults))
    v.extend(map(lambda x: ">%.6f"%x, self.timeoutResults))
    return ', '.join(v)

  def __str__(self):
    if len(self)==0:
      return '???'
    return "%.6f(+-%.6f)" % self.interval(config.display_confidence)

  def strdelta(a, b):
    am, ad = a.interval(config.display_confidence)
    bm, bd = b.interval(config.display_confidence)
    return "%.2fx to %.2fx" % (
        max(0.0, am+ad)/max(1e-32, bm-bd),
        max(0.0, am-ad)/max(1e-32, bm+bd)
        )

  def interval(self, conf):
    md=self.meanDistribution()
    a=md.ppf((1.0-conf)/2.0)
    mean=md.ppf(0.5)
    delta = mean-a
    return mean, delta

  def reasonableLimit(self):
    dd=self.dataDistribution()
    return dd.ppf(config.limit_conf_pct)*config.limit_multiplier

  def __len__(self):
    return len(self.realResults)+len(self.timeoutResults)

  def add(self, p):
    self.realResults.append(p)
    self.reinterpolate();

  def addTimeout(self, p):
    assert p is not None
    self.timeoutResults.append(p)
    self.reinterpolate();

  def numTimeouts(self):
    return len(self.timeoutResults)

  def reinterpolate(self):
    '''recreate interpolatedResults from realResults and timeoutResults'''
    self.interpolatedResults = list(self.realResults)
    mkdistrib = lambda: stats.norm(numpy.mean(self.interpolatedResults), numpy.std(self.interpolatedResults))
    if len(self.interpolatedResults) == 0:
      '''all tests timed out, seed with double the average timeout'''
      self.interpolatedResults.append(sum(self.timeoutResults)/len(self.timeoutResults)*2.0)
    if len(self.interpolatedResults) == 1:
      '''only 1 test, use prior stddev'''
      self.distribution = stats.norm(self.interpolatedResults[0], self.interpolatedResults[0]*config.prior_stddev_pct)
    else:
      '''estimate stddev with least squares'''
      self.distribution = mkdistrib()
    for p in sorted(self.timeoutResults):
      '''now lets estimate values for the points that timed out'''
      '''new points are assigned the median value above their timeout'''
      self.interpolatedResults.append(max(p, min(self.distribution.isf(self.distribution.sf(p)/2.0), p*4)))
      self.distribution = mkdistrib()
 
  def dataDistribution(self):
    '''estimated probability distribution of a single timing run'''
    return self.distribution

  def meanDistribution(self):
    '''estimated probability distribution of the real mean value'''
    return stats.norm(self.mean(), math.sqrt(self.meanVariance()))

  def mean(self):
    assert len(self)>0
    m,v=self.distribution.stats()
    return m
  
  def min(self):
    assert len(self)>0
    return min(self.interpolatedResults)

  def variance(self):
    assert len(self)>0
    m,v=self.distribution.stats()
    return v

  def meanVariance(self, offset=0):
    '''square of stderror'''
    assert len(self)>0
    return self.variance()/float(len(self)+offset)

  def estimatedBenifitNextTest(self, offset=1):
    return self.meanVariance()-self.meanVariance(offset)

  def ttest(self, that):
    '''estimate probability P(data | self and that have same mean)'''
    assert len(self)>0
    assert len(that)>0
    return stats.ttest_ind(self.interpolatedResults, that.interpolatedResults, 0)[1]

  def diffChance(self, that):
    '''estimate probability self and that have different means'''
    return 1.0 - self.ttest(that)

  def sameChance(self, that):
    '''estimate probability self and that have means within config.same_threshold_pct'''
    denom = min(self.mean(), that.mean())
    dd=stats.norm((self.mean()-that.mean())/denom, math.sqrt(self.meanVariance()+that.meanVariance())/denom)
    return dd.cdf(config.same_threshold_pct/2.0)-dd.cdf(-config.same_threshold_pct/2.0)

  def last(self):
    if self.realResults:
      return self.realResults[-1]

class ResultsDB:
  '''stores many Results for different input sizes'''
  def __init__(self, metric, vals=dict()):
    self.metric = metric
    self.nToResults = dict(vals)

  def __getitem__(self, n):
    try:
      return self.nToResults[n]
    except:
      self.nToResults[n]=Results()
      return self[n]
  
  def __setitem__(self, n, v):
    self.nToResults[n] = v

  def __repr__(self):
    return "ResultsDB(%s, {"%repr(self.metric)+\
           ', '.join(map(lambda x: "%d: %s"%(x[0], repr(x[1])), self.nToResults.iteritems()))+\
           "})"

  def totalTests(self):
    return sum(map(len, self.nToResults.values()))

  def keys(self):
    return self.nToResults.keys()

class Candidate:
  nextCandidateId=0
  '''A candidate algorithm in the population'''
  def __init__(self, cfg, infoxml, mutators=[]):
    self.config      = ConfigFile(cfg)
    self.metrics     = [ResultsDB(x) for x in config.metrics]
    self.mutators    = list(mutators)
    self.cid         = Candidate.nextCandidateId
    self.infoxml     = infoxml
    self.lastMutator = None
    self.outputdir   = storagedirs.candidate(self.cid)
    self.C           = 10    # exploration/exploitation trade-off in the DMAB algorithm
    Candidate.nextCandidateId += 1

  def __str__(self):
    return "Candidate%d"%self.cid

  def clone(self):
    '''
    this creates ResultDB *references*, not copies
    so new results will be added to both algs
    use clearResults to remove the copies
    '''
    t=Candidate(self.config, self.infoxml, self.mutators)
    for i in xrange(len(self.metrics)):
      for n in self.metrics[i].keys():
        t.metrics[i][n] = self.metrics[i][n]
    return t

  def clearResultsAbove(self, val):
    for i in xrange(len(self.metrics)):
      for n in self.metrics[i].keys():
        if n>=val:
          self.metrics[i][n] = Results()

  def clearResults(self):
    for i in xrange(len(self.metrics)):
      for n in self.metrics[i].keys():
        self.metrics[i][n] = Results()

  def addMutator(self, m):
    self.mutators.append(m)

  ''' Selects a mutator according to the Upper Confidence Bound algorithm '''
  def upperConfidenceBoundMutate(self, n, mutatorLog = []):
    # compute the total number of mutations
    totalMutations = len(mutatorLog) + len(self.mutators)

    bestScore = -1 # scores are guaranteed to be non-negative
    bestMutator = None
    for m in self.mutators:

      m.timesSelected = 1
      for m2 in mutatorLog:
        if m == m2:
          m.timesSelected += 1
      
      score = m.computeRocScore(mutatorLog) + self.C*math.sqrt(2.0*math.log(totalMutations) / m.timesSelected)
      if score > bestScore:
        bestScore = score
        bestMutator = m

    self.lastMutator = bestMutator
    self.lastMutator.mutate(self, n)


  def mutate(self, n, minscore=None):
    if minscore is not None:
      opts=filter(lambda x: x.score>minscore, self.mutators)
      if opts:
        self.lastMutator=random.choice(opts)
      else:
        raise NoMutators()
    else:
      self.lastMutator=random.choice(self.mutators)
    self.lastMutator.mutate(self, n)

  def reasonableLimit(self, n):
    return self.metrics[config.timing_metric_idx][n].reasonableLimit()

  def resultsStr(self, n, baseline=None):
    s=[]
    t=str
    if config.print_raw:
      t=repr
    for i, m in enumerate(self.metrics):
      s.append("%s: %s" % (config.metrics[i], t(m[n])))
    return ', '.join(s)

  def numTests(self, n):
    return len(self.metrics[config.timing_metric_idx][n])

  def numTimeouts(self, n):
    return self.metrics[config.timing_metric_idx][n].numTimeouts()
  
  def numTotalTests(self):
    return self.metrics[config.timing_metric_idx].totalTests()

  def hasAccuracy(self, n, target):
    return self.metrics[config.accuracy_metric_idx][n].mean() >= target

  def cfgfile(self):
    cf=os.path.join(self.outputdir,'config')
    self.config.save(cf)
    return cf

  def rmfiles(self):
    for f in ('config', 'stats', 'stats_raw'):
      f=os.path.join(self.outputdir,f)
      if os.path.isfile(f):
        os.unlink(f)
    os.rmdir(self.outputdir)

  def writestats(self, n, filename=None):
    if filename is None:
      filename=os.path.join(self.outputdir,'stats')
    first=not os.path.isfile(filename)
    s=open(filename, 'a')
    if first:
      s.write("#input, ")
      for m in config.metrics:
        s.write("%s_mean, %s_stddev, %s_stderr, %s_ci, "%(m, m, m, m))
      s.write("\n")
    s.write("%6d, "%n)
    for m in self.metrics:
      avg,ci = m[n].interval(config.display_confidence)
      sd = math.sqrt(m[n].variance())
      se = math.sqrt(m[n].meanVariance())
      s.write("%.8f, %.8f, %.8f, %.8f, "%(avg,sd,se,ci))
    s.write("\n")
    s.close()

class Input:
  def __init__(self, pfx):
    self.pfx=pfx
    self.outputHash=None
    self.firstCandidate=None

class CandidateTester:
  def __init__(self, app, n, args=[]):
    self.app = app
    self.bin = pbutil.benchmarkToBin(app)
    self.n = n
    self.cmd = [
        self.bin,
        "--time",
        #"--trials=1",
        "--accuracy",
        "--offset=%d"%config.offset
      ]
    #remove default options
    self.cmd = filter(lambda x: x not in ["--offset=0", "--trials=1"], self.cmd)
    self.cmd.extend(args)
    self.args=args
    self.inputs=[]
    self.testCount = 0
    self.timeoutCount = 0
    self.crashCount = 0

  def nextTester(self):
    return CandidateTester(self.app, self.n*2, self.args)
  
  def testN(self, candidate, trials, limit=None):
    for x in xrange(trials - candidate.numTests(self.n)):
      self.test(candidate, limit)

  def getInputArg(self, testNumber):
    if config.use_iogen:
      pfx=storagedirs.inputpfx(self.n, testNumber)
      if len(self.inputs) <= testNumber:
        assert len(self.inputs) == testNumber
        cmd = self.cmd + ['--iogen-create='+pfx, "--n=%d"%self.n]
        debug_logcmd(cmd)
        devnull = open("/dev/null", "w")
        try:
          if subprocess.call(cmd, stdout=devnull, stderr=devnull) != 0:
            raise InputGenerationException(testNumber)
          self.inputs.append(Input(pfx))
        finally:
          devnull.close()
      return ["--iogen-run="+pfx, "--iogen-n=%d"%self.n]
    else:
      return ["--n=%d"%self.n]

  def checkOutputHash(self, candidate, i, value):
    if self.inputs[i].outputHash is None:
      self.inputs[i].outputHash = value
      self.inputs[i].firstCanidate = candidate
    elif self.inputs[i].outputHash != value:
      warnings.warn(InconsistentOutput(self.inputs[i].firstCanidate, candidate, self.inputs[i].pfx))

  def test(self, candidate, limit=None):
    self.testCount += 1
    cfgfile = candidate.cfgfile()
    testNumber = candidate.numTests(self.n)
    if testNumber>=config.max_trials:
      warnings.warn(tunerwarnings.TooManyTrials(testNumber+1))
    cmd = list(self.cmd)
    cmd.append("--config="+cfgfile)
    cmd.extend(timers.inputgen.wrap(lambda:self.getInputArg(testNumber)))
    if limit is not None:
      cmd.append("--max-sec=%f"%limit)
    cmd.extend(getMemoryLimitArgs())
    try:
      debug_logcmd(cmd)
      if config.check:
        results = timers.testing.wrap(lambda: pbutil.executeRun(cmd+['--hash'], config.metrics+['outputhash']))
        self.checkOutputHash(candidate, testNumber, results[-1]['value'])
        del results[-1]
      else:
        results = timers.testing.wrap(lambda: pbutil.executeRun(cmd, config.metrics))
      for i,result in enumerate(results):
        if result is not None:
          candidate.metrics[i][self.n].add(result['average'])
      return True
    except pbutil.TimingRunTimeout:
      assert limit is not None
      warnings.warn(tunerwarnings.ProgramTimeout(candidate, self.n, limit))
      candidate.metrics[config.timing_metric_idx][self.n].addTimeout(limit)
      self.timeoutCount += 1
      return False
    except pbutil.TimingRunFailed, e:
      self.crashCount += 1
      raise CrashException(testNumber, self.n, candidate, cmd)

  def race(self, candidatea, candidateb, limit=None):
    self.testCount += 1
    cfgfilea = candidatea.cfgfile()
    cfgfileb = candidateb.cfgfile()
    cmd = list(self.cmd)
    cmd.extend(timers.inputgen.wrap(lambda:self.getInputArg(0)))
    if limit is not None:
      cmd.append("--max-sec=%f"%limit)
    cmd.extend(getMemoryLimitArgs())
    try:
      debug_logcmd(cmd)
      resulta,resultb = timers.testing.wrap(lambda: pbutil.executeRaceRun(cmd, cfgfilea, cfgfileb))
      best = min(resulta['timing'], resultb['timing'])
      for candidate, result in [(candidatea,resulta), (candidateb, resultb)]:
        if result['timing'] < 2**31:
          for i,metric in enumerate(config.metrics):
            candidate.metrics[i][self.n].add(result[metric])
        else:
          candidate.metrics[config.timing_metric_idx][self.n].addTimeout(best)
      return True
    except pbutil.TimingRunTimeout:
      assert limit is not None
      warnings.warn(tunerwarnings.ProgramTimeout(candidate, self.n, limit))
      candidate.metrics[config.timing_metric_idx][self.n].addTimeout(limit)
      self.timeoutCount += 1
      return False
    except pbutil.TimingRunFailed, e:
      self.crashCount += 1
      raise CrashException(testNumber, self.n, candidate, cmd)
  
  def comparer(self, metricIdx, confidence, maxTests):
    '''return a cmp like function that dynamically runs more tests to improve confidence'''
    def compare(a, b):
      assert a.numTests(self.n)>0
      assert b.numTests(self.n)>0
      if metricIdx != config.timing_metric_idx:
        if len(a.metrics[metricIdx][self.n])==0:
          warnings.warn(tunerwarnings.ComparisonSkipped(self.n, a, b))
          return 0
        if len(b.metrics[metricIdx][self.n])==0:
          warnings.warn(tunerwarnings.ComparisonSkipped(self.n, a, b))
          return 0

      for x in xrange(2*maxTests+1):
        ra=a.metrics[metricIdx][self.n]
        rb=b.metrics[metricIdx][self.n]
        if ra.diffChance(rb) >= confidence:
          # we can eliminate the null hypothesis, just compare
          return config.metric_orders[metricIdx]*cmp(ra.mean(), rb.mean())
        if ra.sameChance(rb) >= confidence:
          return 0
        if ra.estimatedBenifitNextTest() >= rb.estimatedBenifitNextTest() and len(ra)<maxTests:
          self.test(a)
        elif len(rb)<maxTests:
          self.test(b)
        elif len(ra)<maxTests:
          self.test(a)
        else:
          break
      warnings.warn(ComparisonFailed(self.n, a, b))
      return 0
    return compare

  def cleanup(self):
    if config.cleanup_inputs:
      storagedirs.clearInputs();
      self.inputs=[]

if __name__ == "__main__":
  print "TESTING CANDIDATETESTER"
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName('multiply')
  pbutil.compileBenchmarks([benchmark])
  tester = CandidateTester(benchmark, 768)
  try:
    candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)))
    candidate2 = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)))
    candidate2.config['MatrixMultiplyTransposed_0_lvl1_rule']=1
    tester.test(candidate)
    tester.test(candidate)
    tester.test(candidate)
    tester.test(candidate2)
    tester.test(candidate2)
    tester.test(candidate2)
    print candidate.metrics[0]
    print candidate2.metrics[0]
    print str(candidate.metrics[0][768])
    print str(candidate2.metrics[0][768])
    c=tester.comparer(0, .95, 25)
    print c(candidate, candidate2)
    print candidate.metrics[0][768].sameChance(candidate2.metrics[0][768])
    print candidate.metrics[0][768].diffChance(candidate2.metrics[0][768])
    print candidate.metrics[0]
    print candidate2.metrics[0]
  finally:
    tester.cleanup()





