#!/usr/bin/python
from configtool import ConfigFile, defaultConfigFile
import pbutil
import tempfile, os, math, warnings, random, sys, subprocess
import shutil
from scipy import stats
from tunerconfig import config
warnings.simplefilter('ignore', DeprecationWarning)

class StorageDirsTemplate:
  def __init__(self, root):
    self.root    = root
    self.configd = os.path.join(root, 'config')
    self.inputd  = os.path.join(root, 'inputs')
    self.resultd = os.path.join(root, 'results')
    self.statsd  = os.path.join(root, 'stats')
    os.mkdir(self.configd)
    os.mkdir(self.resultd)
    os.mkdir(self.statsd)
    os.mkdir(self.inputd)

  def config(self, cid):
    return os.path.join(self.configd, "candidate%05d.cfg" % cid)
  
  def inputpfx(self, size, number):
    return os.path.join(self.inputd, "n%010d_i%02d_" % (size, number))
      
  def clearInputs(self):
    for f in os.listdir(self.inputd):
      os.unlink(os.path.join(self.inputd, f))

storage_dirs = None

def callWithLogDir(fn, root=config.tmpdir, delete=True):
  d = tempfile.mkdtemp(prefix='pbtunerun_', dir=root)
  print d
  global storage_dirs
  storage_dirs = StorageDirsTemplate(d)
  try:
    fn()
  finally:
    if delete:
      shutil.rmtree(d)

def debug_logcmd(cmd):
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
    v.extend(map(lambda x: "%.4f"%x,  self.realResults))
    v.extend(map(lambda x: ">%.4f"%x, self.timeoutResults))
    return ', '.join(v)

  def __str__(self):
    if len(self)==0:
      return '???'
    return "%.4f(+-%.4f)" % self.interval(config.display_confidence)

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
    return len(self.interpolatedResults)

  def add(self, p):
    p=min(config.max_result, p)
    self.realResults.append(p)
    self.reinterpolate();

  def addTimeout(self, p):
    self.timeoutResults.append(p)
    self.reinterpolate();

  def reinterpolate(self):
    '''recreate interpolatedResults from realResults and timeoutResults'''
    self.interpolatedResults = list(self.realResults)
    mkdistrib = lambda: stats.norm(*stats.norm.fit(self.interpolatedResults))
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
    if min(self.interpolatedResults) == max(self.interpolatedResults):
      return stats.norm(self.interpolatedResults[0], 0)
 
  def dataDistribution(self):
    '''estimated probability distribution of a single timing run'''
    return self.distribution

  def meanDistribution(self):
    '''estimated probability distribution of the real mean value'''
    return stats.norm(self.mean(), math.sqrt(self.meanVariance()))

  def mean(self):
    assert len(self)>0
    return self.distribution.stats('m')

  def variance(self):
    assert len(self)>0
    return self.distribution.stats('v')

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

  def keys(self):
    return self.nToResults.keys()

class Candidate:
  nextCandidateId=0
  '''A candidate algorithm in the population'''
  def __init__(self, cfg, infoxml, mutators=[]):
    self.config  = ConfigFile(cfg)
    self.metrics = [ResultsDB(x) for x in config.metrics]
    self.mutators = list(mutators)
    self.cid = Candidate.nextCandidateId
    self.infoxml = infoxml
    self._cfgfile = storage_dirs.config(self.cid)
    Candidate.nextCandidateId+=1

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

  def mutate(self, n):
    random.choice(self.mutators).mutate(self, n)

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

  def cfgfile(self):
    self.config.save(self._cfgfile)
    return self._cfgfile

  def rmcfgfile(self):
    try:
      os.unlink(self._cfgfile)
    except:
      pass

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

  def nextTester(self):
    return CandidateTester(self.app, self.n*2, self.args)
  
  def testN(self, candidate, trials, limit=None):
    for x in xrange(trials - len(candidate.metrics[config.timing_metric_idx][self.n])):
      self.test(candidate, limit)

  def getInputArg(self, testNumber):
    if config.use_iogen:
      pfx=storage_dirs.inputpfx(self.n, testNumber)
      if len(self.inputs) <= testNumber:
        assert len(self.inputs) == testNumber
        cmd = self.cmd + ['--iogen-create='+pfx, "--n=%d"%self.n]
        debug_logcmd(cmd)
        subprocess.check_call(cmd)
        self.inputs.append(Input(pfx))
      return "--iogen-run="+pfx
    else:
      return "--n=%d"%self.n

  def checkOutputHash(self, candidate, i, value):
    if self.inputs[i].outputHash is None:
      self.inputs[i].outputHash = value
      self.inputs[i].firstCanidate = candidate
    elif self.inputs[i].outputHash != value:
      warnings.warn("difference detected between %s and %s on input %s" % (
           str(self.inputs[i].firstCanidate),
           str(candidate),
           str(self.inputs[i].pfx)
        ))

  def test(self, candidate, limit=None):
    cfgfile = candidate.cfgfile()
    testNumber = len(candidate.metrics[config.timing_metric_idx][self.n])
    cmd = list(self.cmd)
    cmd.append("--config="+cfgfile)
    cmd.append(self.getInputArg(testNumber))
    if limit is not None:
      cmd.append("--max-sec=%f"%limit)
    try:
      debug_logcmd(cmd)
      if config.check:
        results = pbutil.executeRun(cmd+['--hash'], config.metrics+['outputhash'])
        self.checkOutputHash(candidate, testNumber, results[-1]['value'])
        del results[-1]
      else:
        results = pbutil.executeRun(cmd, config.metrics)
      for i,result in enumerate(results):
        if result is not None:
          candidate.metrics[i][self.n].add(result['average'])
      return True
    except pbutil.TimingRunTimeout:
      candidate.metrics[config.timing_metric_idx][self.n].addTimeout(limit)
      return False
  
  def comparer(self, metricIdx, confidence, maxTests):
    '''return a cmp like function that dynamically runs more tests to improve confidence'''
    def compare(a, b):
      for x in xrange(2*maxTests+1):
        ra=a.metrics[0][self.n]
        rb=b.metrics[0][self.n]
        if ra.diffChance(rb) >= confidence:
          # we can eliminate the null hypothesis, just compare
          return cmp(ra.mean(), rb.mean())
        if ra.sameChance(rb) >= confidence:
          return 0
        if ra.estimatedBenifitNextTest() >= rb.estimatedBenifitNextTest() and len(ra)<maxTests:
          self.test(a)
        elif len(rb)<maxTests:
          self.test(b)
        elif len(ra)<maxTests:
          self.test(a)
        else:
          warnings.warn("comparison failed between two candidates")
          return 0
      assert False
    return compare

  def cleanup(self):
    if config.cleanup_inputs:
      storage_dirs.clearInputs();
      self.inputs=[]

if __name__ == "__main__":
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





