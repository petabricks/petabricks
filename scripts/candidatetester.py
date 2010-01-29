#!/usr/bin/python
from configtool import ConfigFile, defaultConfigFile
import pbutil
import tempfile, os, math, warnings
from scipy import stats
warnings.simplefilter('ignore', DeprecationWarning)

class config:
  metrics               = ['timing', 'accuracy']
  offset                = 0
  tmpdir                = "/tmp"
  '''confidence intervals when displaying numbers'''
  display_confidence    = 0.95
  '''confidence intervals when comparing results'''
  compare_confidence    = 0.95
  '''guessed stddev when only 1 test is taken'''
  prior_stddev_pct      = 0.15
  '''percentage change to be viewed as insignificant when testing if two algs are equal'''
  same_threshold_pct    = 0.01

def tmpcfgfile(n=0):
  fd, name = tempfile.mkstemp(prefix='pbtune_%d_'%n, suffix='.cfg', dir=config.tmpdir)
  os.close(fd)
  return name

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
    md=self.meanDistribution()
    a=md.ppf((1.0-config.display_confidence)/2.0)
    b=md.ppf(0.5)
    delta = b-a
    return "%.4f (+- %.4f)" % (b, delta)

  def __len__(self):
    return len(self.interpolatedResults)

  def add(self, p):
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
      points.append(max(p, min(self.distribution.isf(dd.sf(p)/2.0), p*4)))
      self.distribution = mkdistrib()

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
    dd=stats.norm(self.mean()-that.mean(), math.sqrt(self.meanVariance()+that.meanVariance()))
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

  def __repr__(self):
    return "ResultsDB(%s, {"%repr(self.metric)+\
           ', '.join(map(lambda x: "%d: %s"%(x[0], repr(x[1])), self.nToResults.iteritems()))+\
           "})"

  def keys(self):
    return self.nToResults.keys()

class Candidate:
  '''A candidate algorithm in the population'''
  def __init__(self, cfg, mutators=[]):
    self.config  = ConfigFile(cfg)
    self.metrics = [ResultsDB(x) for x in config.metrics]
    self.mutators = list(mutators)

  def clone(self):
    '''
    this creates ResultDB *references*, not copies
    so new results will be added to both algs
    use clearResults to remove the copies
    '''
    t=Candidate(self.config, self.mutators)
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
    m = random.choice(self.mutators)
    m.mutate(self, n)

class CandidateTester:
  def __init__(self, app, n, args=[]):
    self.app = app
    self.bin = pbutil.benchmarkToBin(app)
    self.n = n
    self.cfgTmp = tmpcfgfile(n)
    self.cmd = [
        self.bin,
        "--config=%s"%self.cfgTmp,
        "--n=%d"%self.n,
        "--time",
        "--trials=1",
        "--offset=%d"%config.offset
      ]
    self.cmd.extend(args)
    self.timeout = None

  def test(self, candidate):
    candidate.config.save(self.cfgTmp)
    cmd = list(self.cmd)
    if self.timeout is not None:
      cmd.append("--max-sec=%f"%self.timeout)
    try:
      results = pbutil.executeRun(cmd, config.metrics)
      for i,result in enumerate(results):
        if result is not None:
          candidate.metrics[i][self.n].add(result['average'])
      return True
    except pbutil.TimingRunTimeout:
      timingIdx = filter(lambda x: x[1]=='timing', enumerate(config.metrics))[0][0]
      candidate.metrics[timingIdx][self.n].addTimeout(self.timeout)
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
    os.unlink(self.cfgTmp)

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





