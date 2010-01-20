#!/usr/bin/python
from configtool import ConfigFile
import pbutil
import tempfile, os, math
from scipy import stats

class config:
  metrics               = ['timing', 'accuracy']
  offset                = 0
  tmpdir                = "/tmp"
  display_confidence    = 0.95
  prior_stddev_pct      = 0.15

def tmpcfgfile(n=0):
  fd, name = tempfile.mkstemp(prefix='pbtune_%d_'%n, suffix='.cfg', dir=config.tmpdir)
  os.close(fd)
  return name

class Results:
  '''stores a list of (timing|accuracy) test results and collects statistics'''
  def __init__(self, results=[]):
    self.results=results   #listof(float)
    self.timeoutResults=[] #listof(float)
  def __repr__(self):
    v=[]
    v.extend(map(lambda x: "%.4f"%x, self.results))
    v.extend(map(lambda x: ">%.4f"%x, self.timeoutResults))
    return ', '.join(v)
  def __str__(self):
    if len(self)==0:
      return '???'
    md=self.meanDistribution()
    a=md.ppf((1.0-config.display_confidence)/2.0)
    b=md.ppf(0.5)
    delta = b-a
    return "%.4f \xb1 %.4f" % (b, delta)

  def __len__(self):
    return len(self.results)+len(self.timeoutResults)

  def dataDistribution(self):
    '''estimated probability distribution of a single timing run'''
    assert len(self)>0
    mkdistrib = lambda points: stats.norm(*stats.norm.fit(points))
    points = list(self.results)
    if len(points) == 0:
      '''all tests timed out, seed with double the average timeout'''
      points.append(sum(self.timeoutResults)/len(self.timeoutResults)*2.0)
    if len(points) == 1:
      '''only 1 test, use prior stddev'''
      dd = stats.norm(points[0], points[0]*config.prior_stddev_pct)
    else:
      '''estimate stddev with least squares'''
      dd = mkdistrib(points)
    for p in sorted(self.timeoutResults):
      '''now lets estimate values for the points that timed out'''
      '''new points are assigned the median value above their timeout'''
      points.append(max(p, min(dd.isf(dd.sf(p)/2.0), p*2)))
      dd = mkdistrib(points)
    return dd

  def meanDistribution(self):
    '''estimated probability distribution of the real mean value'''
    dd=self.dataDistribution()
    mean, var = dd.stats()
    var/=len(self)
    return stats.norm(mean, math.sqrt(var))

class ResultsDB:
  '''stores many Results for different input sizes'''
  def __init__(self, metric, vals=dict()):
    self.metric = metric
    self.nToResults = vals
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

class Candidate:
  def __init__(self, cfg):
    self.cfg = ConfigFile(cfg)
    self.metrics = map(ResultsDB, config.metrics)

class CandidateTester:
  def __init__(self, app, n, timeout=2**31):
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
        "--offset=%d"%config.offset,
      ]
    self.timeout = timeout
  def test(self, candidate):
    candidate.cfg.save(self.cfgTmp)
    cmd = list(self.cmd)
    cmd.append("--max-sec=%f"%self.timeout)
    try:
      results = pbutil.executeRun(cmd, config.metrics)
      for i,result in enumerate(results):
        if result is not None:
          candidate.metrics[i][self.n].results.append(result['average'])
      return True
    except pbutil.TimingRunTimeout:
      timingIdx = filter(lambda x: x[1]=='timing', enumerate(config.metrics))[0][0]
      candidate.metrics[timingIdx][self.n].timeoutResults.append(self.timeout)
      return False

  def cleanup(self):
    os.unlink(self.cfgTmp)

if __name__ == "__main__":
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName('multiply/multiply')
  pbutil.compileBenchmarks([benchmark])
  tester = CandidateTester(benchmark, 768, 1.0)
  try:
    candidate = Candidate(pbutil.benchmarkToCfg(tester.app))
    tester.test(candidate)
    tester.test(candidate)
    print len(candidate.metrics[0][768])
    print candidate.metrics[0]
    print str(candidate.metrics[0][768])
    print candidate.metrics[0][768].dataDistribution().stats()
    print candidate.metrics[0][768].meanDistribution().stats()
  finally:
    tester.cleanup()





