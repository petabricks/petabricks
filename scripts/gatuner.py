#!/usr/bin/python
from configtool import ConfigFile
import pbutil
import tempfile, os

class config:
  metrics       = ['timing', 'accuracy']
  offset        = 0
  tmpdir        = "/tmp"

def tmpcfgfile(n=0):
  fd, name = tempfile.mkstemp(prefix='pbtune_%d_'%n, suffix='.cfg', dir=config.tmpdir)
  os.close(fd)
  return name

class Results:
  '''stores a list of (timing|accuracy) test results and collects statistics'''
  def __init__(self, results=[]):
    self.results=results #listof(float)
    self.timeoutResults=[] #listof(float)
  def __repr__(self):
    v=[]
    v.extend(map(lambda x: "%.4f"%x, self.results))
    v.extend(map(lambda x: ">%.4f"%x, self.timeoutResults))
    return ', '.join(v)

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
           ', '.join(map(lambda x: "%d: %s"%x, self.nToResults.iteritems()))+\
           "})"


class Candidate:
  def __init__(self, cfg):
    self.cfg = ConfigFile(cfg)
    self.metrics = map(ResultsDB, config.metrics)

class CandidateTester:
  def __init__(self, app, n):
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
  def test(self, candidate):
    timeout = 999999.0
    cmd = list(self.cmd)
    cmd.append("--max-sec=%f"%timeout)
    try:
      results = pbutil.executeRun(cmd, config.metrics)
      for i,result in enumerate(results):
        if result is not None:
          candidate.metrics[i][self.n].results.append(result['average'])
      return True
    except pbutil.TimingRunTimeout:
      timingIdx = filter(lambda i,m: m=='timing', enumerate(config.metrics))[0][0]
      candidate.metrics[timingIdx][self.n].timeoutResults.append(timeout)
      return False

  def cleanup(self):
    os.unlink(self.cfgTmp)

if __name__ == "__main__":
  tester = CandidateTester("simple/add", 512)
  try:
    candidate = Candidate(pbutil.benchmarkToCfg(tester.app))
    tester.test(candidate)
    tester.test(candidate)
    print candidate.metrics[0]
  finally:
    tester.cleanup()





