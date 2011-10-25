#!/usr/bin/python
import pbutil, progress, tunerconfig, sgatuner, tunerwarnings
import math 
import os
import socket 
import sys
import tempfile 
import csv 
import time
import warnings

TRAILS   = 5
SHORTBAR = '-'*40
LONGBAR  = '='*50
VERSION  = "2.0"
LOGDIR    = './testdata/perflogs'
TIMESTAMP = time.time()
REV = "-"
DEBUG = False

class logdialect(csv.excel_tab):
  lineterminator="\n"

def writelog(filename, entry):
  '''appends map(entry) to filename'''
  keys = list(sorted(entry.keys()))
  header=True

  if os.path.isfile(filename):
    fd=open(filename)
    old_keys = list(csv.reader(fd, dialect=logdialect).next())
    old_keys[0]=old_keys[0].replace('#','',1) #remove hash at start
    fd.close()
    if old_keys == keys:
      header=False
    else:
      print "WARNING: logfile has outdated format", filename
      print "         renamed to", filename+".old"
      print old_keys, keys
      os.rename(filename, filename+".old")
  
  fd=open(filename, "a")
  log = csv.writer(fd, dialect=logdialect)
  if header:
    log.writerow(['#'+keys[0]] + keys[1:])
  log.writerow(map(lambda x: entry[x], keys))
  fd.close()

def geomean(nums):
  return (reduce(lambda x, y: x*y, nums))**(1.0/len(nums))

def perfScore(perf, baseline):
  return 100.0*baseline/perf['average']

def fmtPerf(perf, baseline):
  baseline=100.0*baseline
  mean      = baseline/perf['average']
  #meanprime = baseline/(perf['average']+perf['stddev']/math.sqrt(TRAILS))
  meanprime = baseline/(perf['average']+perf['stddev'])
  std  = mean-meanprime
  return "perf:%6.1f +- %2.0f" % (mean, std)

def fmtAcc(acc, target):
  diff = acc['average'] - target
  if diff == 0:
    return ""
  if diff < 0:
    s = " ERROR ACCURACY TOO LOW"
  else:
    s = ""
  if target != 0:
    diffStr = "%6.1f%%"%(100.0*(1.0+diff/abs(target)))
  else:
    diffStr = "%6.2f "%diff
  return "acc:"+diffStr+s

expandCfg = lambda x: './testdata/configs/'+x
expandLog = lambda x: LOGDIR+'/'+x.replace('.cfg','.log')
fmtCfg = lambda x: x.replace('.cfg','').ljust(20)


class Benchmark:
  def __init__(self, benchmark, cfg, n, acc_target, baseline_perf, baseline_training):
    self.benchmark  = benchmark
    self.cfg        = cfg
    self.n          = int(n)
    self.acc_target = float(acc_target)
    self.baseline        = float(baseline_perf)
    self.tuning_baseline = float(baseline_training)
    self.fixed_perf = None
    self.fixed_acc  = None
    self.tuned_perf = None
    self.tuned_acc  = None
    self.tuned_candidate = None
    self.tuning_time = 0.0
    assert os.path.isfile(expandCfg(cfg))

  def scoreFixed(self):
    return perfScore(self.fixed_perf, self.baseline)
  
  def scoreTuned(self):
    return perfScore(self.tuned_perf, self.baseline)
  
  def scoreTrainingTime(self):
    return 100.0*self.tuning_baseline/self.tuning_time

  def run(self, cfg):
    return pbutil.executeTimingRun(pbutil.benchmarkToBin(self.benchmark),
                                   int(self.n),
                                   ['--trials=%d'%TRAILS,
                                    '--config='+cfg,
                                    '--accuracy'],
                                   None,
                                   ['timing', 'accuracy'])
  
  def runFixed(self):
    self.fixed_perf, self.fixed_acc = self.run(expandCfg(self.cfg))

  def runTuned(self):
    fd, cfg = tempfile.mkstemp('.cfg')
    try:
      os.close(fd)
      self.tuned_candidate.config.save(cfg)
      self.tuned_perf, self.tuned_acc = self.run(cfg)
      self.tuned_candidate.config.save(expandCfg(self.cfg)+".latest")
    finally:
      os.unlink(cfg)

  def printFixed(self):
    print fmtCfg(self.cfg), \
          fmtPerf(self.fixed_perf, self.baseline), \
          fmtAcc(self.fixed_acc, self.acc_target)
  
  def printTuned(self):
    print fmtCfg(self.cfg), \
          fmtPerf(self.tuned_perf, self.baseline), \
          "training:%6.1f" % self.scoreTrainingTime(), \
          fmtAcc(self.tuned_acc, self.acc_target)

  def printDebug(self):
    print "%s fixed:%.4f tuned:%.4f tuning_time:%6.2f" % (
          fmtCfg(self.cfg),
          self.fixed_perf['average'],
          self.tuned_perf['average'],
          self.tuning_time)


  def autotune(self):
    self.tuning_time -= time.time()
    tunerconfig.applypatch(tunerconfig.patch_pbbenchmark)
    tunerconfig.applypatch(tunerconfig.patch_n_offset(self.n))
    tunerconfig.applypatch(tunerconfig.patch_accuracy_target(self.acc_target))
    self.tuned_candidate=sgatuner.autotune(self.benchmark)
    tunerconfig.applypatch(tunerconfig.patch_reset)
    self.tuning_time += time.time()


  def logEntry(self):
    return {
        'name'                : self.benchmark,
        'hash'                : hash( (self.cfg, self.acc_target, self.n, self.baseline, self.tuning_baseline) ),
        'n'                   : self.n,
        'acc_target'          : self.acc_target,
        'baseline'            : self.baseline,
        'fixed_perf'          : self.fixed_perf['average'],
        'fixed_acc'           : self.fixed_acc['average'],
        'tuned_perf'          : self.tuned_perf['average'],
        'tuned_acc'           : self.tuned_acc['average'],
        'tuning_time'         : self.tuning_time,
        'tuning_baseline'     : self.tuning_baseline,
        'score_fixed'         : self.scoreFixed(),
        'score_tuned'         : self.scoreTuned(),
        'score_training_time' : self.scoreTrainingTime(),
        'hostname'            : socket.gethostname(),
        'timestamp'           : TIMESTAMP,
        'revision'            : REV,
      }



def main():
  warnings.simplefilter('ignore', tunerwarnings.NewProgramCrash)
  warnings.simplefilter('ignore', tunerwarnings.TargetNotMet)
  warnings.simplefilter('ignore', tunerwarnings.NanAccuracy)

  #Parse input options
  from optparse import OptionParser
  parser = OptionParser(usage="usage: pbbenchmark [options]")
  parser.add_option("--learning", action="store_true", dest="learning", default=False, help="enable heuristics learning")
  parser.add_option("--heuristics", type="string", help="name of the file containing the set of heuristics to use. Automatically enables --learning", default=None)

  (options, args) = parser.parse_args()

  if options.heuristics:
    options.learning = True

  if options.learning:
    print "Learning of heuristics is ACTIVE"
    if options.heuristics:
      print "Using heuristics file: "+ str(options.heuristics)
    else:
      print "Using only heuristics in the database"

  progress.push()
  progress.status("compiling benchmarks")

  pbutil.chdirToPetabricksRoot()
  pbutil.compilePetabricks()

  global REV
  REV=pbutil.gitRevision()

  r, lines = pbutil.loadAndCompileBenchmarks("./scripts/pbbenchmark.tests", learning=options.learning, heuristicSetFileName=options.heuristics)

  if filter(lambda x: x.rv!=0, r):
    print "compile failed"
    sys.exit(1)

  print 
  print "All scores are relative performance to a baseline system."
  print "Higher is better."
  print

  baselines = dict()
  for line in csv.reader(open("./testdata/configs/baselines.csv")):
    if len(line)>=3:
      baselines[line[0]] = line[1:]

  benchmarks=[]
  for benchmark, cfg, n, accTarg in lines:
    try:
      baseline = baselines[benchmark]
    except KeyError:
      baseline = (1.0, 1.0)
    benchmarks.append(Benchmark(benchmark, cfg, n, accTarg, baseline[0], baseline[1]))


  print LONGBAR
  print "Fixed (no autotuning) scores:"
  print SHORTBAR
  progress.remainingTicks(len(benchmarks)+3)
  progress.tick()
  for b in benchmarks:
    progress.status("running fixed "+fmtCfg(b.cfg))
    b.runFixed()
    b.printFixed()
  progress.tick()
  score_fixed = geomean(map(Benchmark.scoreFixed, benchmarks))

  print SHORTBAR
  print "Fixed Score (pbbenchmark v%s): %.2f" % (VERSION, geomean(map(Benchmark.scoreFixed, benchmarks)))
  print LONGBAR
  print



  print LONGBAR
  print "Tuned scores:"
  print SHORTBAR
  for b in benchmarks:
    progress.status("running tuned "+fmtCfg(b.cfg))
    progress.status("autotuning")
    b.autotune()
    b.runTuned()
    b.printTuned()
    progress.tick()
  
  score_tuned = geomean(map(Benchmark.scoreTuned, benchmarks))
  score_training_time = geomean(map(Benchmark.scoreTrainingTime, benchmarks))

  print SHORTBAR
  print "Tuned Score (pbbenchmark v%s): %.2f" % (VERSION, score_tuned)
  print "Training Time Score (pbbenchmark v%s): %.2f" % (VERSION, score_training_time)
  print LONGBAR
  print

  if DEBUG:
    print LONGBAR
    print "Debug:"
    print SHORTBAR
    for b in benchmarks:
      b.printDebug()
    print LONGBAR
    print

  fd = open("./testdata/configs/baselines.csv.latest", "w")
  for b in benchmarks:
    print >>fd, "%s, %f, %f" % (b.benchmark, b.tuned_perf['average'], b.tuning_time)
  fd.close()

  if not os.path.isdir(LOGDIR):
    os.mkdir(LOGDIR)
  
  for b in benchmarks:
    writelog(expandLog(b.cfg), b.logEntry())
    
  writelog(expandLog('scores.log'), {
      'version'             : VERSION,
      'score_fixed'         : score_fixed,
      'score_tuned'         : score_tuned,
      'score_training_time' : score_training_time,
      'hostname'            : socket.gethostname(),
      'timestamp'           : TIMESTAMP,
      'revision'            : REV,
    })
  
  progress.tick()
  progress.status("done")
  progress.pop()


if __name__ == "__main__":
  main()


