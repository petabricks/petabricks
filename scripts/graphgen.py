#!/usr/bin/python

import csv,optparse,os,sys
import pbutil
import storagedirs
import candidatetester 
from candidatetester import Candidate, CandidateTester
from sgatuner import mainname
from configtool import ConfigFile
from traininginfo import TrainingInfo
import tunerwarnings
import warnings

options=None

def main(benchmark, n, filename):
  if os.path.isdir(filename):
    filename=os.path.join(filename, 'stats/candidatelog.csv')
  f = open(filename)
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  main = mainname([pbutil.benchmarkToBin(benchmark)])
  infoxml = infoxml.transform(main)
  binpath=pbutil.benchmarkToBin(benchmark)
  tester = CandidateTester(benchmark, n)
  root = os.path.dirname(filename)
  def findconfig(c):
    if c[0]=='/':
      c=c[1:]
    if os.path.isfile(os.path.join(root, c)):
      return os.path.join(root, c)
    if os.path.isfile(os.path.join(root, '..', c)):
      return os.path.join(root, '..', c)
    return None
  rows = list(csv.DictReader(f))
  for i, row in enumerate(rows):
    if options.onlyrounds \
        and i+1<len(rows) \
        and row.has_key('round_number') \
        and rows[i+1]['round_number']==row['round_number']:
      continue
    config = findconfig(row['config_path'])
    row['tests'] = int(row['tests_complete'])+int(row['tests_timeout'])+int(row['tests_crashed'])
    candidate = Candidate(ConfigFile(config), infoxml)
    while candidate.numTests(n)<options.trials:
      try:
        tester.testN(candidate, options.trials, options.timeout)
      except candidatetester.CrashException, e:
        print >>sys.stderr, e
    try:
      row['minperf'] = candidate.metrics[0][n].min()
      row['perf_on_%d'%n], row['perf_on_%d_ci'%n] = candidate.metrics[0][n].interval(options.confidence)
      row['invperf']=1.0/row['perf_on_%d'%n]
    except Exception,e:
      row['minperf'] = -1
      row['perf_on_%d'%n] = -1
      print >>sys.stderr, e
    row['include']=True
    print >>sys.stderr, row['time'], row['minperf'], row['perf_on_%d'%n], ' (%d of %d)'%(i, len(rows))
  rows = filter(lambda x: x.has_key('include'), rows)
  headers = ['time','minperf', 'perf_on_%d'%n, 'perf_on_%d_ci'%n, 'tests', 'candidates', 'input_size', 'invperf', 'tests_timeout']
  print '#',','.join(headers)
  t=csv.DictWriter(sys.stdout, headers, extrasaction='ignore')
  t.writerows(rows)
  

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser(usage="usage: graphgen.py [options] benchmark candidatelog.csv")
  parser.add_option('--trials', type='int', default=10)
  parser.add_option('--confidence', type='float', default=.95)
  parser.add_option('--timeout', type='float', default=5.0)
  parser.add_option('--onlyrounds', type='int', default=True)
  parser.add_option('-n', type='int', default=1024)

  warnings.simplefilter('ignore',  tunerwarnings.TooManyTrials)

  (options, args) = parser.parse_args()
  if len(args)!=2:
    parser.print_usage()
    sys.exit(1)
  benchmark=args[0]
  config=os.path.abspath(args[1])
  pbutil.chdirToPetabricksRoot();
  #pbutil.compilePetabricks();
  benchmark=pbutil.normalizeBenchmarkName(benchmark)
  #pbutil.compileBenchmarks([benchmark])
  storagedirs.callWithLogDir(lambda: main(benchmark, options.n, config), '/tmp', True)


