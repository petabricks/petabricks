#!/usr/bin/python

import csv,optparse,os,sys
import pbutil
import storagedirs
from candidatetester import Candidate, CandidateTester
from sgatuner import mainname
from configtool import ConfigFile
from traininginfo import TrainingInfo

options=None


def main(benchmark, n, filename):
  f = open(filename)
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  main = mainname([pbutil.benchmarkToBin(benchmark)])
  infoxml = infoxml.transform(main)
  binpath=pbutil.benchmarkToBin(benchmark)
  tester = CandidateTester(benchmark, n)

  root = os.path.dirname(filename)
  print '#time', 'tests', 'candidates', 'time_on_%d'%n, 'conferror'+str(options.confidence)
  def findconfig(c):
    if os.path.isfile(os.path.join(root, c)):
      return os.path.join(root, c)
    if os.path.isfile(os.path.join(root, '..', c)):
      return os.path.join(root, '..', c)
    return None
  for row in csv.DictReader(f):
    config_path = findconfig(row['config_path'])
    tests = int(row['tests_complete'])+int(row['tests_timeout'])+int(row['tests_crashed'])
    candidates = int(row['candidates'])
    candidate = Candidate(ConfigFile(config_path), infoxml)
    for i in xrange(options.trials):
      tester.test(candidate, options.timeout)
    results=candidate.metrics[0][n].interval(options.confidence)
    print row['time'], tests, candidates, "%.10f"%results[0], "%.10f"%results[1]


if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser(usage="usage: graphgen.py [options] benchmark candidatelog.csv")
  parser.add_option('--trials', type='int', default=3)
  parser.add_option('--confidence', type='float', default=.95)
  parser.add_option('--timeout', type='float', default=10.0)
  parser.add_option('-n', type='int', default=4096)

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


