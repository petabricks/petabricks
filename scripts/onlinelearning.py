#!/usr/bin/python
import logging, time, sys, itertools
import pbutil
import sgatuner
import tunerconfig
import tunerwarnings
import warnings 
import mutators 
import storagedirs
from tunerconfig import config, option_callback
from candidatetester import Candidate, CandidateTester
from traininginfo import TrainingInfo
from configtool import defaultConfigFile
from storagedirs import timers
from sgatuner import Population
from mutators import MutateFailed


def onlinelearnInner(benchmark):
  if config.debug:
    logging.basicConfig(level=logging.DEBUG)
  n = config.max_input_size
  infoxml = TrainingInfo(pbutil.benchmarkToInfo(benchmark))
  main = sgatuner.mainname([pbutil.benchmarkToBin(benchmark)])
  tester = CandidateTester(benchmark, n)
  candidate = Candidate(defaultConfigFile(pbutil.benchmarkToBin(tester.app)), infoxml.transform(main))
  sgatuner.addMutators(candidate, infoxml.globalsec())
  sgatuner.addMutators(candidate, infoxml.transform(main))
  candidate.addMutator(mutators.MultiMutator(2))

  def fitness(candidate):
    t=candidate.metrics[0][n].last()
    if t is None:
      return 10**10
    a=candidate.metrics[1][n].last()
    return t

  if not config.delete_output_dir:
    storagedirs.cur.dumpConfig()
    storagedirs.cur.dumpGitStatus()
    storagedirs.cur.saveFile(pbutil.benchmarkToInfo(benchmark))
    storagedirs.cur.saveFile(pbutil.benchmarkToBin(benchmark))
    
  try:
    timers.total.start()
    config.end_time = time.time() + config.max_time
        
    for gen in itertools.count():
      c = candidate.clone()
      for z in xrange(config.mutate_retries):
        try:
          c.mutate(tester.n)
          break
        except MutateFailed:
          if z==config.mutate_retries-1:
            warnings.warn(tunerwarnings.MutateFailed(candidate, z, tester.n))
          continue
      if tester.race(candidate, c):
        pf=fitness(candidate)
        cf=fitness(c)
        if cf < pf:
          candidate = c 
          print gen,'parent',pf,'child',cf,"(switched to child)"
        else:
          print gen,'parent',pf,'child',cf
      else:
        print 'error'

    timers.total.stop()
  finally:
    at = storagedirs.getactivetimers()
    if len(at):
      storagedirs.openCsvStats("timers", at.keys()).writerow(at.values())
    tester.cleanup()

def onlinelearn(benchmark):
  storagedirs.callWithLogDir(lambda: onlinelearnInner(benchmark),
                             config.output_dir,
                             config.delete_output_dir)

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser(usage="usage: onlinelearning.py [options] Benchmark -n N")
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
  parser.add_option("--population_high_size",  type="int",    action="callback", callback=option_callback)
  parser.add_option("--population_low_size",   type="int",    action="callback", callback=option_callback)
  parser.add_option("--offset",                type="int",    action="callback", callback=option_callback)
  parser.add_option("--name",                  type="string", action="callback", callback=option_callback)
  (options, args) = parser.parse_args()
  if len(args)!=1 or not options.n:
    parser.print_usage()
    sys.exit(1)
  tunerconfig.applypatch(tunerconfig.patch_onlinelearning)
  if options.debug:
    tunerconfig.applypatch(tunerconfig.patch_debug)
  if options.n:
    tunerconfig.applypatch(tunerconfig.patch_n(options.n))
  config.benchmark=args[0]
  pbutil.chdirToPetabricksRoot();
  pbutil.compilePetabricks();
  config.benchmark=pbutil.normalizeBenchmarkName(config.benchmark)
  pbutil.compileBenchmarks([config.benchmark])
  onlinelearn(config.benchmark)

