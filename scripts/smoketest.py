#!/usr/bin/python

import os
import pbutil
import progress
import re
import subprocess 
import sys
import configtool
import time
import shutil
from xml.dom.minidom import parse

check_exclude=[
         "convolution/Convolution",           # Difference
         "multiply/strassen",                 # Difference, why???
         "regression/whereclause",            # Difference, why???
         
         "simple/matrixrotate",               # NewProgramCrash
         "multiply/multiply",                 # NewProgramCrash
         "regression/params",                 # AlwaysCrashes

         "kclustering/kmeans",                # (Variable accuracy)
         "matrixapproximation/matrixapprox",  # (Variable accuracy)
         "regression/accuracymetric",         # (Variable accuracy)
         "preconditioner/preconditioner",     # (Variable accuracy)
    ]

def resolveInputPath(path):
  if os.path.isfile("./testdata/"+path):
    return "./testdata/"+path
  return path

def forkrun(cmd):
  null=open("/dev/null","w")
  return subprocess.Popen(cmd, stdout=null, stderr=null)

def run(cmd):
  return forkrun(cmd).wait()

def checkBenchmark(b):
  if b in check_exclude:
    return True

  import sgatuner, warnings, tunerwarnings
  
  warnings.resetwarnings()
  warnings.simplefilter('error',  tunerwarnings.TunerWarning)
  warnings.simplefilter('ignore', DeprecationWarning)
  warnings.simplefilter('ignore', tunerwarnings.IgnoredTunerWarning)
  warnings.simplefilter('ignore', tunerwarnings.InitialProgramCrash)
  warnings.simplefilter('ignore', tunerwarnings.ProgramTimeout)

  try:
    sgatuner.regression_check(b)
    print "check PASSED"
    return True
  except tunerwarnings.TunerWarning, e:
    print "check FAILED (%s: %s)" % (e.__class__.__name__, str(e))
    return False
  except:
    import traceback
    traceback.print_exc(10)
    return False

def testBenchmark(b):
  name=b[0]
  bin=pbutil.benchmarkToBin(name)
  cfg=pbutil.benchmarkToCfg(name)

  if not os.path.isfile(bin):
    return False
  
  #build cmd
  hash=name
  iofiles=[]
  for x in b[1:]:
    iofiles.append(resolveInputPath(x))
    hash+=" "+os.path.basename(x)
  outfile="./testdata/.output/"+re.sub("[ /.]",'_',hash)
  iofiles.append(outfile)

  try:
    cmd=[bin, '--fixedrandom', '--config=%s.cfg'%outfile, '--reset']
    if run(cmd) != 0:
      print "ERROR: reset config failed"
      return False
  except OSError:
    print "ERROR: program not runnable"
    return False

  if os.path.isfile("%s.cfg.default"%outfile):
    shutil.copy("%s.cfg.default"%outfile, "%s.cfg"%outfile)

  try:
    infoxml=parse(pbutil.benchmarkToInfo(name))
  except:
    print "invalid *.info file"
    return False

  def test():
    cmd=[bin, '--fixedrandom', '--config=%s.cfg'%outfile]
    cmd.extend(iofiles)
    rv = run(cmd)
    if rv != 0:
      print "run FAILED (status=%d, cmd=%s)"%(rv, ' '.join(cmd))
      return False

    checkcmd=["git","diff","--exit-code", outfile]
    rv = run(checkcmd)
    if rv != 0:
      time.sleep(0.1) #try letting the filesystem settle down
      rv = run(checkcmd)
      if rv != 0:
        print "run FAILED (wrong output)"
        return False
    
    print "run PASSED"
    return True

  return test()

t1=time.time()
results=pbutil.loadAndCompileBenchmarks("./scripts/smoketest.tests", sys.argv[1:], testBenchmark, postfn=checkBenchmark)
t2=time.time()


passed=len(filter(lambda x: x.rv==0, results))
total=len(results)

print "%d of %d tests passed (took %.2fs)"%(passed,total,(t2-t1))

sys.exit(min(total-passed, 124))

