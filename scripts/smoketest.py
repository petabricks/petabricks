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

CHECK=True

check_exclude=[
         "convolution/Convolution",       # Difference
         "multiply/strassen",             # Difference, why???
         "regression/whereclause",        # Difference, why???
         
         "simple/matrixrotate",           # NewProgramCrash
         "multiply/multiply",             # NewProgramCrash
         "regression/params",             # AlwaysCrashes

         "convolution2/ConvFFTRecursion",
         "convolution2/Convolution",
         "convolution2/ConvLinAlg",

         "kclustering/kmeans",            # (Variable accuracy)
         "matrixapprox/matrixapprox",     # (Variable accuracy)
         "regression/accuracymetric",     # (Variable accuracy)
         "preconditioner/preconditioner", # (Variable accuracy)
         "kernel/nwkdeVA",                # (Variable accuracy)

         "kernel/nwkde",                  # floating-point precision errors

         "regression/floattunables",
         "regression/floattunables2",
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


def diffFiles(a, b):
  '''true if files differ'''
  try:
    af=open(a)
    bf=open(b)
    rv = (af.read() != bf.read())
    af.close()
    bf.close()
    return rv
  except Exception, e:
    print "ERROR: ",e
    return True


def checkBenchmark(b):
  if b in check_exclude or not CHECK:
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
  iofiles.append(outfile+".latest")

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
    if isFloatingPoint() and os.path.exists(outfile+".float"):
      ext = ".float"
      print "FLOAT"
    else:
      ext = ""

    #run cpu config
    cmd=[bin, '--fixedrandom', '--config=%s.cfg'%outfile]
    cmd.extend(iofiles)
    rv = run(cmd)
    if rv != 0:
      print "run FAILED (status=%d, cmd=%s)"%(rv, ' '.join(cmd))
      return False

    if diffFiles(outfile+ext, outfile+".latest"):
      time.sleep(0.1) #try letting the filesystem settle down
      if diffFiles(outfile+ext, outfile+".latest"):
        print "run FAILED (wrong output)"
        return False
    
    print "run PASSED"

    if (not haveOpenCL()) or (not os.path.exists(outfile+".gpucfg")):
      return True

    #run gpu config
    cmd=[bin, '--fixedrandom', '--noisolation', '--config=%s.gpucfg'%outfile]
    cmd.extend(iofiles)
    rv = run(cmd)
    if rv != 0:
      print "gpu FAILED (status=%d, cmd=%s)"%(rv, ' '.join(cmd))
      return False

    if diffFiles(outfile+ext, outfile+".latest"):
      time.sleep(0.1) #try letting the filesystem settle down
      if diffFiles(outfile+ext, outfile+".latest"):
        print "gpu FAILED (wrong output)"
        return False
    
    print "gpu PASSED"
    return True

  return test()

def isFloatingPoint():
  for line in open("./src/config.h"):
    if "MATRIX_ELEMENT_T" in line and "float" in line:
       return True
  return False
	
def haveOpenCL():
  for line in open("./src/config.h"):
    if "HAVE_OPENCL" in line:
      if "/*" in line:
        return False
      else:
        return True
  return False








if 'nocheck' in sys.argv[1:]:
  sys.argv[1:] = filter(lambda x: x!='nocheck', sys.argv[1:])
  CHECK = False

from optparse import OptionParser
parser = OptionParser(usage="usage: smoketest.py [options]")
parser.add_option("--learning", action="store_true", dest="learning", default=False, help="enable heuristics learning")
parser.add_option("--heuristics",            type="string", help="name of the file containing the set of heuristics to use. Automatically enables --learning", default=None)

(options, args) = parser.parse_args()

if options.heuristics:
  options.learning = True

if options.learning:
  print "Learning of heuristics is ACTIVE"
  if options.heuristics:
    print "Using heuristics file: "+ str(options.heuristics)
  else:
    print "Using only heuristics in the database"
  
t1=time.time()
results,b=pbutil.loadAndCompileBenchmarks("./scripts/smoketest.tests", args, testBenchmark, postfn=checkBenchmark, learning=options.learning, heuristicSetFileName=options.heuristics, noLearningList=check_exclude)
t2=time.time()


passed=len(filter(lambda x: x.rv==0, results))
total=len(results)

print "%d of %d tests passed (took %.2fs)"%(passed,total,(t2-t1))

sys.exit(min(total-passed, 124))

