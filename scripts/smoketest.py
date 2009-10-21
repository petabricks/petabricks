#!/usr/bin/python

import os
import pbutil
import progress
import re
import subprocess 
import sys
import configtool
import time
from xml.dom.minidom import parse


def resolveInputPath(path):
  if os.path.isfile("./testdata/"+path):
    return "./testdata/"+path
  return path

def forkrun(cmd):
  null=open("/dev/null","w")
  return subprocess.Popen(cmd, stdout=null, stderr=null)

def run(cmd):
  return forkrun(cmd).wait()

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

  cmd=[bin, '--fixedrandom', '--config=%s.cfg'%outfile, '--reset']
  if run(cmd) != 0:
    print "reset config failed"
    return False

  try:
    infoxml=parse(pbutil.benchmarkToInfo(name))
  except:
    print "invalid *.info file"
    return False

 #splitsto3  = map(lambda x: configtool.setfilter(x.getAttribute("name"), 3), pbutil.getTunablesSplitSize(infoxml))
 #cfgStatic  = map(lambda x: configtool.setfilter(x.getAttribute("name"), 2147483648), pbutil.getTunablesSequential(infoxml))
 #cfgDynamic = map(lambda x: configtool.setfilter(x.getAttribute("name"), 0), pbutil.getTunablesSequential(infoxml))

  def setCfg(x):
    configtool.processConfigFile(cfg,cfg,x)
    return True
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
results=pbutil.loadAndCompileBenchmarks("./scripts/smoketest.tests", sys.argv[1:], testBenchmark)
t2=time.time()


passed=len(filter(lambda x: x.rv==0, results))
total=len(results)

print "%d of %d tests passed (took %.2fs)"%(passed,total,(t2-t1))

sys.exit(min(total-passed, 124))

