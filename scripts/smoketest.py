#!/usr/bin/python

import pbutil
import os
import sys
import re
import subprocess 
import time
import progress

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

  if not os.path.isfile(bin):
    return False

  #build cmd
  hash=name
  cmd=[bin, '--fixedrandom']
  for x in b[1:]:
    cmd.append(resolveInputPath(x))
    hash+=" "+os.path.basename(x)
  outfile="./testdata/.output/"+re.sub("[ /.]",'_',hash)
  cmd.append(outfile)

  #run
  p = forkrun(cmd)
  rv = p.wait()
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

t1=time.time()
#progress.curseswrapper(lambda: pbutil.loadAndCompileBenchmarks("./scripts/smoketest.tests", sys.argv[1:], testBenchmark))
pbutil.loadAndCompileBenchmarks("./scripts/smoketest.tests", sys.argv[1:], testBenchmark)
t2=time.time()

# t3=time.time()
# print "%d of %d tests passed (%.2fs compile, %.2fs run)"%(passed,total,(t2-t1),(t3-t2))

# progress.pop()
# progress.remaining(0)
# progress.clear()

# sys.exit(min(total-passed, 124))

