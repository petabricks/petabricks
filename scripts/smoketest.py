#!/usr/bin/python

import pbutil
import os
import sys
import re
import subprocess 
import time

benchmarks=pbutil.loadAndCompileBenchmarks("./scripts/smoketest.tests", sys.argv[1:])

def resolveInputPath(path):
  if os.path.isfile("./testdata/"+path):
    return "./testdata/"+path
  return path

def forkrun(cmd):
  null=open("/dev/null","w")
  return subprocess.Popen(cmd, stdout=null, stderr=null)

def run(cmd):
  return forkrun(cmd).wait()

width=reduce(max, map(lambda b: len(b[0]), benchmarks), 0)

passed=0
total=0

runjobs=[]

print "Running benchmarks:"
for b in benchmarks:
  total+=1
  name=b[0]
  bin=pbutil.benchmarkToBin(b[0])

  msg=name.ljust(width)

  if not os.path.isfile(bin):
    print msg+" compile FAILED"
    continue

  #build cmd
  hash=name
  cmd=[bin, '--fixedrandom']
  for x in b[1:]:
    cmd.append(resolveInputPath(x))
    hash+=" "+os.path.basename(x)
  outfile="./testdata/.output/"+re.sub("[ /.]",'_',hash)
  cmd.append(outfile)

  #run
  runjobs.append((msg,forkrun(cmd),outfile))

for msg,p,outfile in runjobs:
  rv = p.wait()
  if rv != 0:
    print msg+" run FAILED (status=%d)"%rv
    continue

  checkcmd=["git","diff","--exit-code", outfile]
  rv = run(checkcmd)
  if rv != 0:
    time.sleep(0.1) #try letting the filesystem settle down
    rv = run(checkcmd)
    if rv != 0:
      print msg+" run FAILED (wrong output)"
      continue
  
  print msg+" run PASSED"
  passed+=1

print "%d of %d tests passed"%(passed,total)

sys.exit(min(total-passed, 124))

