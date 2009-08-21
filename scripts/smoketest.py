#!/usr/bin/python

import pbutil
import os
import sys
import re
import subprocess 
import time
import progress

progress.remaining(1.05)
progress.status("running smoketest")

t1=time.time()
benchmarks=pbutil.loadAndCompileBenchmarks("./scripts/smoketest.tests", sys.argv[1:])
t2=time.time()

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


progress.remaining(0.05)
progress.push()
progress.status("running benchmarks")
progress.echo("Running benchmarks:")
progress.remainingTicks(2*len(benchmarks))

for b in benchmarks:
  progress.tick()
  total+=1
  name=b[0]
  bin=pbutil.benchmarkToBin(b[0])

  msg=name.ljust(width)

  if not os.path.isfile(bin):
    progress.echo(msg+" compile FAILED")
    progress.tick()
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
  runjobs.append((msg,forkrun(cmd),outfile, cmd))

for msg,p,outfile,cmd in runjobs:
  rv = p.wait()
  progress.tick()
  if rv != 0:
    progress.echo(msg+" run FAILED (status=%d, cmd=%s)"%(rv, ' '.join(cmd)))
    continue

  checkcmd=["git","diff","--exit-code", outfile]
  rv = run(checkcmd)
  if rv != 0:
    time.sleep(0.1) #try letting the filesystem settle down
    rv = run(checkcmd)
    if rv != 0:
      progress.echo(msg+" run FAILED (wrong output)")
      continue
  
  progress.echo(msg+" run PASSED")
  passed+=1

t3=time.time()
progress.echo("%d of %d tests passed (%.2fs compile, %.2fs run)"%(passed,total,(t2-t1),(t3-t2)))

progress.pop()
progress.remaining(0)
progress.clear()

sys.exit(min(total-passed, 124))

