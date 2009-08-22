#!/usr/bin/python

"""Module docstring

Usage: ./autotune.py <options> <program>

Options:
  -p             Number of threads to target 
                   - Must be greater than 1
                   - Default is worker_threads (from config file)
  -n, --random   Size of random data to optimize on
                   - Default is 100000
  --min          Min size to autotune on
                   - Default is 64
  --max          Max size to autotune on
                   - Default is 4096
  -h, --help     This help screen

"""


import re
import sys
import os
import math 
import getopt
import subprocess
import pbutil
import progress
import time
from pprint import pprint

from xml.dom.minidom import parse

INFERINPUTSIZES_SEC=5

inputSize=-1
inputSizeTarget=1.0
app = None
cfg = None
transforms=dict()
defaultArgs = None
results=[]
NULL=open("/dev/null", "w")
maxint = 2147483647
ignore_list = []

def mkcmd(args):
  t=[pbutil.benchmarkToBin(app)]
  t.extend(defaultArgs)
  if type(args) is type([]):
    t.extend(args)
  else:
    t.append(args)
  return t

getConfigVal = lambda key: pbutil.getConfigVal(cfg, key)
setConfigVal = lambda key, val: pbutil.setConfigVal(cfg, key, val)
nameof = lambda t: str(t.getAttribute("name"))

class TaskStats():
  count=0
  sec=0.0
  weight=1
  def __init__(self, w=1):
    self.weight=w

taskStats=dict()
tasks=[]

class TuneTask():
  type=""
  multiplier=1
  fn=lambda: None
  def __init__(self, type, fn, multiplier=1):
    self.type=type
    self.fn=fn
    self.multiplier=multiplier
  def run(self):
    if not taskStats.has_key(self.type):
      taskStats[self.type]=TaskStats()
    t=time.time()
    self.fn()
    taskStats[self.type].count+=1
    taskStats[self.type].sec+=time.time()-t

def reset():
  ignore_vals = []
  for ignore in ignore_list:
    ignore_vals.append((ignore, getConfigVal(ignore)))
  run_command = mkcmd("--reset")
  subprocess.check_call(run_command)
  for ignores in ignore_vals:
    setConfigVal(ignores[0], ignores[1])

def getIgnoreList():
  try:
    f = open(app + ".ignore")
    try:
      for line in f:
        ignore_list.append(line.replace("\n", ""))
    finally:
      f.close()
  except:
    pass

def mainname():
  run_command = mkcmd("--name")
  p = subprocess.Popen(run_command, stdout=subprocess.PIPE, stderr=NULL)
  os.waitpid(p.pid, 0)
  lines = p.stdout.readlines()
  return lines[-1].strip()

def getCallees(tx):
  return map( lambda c: transforms[c.getAttribute("callee")], tx.getElementsByTagName("calls") )

def getTunables(tx, type):
  return filter( lambda t: t.getAttribute("type")==type, tx.getElementsByTagName("tunable") )

def getChoiceSites(tx):
  #it would be nice to export this data, for now parse it from tunable names
  getSiteRe = re.compile( re.escape(nameof(tx)) + "_([0-9]*)_lvl[0-9]*_.*" )
  getSite=lambda t: int(getSiteRe.match(nameof(t)).group(1))
  sites=[]
  sites.extend(map(getSite, getTunables(tx,"algchoice.cutoff")))
  sites.extend(map(getSite, getTunables(tx,"algchoice.alg")))
  return list(set(sites))
  
def walkCallTree(tx, fndown=lambda x,y,z: None, fnup=lambda x,y,z: None):
  seen = set()
  seen.add(nameof(tx))
  def _walkCallTree(tx, depth=0):
    loops=len(filter(lambda t: nameof(t) in seen, getCallees(tx)))
    fnup(tx, depth, loops)
    for t in getCallees(tx):
      n=nameof(t)
      if n not in seen:
        seen.add(n) 
        _walkCallTree(t, depth+1)
    fndown(tx, depth, loops)
  _walkCallTree(tx)

#execute the algorithm with main set to ctx and return averagetiming
def timingRun(ctx, n, limit=None):
  if limit >= maxint:
    limit=None
  if limit is not None:
    limit=int(math.ceil(limit))
  args=["--transform="+nameof(ctx)]
  args.extend(defaultArgs)
  return pbutil.executeTimingRun(pbutil.benchmarkToBin(app), n, args, limit)['average']

#binary search to find a good value for param
def optimizeParam(ctx, n, param, start=0, stop=-1, branchfactor=7, best=(-1, maxint), worst=(-1, -maxint)):
  def timeat(x, thresh):
    old=getConfigVal(param)
    setConfigVal(param, x)
    t=timingRun(ctx, n, thresh)
    setConfigVal(param, old)
    return t
  if stop<0:
    stop=n
  step=(stop-start)/float(branchfactor-1)
  progress.status("optimizing %s in %s, searching [%d,%d], impact=%.2f" %(param,nameof(ctx),start,stop,max(0,(worst[1]-best[1])/best[1])))
  if step>=1:
    xs=map(lambda i: start+int(round(step*i)), xrange(branchfactor))
    ys=[]
    for i in xrange(len(xs)):
      progress.remaining(math.log(stop-start, branchfactor/2.0)*branchfactor - i)
      x=xs[i]
      if x==best[0]:
        y=best[1] # cached value
      else:
        try:
          y=timeat(x, best[1]+1)
        except pbutil.TimingRunTimeout, e:
          y=maxint
      if y<best[1]:
        best=(x,y)
      ys.append(y)
    minTime, minX = reduce(min, map(lambda i: (ys[i], xs[i]), xrange(len(xs))))
    maxTime, maxX = reduce(max, map(lambda i: (ys[i], xs[i]), xrange(len(xs))))
    improvement=(maxTime-minTime)/maxTime
    newStart = max(int(round(minX-step)), start)
    newStop = min(int(round(minX+step)), stop)
    best=(minX, minTime)
    if worst[1]<maxTime:
      worst=(maxX, maxTime)
    #print minX, start, stop, improvement
    if improvement > 0.05:
      return optimizeParam(ctx, n, param, newStart, newStop, branchfactor, best, worst)
  return best[0], worst[1]/best[1]-1.0

def autotuneCutoff(tx, tunable, n, min=0, max=-1):
  progress.push()
  progress.status("* optimize " + nameof(tx) + " tunable " + nameof(tunable))
  val, impact = optimizeParam(tx, n, nameof(tunable), min, max, best=(-1, 1.0+reduce(min, results)))
  progress.echo("* optimize " + nameof(tx) + " tunable " + nameof(tunable) + " = %d "%val + "(impact=%.2f)"%impact)
  setConfigVal(tunable, val)
  progress.pop()

iterRe = re.compile(r"^BEGIN ITERATION .* / ([0-9]+) .*")
slotRe = re.compile(r"^SLOT\[([0-9]+)\] (ADD|KEEP) *(.*) = [0-9.]+")

def autotuneAlgchoice(tx, site, ctx, n, cutoffs):
  assert n>0
  #print "* algchoice", nameof(tx), "site", site, "in", nameof(ctx), "cutoffs", map(nameof, cutoffs)
  progress.push()
  cmd=mkcmd(["--transform="+nameof(ctx), "--autotune", "--autotune-transform="+nameof(tx),  "--autotune-site=%d"%site, "--max=%d"%n])
  for x in cutoffs:
    cmd.append("--autotune-tunable="+nameof(x))
  p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=NULL)
  pfx="tuning "+nameof(tx)+":%d - "%site
  str=""
  progress.status(pfx)
  while True:
    line=p.stdout.readline()
    if line == "":
      break
    m=iterRe.match(line)
    if m:
      progress.remaining(1.0 - math.log(int(m.group(1)), 2)/math.log(n,2))
    m=slotRe.match(line)
    if m:
      if m.group(1)=="0":
        str=m.group(3)
        progress.status(str)
  assert p.wait()==0
  progress.echo("* algchoice "+nameof(tx)+":%d - "%site+str)
  progress.pop()

def enqueueAutotuneCmds(tx, maintx, passNumber, depth, loops):
  global inputSize
  cutoffs = []
  ctx=tx
  if loops > 0 or passNumber>1:
    ctx=maintx
  if loops == 0:
    cutoffs.extend(getTunables(tx, "system.seqcutoff"))
  cutoffs.extend(getTunables(tx, "system.splitsize"))
  for site in getChoiceSites(tx):
    tasks.append(TuneTask("algchoice" , lambda: autotuneAlgchoice(tx, site, ctx, inputSize, cutoffs)))
  for tunable in cutoffs:
    tasks.append(TuneTask("cutoff" , lambda: autotuneCutoff(ctx, tunable, inputSize)))

def printTx(tx, depth, loops):
  print ''.ljust(2*depth) + ' - ' + nameof(tx)
    
def determineInputSizes():
  global inputSize
  progress.status("Finding reasonable input size for training... (%d sec) " % INFERINPUTSIZES_SEC)
  inputSize=pbutil.inferGoodInputSizes( pbutil.benchmarkToBin(app)
                                      , [inputSizeTarget]
                                      , INFERINPUTSIZES_SEC)[0]
  progress.echo("* finding reasonable input size for training... %d" % inputSize)

def runTimingTest(tx):
  progress.status("running timing test")
  t=timingRun(tx, inputSize)
  if len(results)>0:
    speedup=results[-1]/t-1.0
    progress.echo("* timing test... %.4f (%.2fx speedup)"%(t, speedup))
  else:
    progress.echo("* initial timing test... %.4f s"%t)
  results.append(t)

def main(argv):
  t1=time.time()

  if len(argv) == 1:
    print "Error.  For help, run:", argv[0], "-h"
    sys.exit(2)

  global app
  global cfg 
  global inputSize 
  global ignore_list
  global defaultArgs

  config_tool_path = os.path.split(argv[0])[0] + "/configtool.py"
  app = argv[-1]
  num_threads = pbutil.cpuCount()
  fast = False

  try:
    opts, args = getopt.getopt(argv[1:-1], "hn:p:", 
        ["help","random=","min=","max=","config=","parallel_autotune","fast"])
  except getopt.error, msg:
    print "Error.  For help, run:", argv[0], "-h"
    sys.exit(2)
  # process options
  for o, a in opts:
    if o in ["-h", "--help"]:
      print __doc__
      sys.exit(0)
    if o == "-p":
      num_threads = int(a)
    if o in ["-n", "--random"]:
      inputSize = int(a)
    if o in ["-c", "--config"]:
      cfg = a
    if o == "--fast":
      fast = True
  
  pbutil.chdirToPetabricksRoot()
  pbutil.compilePetabricks()
  app = pbutil.normalizeBenchmarkName(app)
  pbutil.compileBenchmarks([app])
  cfg = pbutil.benchmarkToCfg(app)
  defaultArgs = ['--config='+cfg, '--threads=%d'%num_threads]
  getIgnoreList()

  try:
    infoxml = parse(pbutil.benchmarkToInfo(app))
  except:
    print "Cannot parse:", pbutil.benchmarkToInfo(app)
    sys.exit(-1)

  print "Reseting config entries"
  reset()

  #build index of transforms
  for t in infoxml.getElementsByTagName("transform"):
    transforms[nameof(t)]=t

  maintx = transforms[mainname()]
  
  print "Call tree:"
  walkCallTree(maintx, fnup=printTx)
  print
  print "Autotuning:"

  progress.status("building work queue")
 
  if inputSize <= 0:
    tasks.append(TuneTask("determineInputSizes", determineInputSizes))
    
  tasks.append(TuneTask("runTimingTest", lambda:runTimingTest(maintx)))

  #build list of tasks
  walkCallTree(maintx, lambda tx, depth, loops: enqueueAutotuneCmds(tx, maintx, 1, depth, loops))
  walkCallTree(maintx, lambda tx, depth, loops: enqueueAutotuneCmds(tx, maintx, 2, depth, loops))
  
  tasks.append(TuneTask("runTimingTest", lambda:runTimingTest(maintx)))

  progress.status("autotuning")

  while len(tasks)>0:
    progress.remaining(len(tasks))
    t=tasks.pop(0)
    t.run()
  progress.clear()
  
  t2=time.time()
  print "autotuning took %.2f sec"%(t2-t1)


if __name__ == "__main__":
    main(sys.argv)

