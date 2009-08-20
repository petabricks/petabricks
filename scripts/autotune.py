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
from pprint import pprint

from xml.dom.minidom import parse

INFERINPUTSIZES_SEC=5

desiredTimings=[1.0]

app = None
cfg = None
transforms=dict()
defaultArgs = None
def mkcmd(args):
  t=[pbutil.benchmarkToBin(app)]
  t.extend(defaultArgs)
  if type(args) is type([]):
    t.extend(args)
  else:
    t.append(args)
  return t

NULL=open("/dev/null", "w")

maxint = 2147483647

ignore_list = []

def getConfigVal(key):
  val = pbutil.getConfigVal(cfg, key)
  return val

def setConfigVal(key, val):
  #print "pbutil.setConfigVal", cfg, key, val
  return pbutil.setConfigVal(cfg, key, val)

def reset():
  ignore_vals = []
  for ignore in ignore_list:
    ignore_vals.append((ignore, getConfigVal(ignore)))
  run_command = mkcmd("--reset")
  subprocess.check_call(run_command)
  for ignores in ignore_vals:
    setConfigVal(ignores[0], ignores[1])

nameof = lambda t: str(t.getAttribute("name"))

# def getTunables(xml, type):
#   transforms = xml.getElementsByTagName("transform")
#
#   algchoices = []
#   for transform in transforms:
#     algchoices += transform.getElementsByTagName("tunable")
#
#   tunables = []
#   for algchoice in algchoices:
#     choice = str(algchoice.getAttribute("name"))
#     if choice not in ignore_list:
#       if algchoice.getAttribute("type") == type:
#         tunables.append(choice)
#   return tunables
#
# def getAlgChoices(xml):
#   transforms = xml.getElementsByTagName("transform")
#
#   algchoices = []
#   for transform in transforms:
#     algchoices += transform.getElementsByTagName("algchoice")
#
#   static_choices = []
#   dynamic_choices = []
#   for algchoice in algchoices:
#     choice = str(algchoice.getAttribute("name"))
#     if choice not in ignore_list:
#       if algchoice.getAttribute("type") == "sequential":
#         static_choices.append(choice)
#       else:
#         dynamic_choices.append(choice)
#   return (static_choices, dynamic_choices)
#
# def autotune(choice, trials, min, max):
#   print "Autotuning:", choice
#   run_command = ["./" + app, "--autotune=%s"%choice, "--min=%d"%min, "--max=%d"%max, "--trials=%d"%trials]
#   if parallel_autotune:
#     run_command.append("--multigrid")
#   #print run_command
#   p = subprocess.Popen(run_command, stdout=subprocess.PIPE, stderr=NULL)
#   os.waitpid(p.pid, 0)
#   lines = p.stdout.readlines()
#   print "Result:" + lines[-int(getConfigVal("autotune_alg_slots"))],
#
# def optimize(tunable, size):
#   print "Optimizing:", tunable
#   run_command = ["./" + app, "--optimize=%s"%tunable, "--random=%d"%size]
#   p = subprocess.Popen(run_command, stdout=subprocess.PIPE, stderr=NULL)
#   os.waitpid(p.pid, 0)
#   print "Result:", getConfigVal(tunable),
#
#   transforms = xml.getElementsByTagName("transform")
#
#   algchoices = []
#   for transform in transforms:
#     algchoices += transform.getElementsByTagName("algchoice")
#
#   static_choices = []
#   dynamic_choices = []
#   for algchoice in algchoices:
#     choice = str(algchoice.getAttribute("name"))
#     if choice not in ignore_list:
#       if algchoice.getAttribute("type") == "sequential":
#         static_choices.append(choice)
#       else:
#         dynamic_choices.append(choice)
#   return (static_choices, dynamic_choices)

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
  
def walkCallTree(tx, fn):
  seen = set()
  seen.add(nameof(tx))
  def _walkCallTree(tx, depth=0):
    loops=0
    for t in getCallees(tx):
      n=nameof(t)
      if n not in seen:
        seen.add(n) 
        _walkCallTree(t, depth+1)
      else: 
        loops+=1
    fn(tx, depth, loops)
  _walkCallTree(tx)

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
  if step>=1:
    xs=map(lambda i: start+int(round(step*i)), xrange(branchfactor))
    ys=[]
    progress.status("Optimizing %s in %s, searching [%d,%d], impact=%.2f" 
                    % (param,nameof(ctx),start,stop,max(0,(worst[1]-best[1])/best[1])))
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
  return best[0], (worst[1]-best[1])/best[1]


def autotuneCutoff(tx, tunable, n, min=0, max=-1):
  if max<0:
    max=n
  print "  * cutoff", nameof(tunable), "in", nameof(tx), "from", min, "to", max

def autotuneAlgchoice(tx, site, ctx, n, cutoffs):
  print "  * algchoice", nameof(tx), "site", site, "in", nameof(ctx), "cutoffs", map(nameof, cutoffs)

def autotuneTx(tx, maintx, n, depth, loops):
  #print "Autotuning", nameof(tx), "(loops=%d, depth=%d)"%(loops, depth)
  cutoffs = []
  ctx=tx
  if loops > 0:
    ctx=maintx
  if loops == 0:
    cutoffs.extend(getTunables(tx, "system.seqcutoff"))
  cutoffs.extend(getTunables(tx, "system.splitsize"))
  for site in getChoiceSites(tx):
    autotuneAlgchoice(tx, site, ctx, n, cutoffs)
  for tunable in cutoffs:
    autotuneCutoff(tx, tunable, n)
  
def main(argv):
  if len(argv) == 1:
    print "Error.  For help, run:", argv[0], "-h"
    sys.exit(2)

  global config_tool_path
  global app
  global cfg 
  global ignore_list
  global parallel_autotune
  global defaultArgs

  config_tool_path = os.path.split(argv[0])[0] + "/configtool.py"
  app = argv[-1]
  num_threads = pbutil.cpuCount()
  data_size = -1
  min = 64
  max = 4096
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
      data_size = int(a)
    if o in ["-c", "--config"]:
      cfg = a
    if o == "--parallel_autotune":
      parallel_autotune = True
    if o == "--min":
      min = int(a)
    if o == "--max":
      max = int(a)
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
 
  if data_size <= 0:
    print "Finding a reasonable input size for training... (%d sec) " % INFERINPUTSIZES_SEC
    data_size=pbutil.inferGoodInputSizes(pbutil.benchmarkToBin(app), desiredTimings,  INFERINPUTSIZES_SEC)[-1]
    print "Using",data_size

  print "Autotuning pass 1..."
  walkCallTree(maintx, lambda tx, depth, loops: autotuneTx(tx, maintx, data_size, depth, loops))

  progress.subtask(2, lambda: progress.echo(optimizeParam(maintx, data_size, "MatrixMultiply_splitsize")))
  progress.subtask(2, lambda: progress.echo(optimizeParam(maintx, data_size, "Transpose_splitsize")))
  progress.clear()

if __name__ == "__main__":
    main(sys.argv)

