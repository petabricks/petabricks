#!/usr/bin/python

"""Module docstring

Usage: ./autotune.py <options> <program>

Options:
  -p             Number of threads to target 
                   - Must be greater than 1
                   - Default is worker_threads (from config file)
  -n, --random   Size of random data to optimize on
                   - Default is 100000
  -h, --help     This help screen

"""


import re
import sys
import os
import math 
import optparse 
import subprocess
import pbutil
import progress
import time
from pprint import pprint
from pbutil import getTunables

from xml.dom.minidom import parse

pbutil.setmemlimit()

INFERINPUTSIZES_SEC=5

inputSizeTarget=1.0
app = None
cfg = None
transforms=dict()
defaultArgs = None
results=[]
NULL=open("/dev/null", "w")
maxint = 2147483647
ignore_list = []
options=None
substderr=open("/dev/null","w")

def goodtimelimit():
  if options.maxsec>0:
    return options.maxsec
  return 1.0+reduce(min, results, 3600*24*7)

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

#these initial weights make the progress bar run more smoothly 
#generated from performance breakdown for Sort on kleptocracy
taskStats = {'cutoff':TaskStats(1.43), 'determineInputSizes':TaskStats(1.56), 'runTimingTest':TaskStats(0.44), 'algchoice':TaskStats(0.57)}
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
    taskStats[self.type].count += self.multiplier
    taskStats[self.type].sec += (time.time()-t)
  def weight(self):
    if not taskStats.has_key(self.type):
      taskStats[self.type]=TaskStats()
    return taskStats[self.type].weight * self.multiplier

def remainingTaskWeight():
  return sum(map(lambda x: x.weight(), tasks))

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
  p = subprocess.Popen(run_command, stdout=subprocess.PIPE, stderr=substderr)
  os.waitpid(p.pid, 0)
  lines = p.stdout.readlines()
  return lines[-1].strip()

def getCallees(tx):
  return map( lambda c: transforms[c.getAttribute("callee")], tx.getElementsByTagName("calls") )

def getChoiceSites(tx):
  getSiteRe = re.compile( re.escape(nameof(tx)) + "_([0-9]*)_lvl[0-9]*_.*" )
  getSite=lambda t: int(getSiteRe.match(nameof(t)).group(1))
  #it would be nice to export this data, for now parse it from tunable names
  sites=[]
  sites.extend(map(getSite, getTunables(tx,"algchoice.cutoff")))
  sites.extend(map(getSite, getTunables(tx,"algchoice.alg")))
  return list(set(sites))

def getChoiceSiteWeight(tx, site, cutoffs):
  getSiteRe = re.compile( re.escape(nameof(tx)) + "_([0-9]*)_lvl[0-9]*_.*" )
  getSite=lambda t: int(getSiteRe.match(nameof(t)).group(1))
  tunables=filter(lambda x: getSite(x)==site, getTunables(tx,"algchoice.alg"))
  algcounts=map(lambda x: int(x.getAttribute("max"))-int(x.getAttribute("min")),tunables) 
  return reduce(max, algcounts, 1)+len(cutoffs)
  
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

def autotuneCutoffBinarySearch(tx, tunable, n, min=0, max=-1):
  progress.push()
  progress.status("* optimize " + nameof(tx) + " tunable " + nameof(tunable))
  val, impact = optimizeParam(tx, n, nameof(tunable), min, max, best=(-1, goottimelimit()))
  print "* optimize " + nameof(tx) + " tunable " + nameof(tunable) + " = %d "%val + "(impact=%.2f)"%impact
  setConfigVal(tunable, val)
  progress.pop()

iterRe = re.compile(r"^BEGIN ITERATION .* / ([0-9]+)")
slotRe = re.compile(r"^SLOT\[([0-9]+)\] (ADD|KEEP) *(.*) = ([0-9.eE-]+)")
runRe = re.compile(r"^ *[*] *(TRY|TEST)")

def autotuneAlgchoice(tx, site, ctx, n, cutoffs):
  progress.push()
  cmd=mkcmd(["--transform="+nameof(ctx), "--autotune", "--autotune-transform="+nameof(tx),  "--autotune-site=%d"%site,
             "--max=%d"%n, "--max-sec=%d"%goodtimelimit()])
  for x in cutoffs:
    cmd.append("--autotune-tunable="+nameof(x))
  if options.debug or options.justprint:
    print ' '.join(cmd)
  if options.justprint:
    progress.pop()
    return True
  runsCur=1
  runsLast=1
  inputCur=1
  #progress formula assumes linear runtime of program
  calcprog=lambda: 1.0 - (math.log(inputCur,2) + min(1.0,runsCur/float(runsLast)))/(math.log(n,2)+1)
  p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=substderr)
  pfx="tuning "+nameof(tx)+":%d - "%site
  if site == -1:
    pfx="tuning %d cutoffs in %s - " % (len(cutoffs), nameof(tx))
  str=""
  progress.status(pfx)
  while True:
    line=p.stdout.readline()
    if line == "":
      break
    #BEGIN ITERATION .... / 4096
    m=iterRe.match(line)
    if m:
      inputCur=int(m.group(1))
      runsLast=runsCur
      runsCur=0
      progress.remaining(calcprog())
    #SLOT[0] KEEP .... = 1.25
    m=slotRe.match(line)
    if m:
      if m.group(1)=="0":
        str=m.group(3)
        progress.status(pfx+str)
    #  * TRY ... 
    m=runRe.match(line)
    if m:
      runsCur+=1
      progress.remaining(calcprog())
  progress.pop()
  if p.wait()==0:
    print "* "+pfx+str
    return True
  else:
    print 'FAILURE OF TUNING STEP:', ' '.join(cmd)
    return False

def enqueueAutotuneCmds(tx, maintx, passNumber, depth, loops):
  mkAlgchoiceTask=lambda tx, site, ctx, n, cutoffs: TuneTask("algchoice" , lambda: autotuneAlgchoice(tx, site, ctx, n, cutoffs), getChoiceSiteWeight(tx, site, cutoffs))

  cutoffs = []
  ctx=tx
  if loops > 0 or passNumber>1:
    ctx=maintx
  if loops == 0:
    cutoffs.extend(pbutil.getTunablesSequential(tx))
  cutoffs.extend(pbutil.getTunablesSplitSize(tx))
  choicesites = getChoiceSites(tx)
  for site in choicesites:
    tasks.append(mkAlgchoiceTask(tx,site,ctx,options.n, cutoffs))
  if len(choicesites)==0 and len(cutoffs)>0 and not options.fast:
    tasks.append(TuneTask("cutoff" , lambda: autotuneAlgchoice(tx, -1, ctx, options.n, cutoffs), len(cutoffs)))
  #for tunable in cutoffs:
  #  tasks.append(TuneTask("cutoff" , lambda: autotuneCutoff(ctx, tunable, options.n)))

def printTx(tx, depth, loops):
  t = len(getTunables(tx, "system.cutoff.splitsize"))
  cs = len(getChoiceSites(tx))
  if loops == 0:
    t+=len(getTunables(tx, "system.cutoff.sequential"))
  print ''.ljust(2*depth) + ' - ' + nameof(tx) + " (%d choice site, %d cutoffs)"%(cs,t)
    
def determineInputSizes():
  progress.status("finding reasonable input size for training... (%d sec) " % INFERINPUTSIZES_SEC)
  options.n=pbutil.inferGoodInputSizes( pbutil.benchmarkToBin(app)
                                      , [inputSizeTarget]
                                      , INFERINPUTSIZES_SEC)[0]
  print "* finding reasonable input size for training... %d" % options.n 

def runTimingTest(tx):
  progress.push()
  progress.remaining(1)
  progress.status("running timing test")
  t=timingRun(tx, options.n)
  progress.remaining(0)
  if len(results)>0:
    speedup=results[-1]/t
    print "* timing test... %.4f (%.2fx speedup)"%(t, speedup)
  else:
    print "* initial timing test... %.4lf s"%t
  results.append(t)
  progress.pop()

def main(argv):
  t1=time.time()

  global app
  global cfg 
  global ignore_list
  global defaultArgs
  global substderr
  global options 

  config_tool_path = os.path.split(argv[0])[0] + "/configtool.py"
  fast = False

  parser = optparse.OptionParser(usage="usage: %prog [options] BENCHMARK")
  parser.add_option("--min", type="int", dest="min", default=1)
  parser.add_option("-n", "--random", "--max", type="int", dest="n", default=-1)
  parser.add_option("--offset", type="int", dest="offset", default=0)
  parser.add_option("--max-sec", type="float", dest="maxsec", default=0)
  parser.add_option("-d", "--debug",  action="store_true", dest="debug", default=False)
  parser.add_option("-f", "--fast",  action="store_true", dest="fast", default=False)
  parser.add_option("--threads",      type="int", dest="threads", default=pbutil.cpuCount())
  parser.add_option("-c", "--config", dest="config", default=None)
  parser.add_option("--noisolation", action="store_true", dest="noisolation", default=False)
  parser.add_option("--print", action="store_true", dest="justprint", default=False)
  parser.add_option("--time", action="store_true", dest="time", default=False)
  parser.add_option("--acctrials", type="int", dest="acctrials", default=None)
  parser.add_option("--accimprovetries", type="int", dest="accimprovetries", default=None)
  parser.add_option("--trials", type="int", dest="trials", default=None)
  parser.add_option("--trials-sec", type="float", dest="trialssec", default=None)
  parser.add_option("--trials-max", type="int", dest="trialsmax", default=None)
  parser.add_option("--transform", dest="transform", default=None)
  options,args = parser.parse_args()

  if len(args) != 1:
    parser.error("expected benchmark name as arg")

  cfg=options.config
  app=args[0]

  pbutil.chdirToPetabricksRoot()
  pbutil.compilePetabricks()
  app = pbutil.normalizeBenchmarkName(app)
  pbutil.compileBenchmarks([app])
  
  if options.debug:
    substderr = sys.__stderr__

  if cfg is None:
    cfg = pbutil.benchmarkToCfg(app)

  defaultArgs = ['--config='+cfg, '--threads=%d'%options.threads, '--offset=%d'%options.offset, '--min=%d'%options.min]

  if options.noisolation:
    defaultArgs.append("--noisolation")

  if options.acctrials is not None:
    defaultArgs.append("--acctrials=%d"%options.acctrials)
  if options.trials is not None:
    defaultArgs.append("--trials=%d"%options.trials)
  if options.trialssec is not None:
    defaultArgs.append("--trials-sec=%f"%options.trialssec)
  if options.trialsmax is not None:
    defaultArgs.append("--trials-max=%d"%options.trialsmax)
  if options.accimprovetries is not None:
    defaultArgs.append("--accimprovetries=%d"%options.accimprovetries)

  getIgnoreList()

  try:
    infoxml = parse(pbutil.benchmarkToInfo(app))
  except:
    print "Cannot parse:", pbutil.benchmarkToInfo(app)
    sys.exit(-1)

 #print "Reseting config entries"
 #reset()

  #build index of transforms
  for t in infoxml.getElementsByTagName("transform"):
    transforms[nameof(t)]=t
    if t.getAttribute("templateChoice")=="0":
      transforms[t.getAttribute("templateName")] = t

  if options.transform is None:
    maintx = transforms[mainname()]
  else:
    maintx = transforms[options.transform]
  
  print "Call tree:"
  walkCallTree(maintx, fnup=printTx)
  print
  print "Autotuning:"

  progress.status("building work queue")
 
  if options.n <= 0:
    tasks.append(TuneTask("determineInputSizes", determineInputSizes))
    
  if options.time:
    tasks.append(TuneTask("runTimingTest", lambda:runTimingTest(maintx)))

  #build list of tasks
  if not options.fast:
    walkCallTree(maintx, lambda tx, depth, loops: enqueueAutotuneCmds(tx, maintx, 1, depth, loops))
  walkCallTree(maintx, lambda tx, depth, loops: enqueueAutotuneCmds(tx, maintx, 2, depth, loops))
  
  if options.time:
    tasks.append(TuneTask("runTimingTest", lambda:runTimingTest(maintx)))

  progress.status("autotuning")

  while len(tasks)>0:
    w1=remainingTaskWeight()
    task=tasks.pop(0)
    w2=remainingTaskWeight()
    progress.remaining(w1, w2)
    task.run()
  progress.clear()

  t2=time.time()
  sec=t2-t1

  

  print "autotuning took %.2f sec"%(t2-t1)
  for k,v in taskStats.items():
    print "  %.2f sec in %s"%(v.sec, k)
    sec -= v.sec
  print "  %.2f sec in unknown"%sec
  
  names=taskStats.keys()
  weights=map(lambda x: x.sec/float(max(x.count, 1)), taskStats.values())
  scale=len(weights)/sum(weights)
  print "Suggested weights:"
  print "taskStats = {" + ", ".join(map(lambda i: "'%s':TaskStats(%.2f)"%(names[i], scale*weights[i]), xrange(len(names)))) + "}"


if __name__ == "__main__":
    main(sys.argv)

