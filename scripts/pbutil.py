#!/usr/bin/python

import re
import sys
import os
import getopt
import subprocess
import time
import signal
import math
from xml.dom.minidom import parse
from pprint import pprint
from configtool import getConfigVal, setConfigVal

try:
  import numpy
except:
  sys.stderr.write("Failed to import numpy\n")

#return number of cpus online
def cpuCount():
  try:
    return os.sysconf("SC_NPROCESSORS_ONLN")
  except:
    None
  try:
    return int(os.environ["NUMBER_OF_PROCESSORS"])
  except:
    None
  try:
    return int(os.environ["NUM_PROCESSORS"])
  except:
    None
  return 1

def chdirToPetabricksRoot():
  isCurDirOk = lambda: os.path.isdir("examples") and os.path.isdir("src")
  if isCurDirOk():
    return
  old=os.getcwd()
  if not isCurDirOk():
    os.chdir(os.pardir)
  if not isCurDirOk():
    os.chdir(os.pardir)
  if not isCurDirOk():
    os.chdir(old)
    raise Exception("This script should be run from petabricks root directory")

def compilePetabricks():
  cmd=["make","-sqC","src","all"]
  if subprocess.call(cmd) != 0: 
    cmd=["make", "-j%d"%cpuCount()]
    p=subprocess.Popen(cmd)
    rv=p.wait()
    if rv!=0:
      raise Exception("pbc compile failed")
    return rv
  return 0
    
    
    
benchmarkToSrc=lambda name:"./examples/%s.pbcc"%name
benchmarkToBin=lambda name:"./examples/%s"%name

jobs=[]
def compileBenchmarks(benchmarks):
  global jobs
  benchmarks=set(benchmarks)#remove dups
  NULL=open("/dev/null","w")
  NCPU=cpuCount()
  failed=[]
  pbc="./src/pbc"
  libdepends=[pbc, "./src/libpbmain.a", "./src/libpbruntime.a", "./src/libpbcommon.a"]
  benchmarkMaxLen=reduce(max,map(len,benchmarks), 0)

  msgMaxLen= len("[%d/%d jobs] "%(NCPU,NCPU))
  msgPfx=lambda:("[%d/%d jobs]"%(len(jobs),NCPU)).ljust(msgMaxLen)
  msg=lambda m: sys.stderr.write("\n"+msgPfx()+m)
  msgUpdate=lambda: sys.stderr.write("\r"+msgPfx())
  msg("Compiling benchmarks:")

  assert os.path.isfile(pbc)
  def checkJob(name, status):
    if status is not None:
      if status == 0:
        msg(name.ljust(benchmarkMaxLen)+" compile PASSED")
      else:
        msg(name.ljust(benchmarkMaxLen)+" compile FAILED (rc=%d)"%status)
        failed.append(name)
    return status is None

  def waitForJobsLeq(n):
    global jobs
    jobs=filter(lambda j: checkJob(j[0], j[1].poll()), jobs)
    while len(jobs)>n:
      pid, status = os.wait()
      done=filter(lambda j: j[1].pid == pid, jobs)
      jobs=filter(lambda j: j[1].pid != pid, jobs)
      assert len(done) == 1
      checkJob(done[0][0], status)

  for name in benchmarks:
    src=benchmarkToSrc(name)
    bin=benchmarkToBin(name)
    if not os.path.isfile(src):
      raise Exception("invalid benchmark "+name)
    srcModTime=max(os.path.getmtime(src), reduce(max, map(os.path.getmtime, libdepends)))
    if os.path.isfile(bin) and os.path.getmtime(bin) > srcModTime:
      msg(name.ljust(benchmarkMaxLen)+" is up to date")
    else:
      if os.path.isfile(bin):
        os.unlink(bin)
      waitForJobsLeq(NCPU-1)
      jobs.append((name,subprocess.Popen([pbc, src], stdout=NULL, stderr=NULL)))
      msgUpdate()
  waitForJobsLeq(0)
  msg("Done\n\n")


def normalizeBenchmarkName(n, search=True):
  n=re.sub("^[./]*examples[/]","",n);
  n=re.sub("[.]pbcc$","",n);
  if os.path.isfile(benchmarkToSrc(n)) or not search:
    return n
  #search for the file
  n+=".pbcc"
  for root, dirs, files in os.walk("./examples"):
    if n in files:
      return normalizeBenchmarkName("%s/%s"%(root,n), False)
  raise Exception("invalid benchmark name: "+n)
  
  

def loadAndCompileBenchmarks(file, searchterms=[]):
  chdirToPetabricksRoot()
  compilePetabricks()
  benchmarks=open(file)
  stripcomment = re.compile("([^#]*)([#].*)?")
  benchmarks=map(lambda x: stripcomment.match(x).group(1).strip(), benchmarks)
  benchmarks=filter(lambda x: len(x)>0, benchmarks)
  ws = re.compile("[ \t]+")
  benchmarks=map(lambda x: ws.split(x), benchmarks)

  if len(searchterms)>0:
    benchmarks=filter(lambda b: any(s in b[0] for s in searchterms), benchmarks)

  compileBenchmarks(map(lambda x: x[0], benchmarks))
  return benchmarks

def killSubprocess(p):
  if p.poll() is None:
    try:
      p.kill() #requires python 2.6
    except:
      os.kill(p.pid, signal.SIGTERM)

def tryAorB(A, B):
  def tryAorBinst(x):
    try:
      return A(x)
    except:
      return B(x)
  return tryAorBinst

#attempt to convert to an int or float
tryIntFloat = tryAorB(int, tryAorB(float, lambda x: x))

class TimingRunTimeout(Exception):
  def __str__(self):
    return repr(self.value)

class TimingRunFailed(Exception):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return repr(self.value)

#parse timing results with a given time limit
def executeTimingRun(prog, n, args=[], limit=None):
  null=open("/dev/null", "w")
  cmd = [ prog, "--n=%d"%n, "--time" ]
  for x in args:
    cmd.append(x);
  p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=null)

  if limit is not None:
    signal.signal(signal.SIGALRM, lambda signum, frame: killSubprocess(p))
    signal.alarm(limit)

  p.wait()

  if limit is not None:
    signal.alarm(0)
    signal.signal(signal.SIGALRM, signal.SIG_DFL)

  if p.returncode == -15:
    raise TimingRunTimeout()
  if p.returncode != 0:
    raise TimingRunFailed(p.returncode)

  rslt = parse(p.stdout)
  rslt = rslt.getElementsByTagName("timing")[0].attributes
  attrs=dict()
  for x in xrange(rslt.length):
    attrs[str(rslt.item(x).name)]=tryIntFloat(rslt.item(x).nodeValue)
  return attrs

def collectTimingSamples(prog, n=100, step=100, maxTime=10.0, x=[], y=[], args=[], scaler=lambda x: x):
  start=time.time()
  left=maxTime
  try:
    while left>0:
      ni = int(math.ceil(scaler(n)))
      y.append(executeTimingRun(prog, ni, args=args, limit=int(left+1))['average'])
      x.append(ni)
      n+=step
      left=start+maxTime-time.time()
  except TimingRunTimeout:
    if len(x)<1:
      raise
  return x,y

def binarySearchInverse(fx, y, thresh=0.001, min=0.0, max=1000000000):
  y0=fx(min)
  yn=fx(max)
  assert y0<=yn
  if y0 > y-thresh:
    return min
  if yn < y+thresh:
    return max
  guess=(min+max)/2.0
  yguess=fx(guess)
  #binary search
  if abs(yguess-y) < thresh:
    return guess
  if yguess>y:
    return binarySearchInverse(fx, y, thresh, min, guess)
  else:
    return binarySearchInverse(fx, y, thresh, guess, max)


#fit y = c1 * x**c2
def expFitRaw(x,y):
  # shift to log scale
  x=map(lambda z: math.log(z,2), x)
  y=map(lambda z: math.log(z,2), y)
  # and polyfit
  c2,c1 = numpy.polyfit(x, y, 1)
  c1=2**c1
  return c1,c2

#fit y = c1 * x**c2
def expFit(x,y):
  c1,c2 = expFitRaw(x,y)
  return lambda x: c1*x**c2,\
         lambda y: 2**(math.log(y/c1, 2)/c2), \
         "%.10f * x^%.4f"%(c1,c2)

#fit y = p[0]*x**n + ... + p[n-2]*x + p[n-1]
#order is picked automatically based on expFit
def polyFit(x,y):
  c1, order = expFitRaw(x,y)
  p = numpy.polyfit(x, y, int(math.ceil(order)))
  fx=lambda x: numpy.polyval(p,x)
  invfx=lambda y: binarySearchInverse(fx, y)
  return fx, invfx, repr(p)

def collectTimingSamples2(prog, maxTime=12.0, args=[]):
  #make initial guess at order
  x,y=collectTimingSamples(prog, 5,   1,   maxTime/4, args=args, scaler=lambda x: 2**x)
  fx, invFx, str = polyFit(x,y)
  #print "Initial guess... ", len(x), str

  x,y=collectTimingSamples(prog, 0.01,  0.01,  maxTime/4, x=x, y=y, args=args, scaler=invFx)
  fx, invFx, str = polyFit(x,y)
  #print "Refinement... ", len(x), str
  
  x,y=collectTimingSamples(prog, 0.5,  0.1,  2*maxTime/4, x=x, y=y, args=args, scaler=invFx)
  fx, invFx, str = polyFit(x,y)
  #print "Refinement... ", len(x), str
  return x,y

def testEstimation(x, y, fit, prog):
  pf, pinv, pStr = fit(x,y)
  print "  ",pStr
  print "   est 10k",   pf(10000) #, "actual=", executeTimingRun(prog,10000)['average']
  print "   est 1 sec", (pinv(1))
  print "   est 2 sec", (pinv(2))
  print "   est 3 sec", (pinv(3))

def inferGoodInputSizes(prog, desiredTimes, maxTime=8.0):
  x,y=collectTimingSamples2(prog, maxTime)
  efx, efy, estr = expFit(x,y)
  pfx, pfy, pstr = polyFit(x,y)
  sizes=map(int, map(pfy, desiredTimes))
  print "Estimating reasonable input sizes (exp model: %s):"%estr, sizes
  return sizes


def getMakefileFlag(name):
  r=re.compile("^"+name+"[ ]*[=][ ]*(.*)")
  return r.match(filter(lambda l: r.match(l), open("src/Makefile"))[0]).group(1).strip()

getCXX      = lambda: getMakefileFlag("CXX")
getCXXFLAGS = lambda: getMakefileFlag("CXXFLAGS")
 
if __name__ == "__main__":
  chdirToPetabricksRoot()
  compilePetabricks()
  compileBenchmarks(["add", "multiply", "transpose","test1","test2","test3","test4","test5","test6","test7","test8","test9","test10","test11"])
  #test executetimingrun
  print "executeTimingRun:"
  pprint(executeTimingRun("./examples/add", 100, ["--trials=10"]))
  print 
  x,y=collectTimingSamples2("./examples/add", 4)
  print "test polyFit"
  testEstimation(x,y,polyFit, "./examples/add")
  print "test expFit"
  testEstimation(x,y,expFit, "./examples/add")
  print 
  print "Estimating input sizes"
  inferGoodInputSizes("./examples/add", [0.1,0.5,1.0], 8)
  


  


