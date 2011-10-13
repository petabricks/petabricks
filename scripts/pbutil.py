#!/usr/bin/python

import errno
import getopt
import math
import os
import progress
import re
import select
import signal
import socket
import subprocess
import sys
import time
from xml.dom.minidom import parse,parseString
from xml.dom import DOMException
from pprint import pprint
from configtool import getConfigVal, setConfigVal
from learningcompiler import LearningCompiler

try:
  import numpy
except:
  sys.stderr.write("failed to import numpy\n")

#return number of cpus online
def cpuCount():
  try:
    return os.sysconf("SC_NPROCESSORS_ONLN")
  except:
    pass
  try:
    return os.sysconf("_SC_NPROCESSORS_ONLN")
  except:
    pass
  try:
    return int(os.environ["NUMBER_OF_PROCESSORS"])
  except:
    pass
  try:
    return int(os.environ["NUM_PROCESSORS"])
  except:
    sys.stderr.write("failed to get the number of processors\n")
  return 1 # guess 1

def getmemorysize():
  try:
    return int(re.match("MemTotal: *([0-9]+) *kB", open("/proc/meminfo").read()).group(1))*1024
  except:
    sys.stderr.write("failed to get total memory\n")
    return 8 * (1024**3) # guess 8gb

def setmemlimit(n = getmemorysize()):
  try:
    import resource
    resource.setrlimit(resource.RLIMIT_AS, (n,n))
  except:
    sys.stderr.write("failed to set memory limit\n")



def parallelRunJobs(jobs, nParallelJobs=None):
  outFile=open("/tmp/parallelRunJobs.txt", "w")
  class JobInfo:
    def __init__(self, id, fn):
      self.id=id
      self.fn=fn
      self.pid=None
      self.fd=None
      self.msg=""
      self.rv=None
    def __cmp__(this, that):
      return this.id-that.id
    def fileno(self):
      return self.fd.fileno()
    def forkrun(self):
      self.fd, w = socket.socketpair()
      self.pid = os.fork()
      if self.pid == 0:
        #child
        progress.disable()
        self.fd.close()
        class Redir():
          def __init__(self, fd):
            self.fd=fd
          def write(self, s):
            self.fd.sendall(s)
            outFile.write(s)
            outFile.flush()
        sys.stdout = Redir(w)
        #sys.stderr = sys.stdout
        try:
          rv = self.fn()
        except Exception, e:
          #import traceback
          #traceback.print_exc()
          print "Exception:",e
          rv = False
        print exitval
        if rv:
          sys.exit(0)
        else:
          sys.exit(1)
      else:
        #parent
        w.close()
        self.fd.setblocking(0)
        return self 
    def handleevent(self):
      if self.pid is None:
        return None
      try:
        m=self.fd.recv(1024)
        if m is not None:
          self.msg+=m
        if self.msg.rfind(exitval) >= 0:
          raise Exception("done")
      except:
        pid, self.rv = os.waitpid(self.pid, 0)
        assert self.pid == pid
        self.pid = None
        self.fd.close()
        self.fd = None
    def kill(self):
      if self.pid is not None:
        os.kill(self.pid, signal.SIGKILL)
    def addmsg(self, msg):
      self.msg+=msg
    def getmsg(self):
      return self.msg.replace(exitval,"") \
                     .strip()
                     
    
  startline = progress.currentline()
  if nParallelJobs is None:
    nParallelJobs=cpuCount()
  exitval="!EXIT!"
  maxprinted=[0]

  jobs_pending = map(lambda id: JobInfo(id, jobs[id]), xrange(len(jobs)))
  jobs_running = []   # JobInfo list
  jobs_done    = []   # JobInfo list

  def mkstatus():
    s="running jobs: "
    failed=len(filter(lambda x: x.rv!=0, jobs_done))
    complete=(len(jobs_done)-failed)
    if complete>0:
      s += "%d complete, "%complete
    if failed>0:
      s += "%d failed, "%failed
    s += "%d running, "%len(jobs_running)
    s += "%d pending"%len(jobs_pending)
    return s
  def updatestatus(fast=False):
    progress.remaining(2*len(jobs_pending)+len(jobs_running))
    if not fast:
      for j in jobs_done[maxprinted[0]:]:
        if j.id==maxprinted[0]:
          print j.getmsg()
          maxprinted[0]+=1
        else:
          break

  progress.push()
  progress.status(mkstatus)
  updatestatus()

  try:
    while len(jobs_pending)>0 or len(jobs_running)>0:
      #spawn new jobs
      while len(jobs_pending)>0 and len(jobs_running)<nParallelJobs:
        jobs_running.append(jobs_pending.pop(0).forkrun())
      updatestatus()
        
      #wait for an event
      rj, wj, xj = select.select(jobs_running, [], jobs_running)

      #handle pending data
      for j in rj:
        j.handleevent()
      for j in wj:
        j.handleevent()
      for j in xj:
        j.handleevent()

      #move completed jobs to jobs_done list
      newdone=filter(lambda x: x.pid is None, jobs_running)
      jobs_running = filter(lambda x: x.pid is not None, jobs_running)
      jobs_done.extend(newdone)
      jobs_done.sort()
      updatestatus(True)
  except KeyboardInterrupt:
    for j in jobs_running:
      j.kill()
      j.addmsg("INTERRUPTED")
    jobs_done.extend(jobs_running)
    jobs_done.sort()
    updatestatus()
    raise
  updatestatus()
  progress.pop()
  return jobs_done

def getscriptpath():
  try:
    import configtool
    m=re.search('''from ['"](.*)['"]''', str(configtool))
    return os.path.dirname(m.group(1))
  except:
    return os.path.abspath(os.path.dirname(sys.argv[0]))
    
def chdirToPetabricksRoot():
  old = os.getcwd()
  new = getscriptpath()
  isCurDirOk = lambda: os.path.isfile("src/compiler/pbc.cpp")
  if not isCurDirOk():
    os.chdir(new)
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


def expandBenchmarkName(name, ext):
  base=re.sub("[.]pbcc$","", name)
  if ext:
    name=base+ext
  if os.path.isfile(name):
    return name
  if name[0] != '/':
    #Try to locate the file in the standard position
    return "./examples/%s" % (name)
  else:
    return name

benchmarkToBin  = lambda name: expandBenchmarkName(name, "")
benchmarkToSrc  = lambda name: expandBenchmarkName(name, ".pbcc")
benchmarkToInfo = lambda name: expandBenchmarkName(name, ".info")
benchmarkToCfg  = lambda name: expandBenchmarkName(name, ".cfg")

class InvalidBenchmarkNameException(Exception):
  def __init__(self, name):
    self.name=name
  def __str__(self):
    return "InvalidBenchmarkNameException(%s)" % self.name

def searchBenchmarkName(n):
  for root, dirs, files in os.walk("./examples"):
    if n in files or n + ".pbcc" in files:
      return normalizeBenchmarkName("%s/%s"%(root,n), False)
  raise InvalidBenchmarkNameException(n)

def normalizeBenchmarkName(orig, search=True):
  n=re.sub("^[./]*examples[/]", "", orig);
  n=re.sub("[.]pbcc$","", n)
  if os.path.isfile(orig+".pbcc"):
    orig = os.path.abspath(orig+".pbcc")
  elif os.path.isfile(orig):
    orig = os.path.abspath(orig)
  else:
    orig = None
  if os.path.isfile(benchmarkToSrc(n)) or not search:
    return n
  else:
    try:
      return searchBenchmarkName(n)
    except InvalidBenchmarkNameException:
      if orig is not None:
        return orig
      raise


def compileBenchmark(pbc, src, binary=None, info=None, jobs=None, heuristics=None):
    if not os.path.isfile(src):
      raise IOError()
    
    #Build the command
    cmd=[pbc]
    
    if binary is not None:
      cmd.append("--output="+binary)
    if info is not None:
      cmd.append("--outputinfo="+info)
    if jobs is not None:
      cmd.append("--jobs="+str(jobs))
    if heuristics is not None:
      cmd.append("--heuristics="+heuristics)
      
    cmd.append(src)
    
    #Remove the output file (if it exists)
    if os.path.isfile(binary):
      os.unlink(binary)
      
    #Execute the compiler
    print "Executing: "+ str(cmd)
    p = subprocess.Popen(cmd, stdout=NULL, stderr=NULL)
    status = p.wait()
    return status
  
  
def compileBenchmarks(benchmarks, learning=False, heuristicSetFileName=None, noLearningList=[]):
  NULL=open("/dev/null","w")
  pbc="./src/pbc"
  libdepends=[pbc, "./src/libpbmain.a", "./src/libpbruntime.a", "./src/libpbcommon.a"]
  assert os.path.isfile(pbc)
  benchmarkMaxLen=0
  jobs_per_pbc=max(1, 2*cpuCount() / len(benchmarks))
  compiler = LearningCompiler(pbc, heuristicSetFileName, minTrialNumber=5, jobs=jobs_per_pbc)

  def innerCompileBenchmark(name):
    print name.ljust(benchmarkMaxLen)
    src=benchmarkToSrc(name)
    binary=benchmarkToBin(name)
    
    srcModTime=max(os.path.getmtime(src), reduce(max, map(os.path.getmtime, libdepends)))
    if os.path.isfile(binary) and os.path.getmtime(binary) > srcModTime:
      print "compile SKIPPED"
      return True  
    try:
      if learning and (name not in noLearningList):
        status=compiler.compileLearningHeuristics(src, finalBinary=binary)
      else:
        status=compileBenchmark(pbc, src, binary=binary, jobs=jobs_per_pbc)
      if status == 0:
        print "compile PASSED"
        return True
      else:
        print "compile FAILED (rc=%d)"%status
        return False
    except IOError:
      print "invalid benchmark"
      return False
      
      
  newjob = lambda name, fn: lambda: innerCompileBenchmark(name) and fn()
  mergejob = lambda oldfn, fn: lambda: oldfn() and fn()

  jobs=[]
  # build jobs list
  jobsdata = dict()
  for b in benchmarks:
    if type(b) is type(()):
      name, fn, postfn = b
    else:
      name, fn, postfn = b, lambda: True, lambda: True
    benchmarkMaxLen=max(benchmarkMaxLen, len(name))
    if not jobsdata.has_key(name):
      jobsdata[name] = [newjob(name,fn), postfn]
      jobs.append(name)
    else:
      jobsdata[name][0] = mergejob(jobsdata[name][0], fn)
  jobs = map(lambda n: mergejob(*jobsdata[n]), jobs)

  if learning:
    #Cannot run multiple jobs in parallel: the autotuning results 
    #would be affected
    return parallelRunJobs(jobs, nParallelJobs=1)
  else:
    return parallelRunJobs(jobs)

def loadAndCompileBenchmarks(file, searchterms=[], extrafn=lambda b: True, postfn=lambda b: True, learning=False, heuristicSetFileName=None, noLearningList=[]):
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
    
  for b in benchmarks:
    b[0]=normalizeBenchmarkName(b[0])

  return compileBenchmarks(map(lambda x: (x[0], lambda: extrafn(x), lambda: postfn(x[0])), benchmarks), learning, heuristicSetFileName, noLearningList), benchmarks

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

def goodwait(p):
  '''
  Python doesn't check if its system calls return EINTR, which is kind of
  dumb, so we have to catch this here.
  '''
  rv=None
  while True:
    try:
      rv=p.wait()
      return rv
    except OSError, e:
      if e.errno != errno.EINTR:
        raise

def xmlToDict(xml, tag, fn=tryIntFloat, idx=0):
  try:
    rslt = xml.getElementsByTagName(tag)[idx].attributes
    attrs=dict()
    for x in xrange(rslt.length):
      attrs[str(rslt.item(x).name)]=fn(rslt.item(x).nodeValue)
    return attrs
  except Exception,e:
    return None

NULL=open("/dev/null", "w")

def callAndWait(cmd):
  p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=NULL)
  goodwait(p)
  if p.returncode == -15:
    raise TimingRunTimeout()
  if p.returncode != 0:
    raise TimingRunFailed(p.returncode)
  return p

#parse timing results with a given time limit
def executeRun(cmd, returnTags=['timing', 'accuracy', 'outputhash'], retries=3):
  p = callAndWait(cmd)
  try:
    xml = parse(p.stdout)
  except Exception, e:
    print 'program crash',e
    if retries>1:
      return executeRun(cmd, returnTags, retries-1)
    else:
      p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=NULL)
      goodwait(p)
      print p.stdout.read()
      sys.exit(99)

  timing = xmlToDict(xml, "timing")
  if timing['average'] > 2**31:
    raise TimingRunTimeout()
  if type(returnTags) is type(""):
    return xmlToDict(xml, returnTags)
  else:
    return map(lambda t: xmlToDict(xml, t), returnTags)

def executeRaceRun(_cmd, configa, configb, retries=3):
  cmd = _cmd + ['--config='+configa, '--race-with='+configb]
  p = callAndWait(cmd)
  try:
    xml = parse(p.stdout)
  except Exception, e:
    print 'program crash',e
    if retries>1:
      return executeRaceRun(_cmd, configa, configb, retries-1)
    else:
      p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=NULL)
      goodwait(p)
      print p.stdout.read()
      sys.exit(99)
  aresult = xmlToDict(xml, "testresult", tryIntFloat, 0)
  bresult = xmlToDict(xml, "testresult", tryIntFloat, 1)
  assert aresult['label']==0
  assert bresult['label']==1
  return aresult, bresult

#parse timing results with a given time limit
def executeTimingRun(prog, n, args=[], limit=None, returnTags='timing'):
  cmd = [ prog, "--n=%d"%n, "--time" ]
  cmd.extend(args);
  if limit:
    cmd.append("--max-sec=%f" % float(limit))
  return executeRun(cmd, returnTags)

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
  x,y=collectTimingSamples(prog, 4,   1,   maxTime, args=args, scaler=lambda x: 2**x)
  return x,y

def testEstimation(x, y, fit, prog):
  pf, pinv, pStr = fit(x,y)
  print "  ",pStr
  print "   est 10k",   pf(10000) #, "actual=", executeTimingRun(prog,10000)['average']
  print "   est 1 sec", (pinv(1))
  print "   est 2 sec", (pinv(2))
  print "   est 3 sec", (pinv(3))

def inferGoodInputSizes(prog, desiredTimes, maxTime=5.0):
  x,y=collectTimingSamples2(prog, maxTime)
  efx, efy, estr = expFit(x,y)
  #pfx, pfy, pstr = polyFit(x,y)
  sizes=map(int, map(efy, desiredTimes))
  return sizes


def getMakefileFlag(name):
  r=re.compile("^"+name+"[ ]*[=][ ]*(.*)")
  return r.match(filter(lambda l: r.match(l), open("src/Makefile"))[0]).group(1).strip()

getCXX      = lambda: getMakefileFlag("CXX")
getCXXFLAGS = lambda: getMakefileFlag("CXXFLAGS")

def getTunables(tx, type):
  return filter( lambda t: t.getAttribute("type")==type, tx.getElementsByTagName("tunable") )

getTunablesSequential=lambda tx: getTunables(tx, "system.cutoff.sequential")
getTunablesSplitSize=lambda tx: getTunables(tx, "system.cutoff.splitsize") 

def mainname(bin):
  run_command = mkcmd("--name")
  p = subprocess.Popen(run_command, stdout=subprocess.PIPE, stderr=substderr)
  os.waitpid(p.pid, 0)
  lines = p.stdout.readlines()
  return lines[-1].strip()

if __name__ == "__main__":
  chdirToPetabricksRoot()
  compilePetabricks()
  compileBenchmarks(map(normalizeBenchmarkName, ["add", "multiply", "transpose"]))
  print "Estimating input sizes"
  inferGoodInputSizes("./examples/simple/add", [0.1,0.5,1.0], 2)

