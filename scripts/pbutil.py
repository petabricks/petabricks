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
  cmd = [ prog, "--n=%d"%n, "--time" ]
  for x in args:
    cmd.append(x);
  p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)

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

def collectTimingSamples(prog, n=100, step=100, maxTime=10.0, x=[], y=[], args=[]):
  start=time.time()
  left=maxTime
  try:
    while left>0:
      y.append(executeTimingRun(prog, n, args=args, limit=int(left+1))['average'])
      x.append(n)
      n+=step
      left=start+maxTime-time.time()
  except TimingRunTimeout:
    if len(x)<1:
      raise
  return x,y

def collectTimingSamples2(prog, maxTime=12.0, args=[]):
  x,y=[],[]
  x,y=collectTimingSamples(prog, 100,  100,  maxTime/2, x=x, y=y, args=args)
  x,y=collectTimingSamples(prog, 1000, 1000, maxTime/2, x=x, y=y, args=args)
  return x,y

def polyFit(x,y):
  lastRes = 3600.0*24.0
  lastP=[1]
  for order in xrange(10):
    p, residuals, rank, singular_values, rcond = numpy.polyfit(x, y, order, full=True)
    res=residuals[0]
    improvement = (lastRes-res)/res
    #print order, improvement, p
    if improvement<0.005 or p[0]<=0:
      break
    lastRes=res
    lastP=p
  return lambda x: numpy.polyval(p,x), repr(p)

#fit y = c1 * x**c2
def expFit(x,y):
  assert len(x)==len(y)

  # shift to log scale
  x=map(lambda z: math.log(z,2), x)
  y=map(lambda z: math.log(z,2), y)
  
  # polyfit
  c2,c1 = numpy.polyfit(x, y, 1)
  c1=2**c1

  return lambda x: c1*x**c2, \
         "%.10f * x^%.4f"%(c1,c2)

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
 
if __name__ == "__main__":
  #test executetimingrun
  print "executeTimingRun:"
  pprint(executeTimingRun("./examples/add", 100, ["--trials=10"]))
  print 
  print "estimatePerformance:"
  x,y=collectTimingSamples2("./examples/add")
  poly,polyStr = polyFit(x,y)
  exp,expStr = expFit(x,y)
  print "poly",polyStr
  print "exp",expStr
  print "poly est 10k", poly(10000)
  print "exp est 10k", exp(10000)
  print "poly est 1 sec", binarySearchInverse(poly, 1)
  print "exp est 1 sec", binarySearchInverse(exp, 1)

  


