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

def weightSamples((x,y)):
  assert len(x) == len(y)
  xw,yw=[],[]
  for i in xrange(len(x)):
    for z in xrange(int(math.ceil(math.log(x[i]-x[0]+1)))):
      xw.append(x[i])
      yw.append(y[i])
  print "weightSamples",len(x),"became",len(xw)
  return xw,yw

def estimateCurve(x,y):
  lastRes = 3600.0*24.0
  lastP=[1]
  for order in xrange(10):
    p, residuals, rank, singular_values, rcond = numpy.polyfit(x, y, order, full=True)
    res=residuals[0]
    improvement = (lastRes-res)/res
    #print order, improvement, p
    if improvement<0.005 or p[0]<=0:
      return lastP
    lastRes=res
    lastP=p
  return lastP

fx=numpy.polyval

#this could be done a much better way, but this is good enough
def invFx(p, y, thresh=0.001, min=0.0, max=1000000000):
  y0=fx(p,min)
  yn=fx(p,max)
  assert y0<=yn
  if y0 > y-thresh:
    return min
  if yn < y+thresh:
    return max
  guess=(min+max)/2.0
  yguess=fx(p, guess)
  #binary search
  if abs(yguess-y) < thresh:
    return guess
  if yguess>y:
    return invFx(p, y, thresh, min, guess)
  else:
    return invFx(p, y, thresh, guess, max)
  
def estimatePerformance(prog, maxTime=12.0, args=[]):
  x,y=collectTimingSamples(prog, 10,   10,   maxTime/3, args=args)
  x,y=collectTimingSamples(prog, 100,  100,  maxTime/3, x=x, y=y, args=args)
  x,y=collectTimingSamples(prog, 1000, 1000, maxTime/3, x=x, y=y, args=args)
  return estimateCurve(x,y)

if __name__ == "__main__":
  #test executetimingrun
  print "executeTimingRun:"
  pprint(executeTimingRun("./examples/add", 100, ["--trials=10"]))
  print 
  print "estimatePerformance:"
  p=estimatePerformance("./examples/add", 9)
  print p
  print fx(p, 10000)
  secInput=invFx(p, 1)
  print secInput, fx(p, secInput)

  


