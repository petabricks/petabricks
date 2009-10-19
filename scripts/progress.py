#!/usr/bin/python

import sys
import math 

barwidth=15
barchars=" -=#"

def prettybar(pct):
  p=int(math.floor(pct*barwidth*len(barchars)/100.0))
  box=p/len(barchars)
  chr=p%len(barchars)
  s=''.join([barchars[-1] for i in xrange(box)])
  if box<barchars:
    s+=barchars[chr]
  s+=''.join([barchars[0] for i in xrange(barwidth-1-box)])
  return "[" + s + "]"

class Progress:
  ticks=0
  parent=None
  maxRemaining=-1
  curRemaining=0
  nextRemaining=0
  curMsg=""
  displayed=""
  def __init__(self, parent=None):
    self.parent=parent

  def hasParent(self):
    return self.parent is not None and self.parent.maxRemaining>=0

  def remaining(self, n, nx=None):
    n=float(n)
    self.maxRemaining=max(self.maxRemaining, n)
    self.curRemaining=n
    if nx is None:
      self.nextRemaining=max(0,n-1.0)
    else:
      self.nextRemaining=nx
    self.update()

  def status(self, m):
    self.curMsg=m
    self.update()

  def startPercent(self):
    if not self.hasParent() or self.maxRemaining < 0:
      return 0.0
    else:
      return self.parent.percent()

  def endPercent(self):
    if not self.hasParent() or self.maxRemaining < 0:
      return 100.0
    else:
      return self.parent.nextPercent()

  def scale(self):
    return self.endPercent()-self.startPercent()
  
  def localPercent(self):
    return 100.0*(1-self.curRemaining/self.maxRemaining)

  def percent(self):
    return self.startPercent()+(self.scale()*(1-self.curRemaining/self.maxRemaining))
  
  def nextPercent(self):
    return self.startPercent()+(self.scale()*(1-self.nextRemaining/self.maxRemaining))

  def getStatus(self):
    if type(self.curMsg) is type(lambda:""):
      return " - "+self.curMsg()
    if len(self.curMsg)>0:
      return " - "+self.curMsg
    if self.parent is not None:
      return self.parent.getStatus()
    return ""

  def update(self):
    m=""
    if self.maxRemaining >= 0:
      m=prettybar(self.percent())
    m += self.getStatus()
    if self.hasParent():
      m+=" (%.0f%%)"%self.localPercent()
    if self.displayed!=m:
      sys.stderr.write("\r"+m+"".ljust(len(self.displayed)-len(m)))
      self.displayed=m
  
  def clear(self):
    if len(self.displayed)>0:
      sys.stderr.write("\r"+"".ljust(len(self.displayed))+"\r")
      self.displayed=""

  def echo(self, msg):
    self.clear()
    print msg
    self.update()

current=Progress()

remaining=lambda n, nx=None: current.remaining(n,nx)
status=lambda m: current.status(m)
echo=lambda m: current.echo(m)
clear=lambda : current.clear()
update=lambda : current.update()

def push():
  global current
  current.clear()
  current=Progress(current)
  current.update()

def pop():
  global current
  current.clear()
  current=current.parent
  current.update()

def subtask(n, fn):
  if n>0:
    remaining(n)
  push()
  fn()
  pop()

def remainingTicks(n):
  current.ticks=n
  remaining(n)

def tick(n=1):
  current.ticks-=n
  remaining(current.ticks)
  assert current.ticks >= 0

def untick(n=1):
  current.ticks+=n
  remaining(current.ticks)

