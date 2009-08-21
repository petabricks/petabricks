#!/usr/bin/python

import sys

class Progress:
  ticks=0
  parent=None
  maxRemaining=-1
  curRemaining=0
  curMsg=""
  displayed=""
  def __init__(self, parent=None):
    self.parent=parent

  def hasParent(self):
    return self.parent is not None and self.parent.maxRemaining>=0

  def remaining(self, n):
    n=float(n)
    self.maxRemaining=max(self.maxRemaining, n)
    self.curRemaining=n
    self.update()

  def status(self, m):
    self.curMsg=m

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
    return self.startPercent()+(self.scale()*(1-(self.curRemaining-1.0)/self.maxRemaining))

  def update(self):
    m=""
    if self.maxRemaining >= 0:
      m="[%.0f%%]"%self.percent()
    if type(self.curMsg) is type(lambda:""):
      m+=" - "+self.curMsg()
    if type(self.curMsg) is type("") and len(self.curMsg)>0:
      m+=" - "+self.curMsg
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

remaining=lambda n: current.remaining(n)
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
  remaining(n)
  current.ticks=n

def tick(n=1):
  current.ticks-=n
  remaining(current.ticks)
  assert current.ticks >= 0

