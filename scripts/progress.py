#!/usr/bin/python
import sys
import math 

barwidth=15
barchars=" -=#"

class StatusWrapper:
  def __init__(self, fd):
    self.displayed=""
    self.fd = fd
  def write(self, s):
    if self.displayed== "":
      sys.__stdout__.write(s)
    else:
      old=self.displayed
      self.status("")
      self.write(s)
      self.status(old)
  def status(self, m):
    if self.displayed!=m:
      self.fd.write("\r"+m+"".ljust(len(self.displayed)-len(m)))
      if m == "":
        self.fd.write("\r")
      self.displayed=m

sys.stdout    = StatusWrapper(sys.stderr)
setstatusline = lambda s: sys.stdout.status(s)

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
    setstatusline(m)

current=Progress()

remaining=lambda n, nx=None: current.remaining(n,nx)
status=lambda m: current.status(m)
clear=lambda : setstatusline("")
update=lambda : current.update()

def push():
  global current
  clear()
  current=Progress(current)
  current.update()

def pop():
  global current
  clear()
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

def curseswrapper(fn):
  clear()
  import curses
  #redirect a stream to curses
  class CursesPrinter:
    def __init__(self, window):
      self.window = window
      self.log = [""]
    def write(self, s):
      lines=str(s).split('\n')
      self.log[-1] += lines[0]
      for l in lines[1:]:
        self.log.append(l)
      self.window.erase()
      h,w=self.window.getmaxyx()
      cnt=map(lambda s: s[:w], self.log[-h:])
      for i in xrange(len(cnt)):
        self.window.insstr(i,0,cnt[i])
      self.window.refresh()
    def dump(self):
      for str in log.log:
        sys.__stdout__.write(str + "\n")
    def replace(self, s = ""):
      self.log = [""]
      self.write(s)

  stdscr = curses.initscr()
  curses.curs_set(0)
  h,w = stdscr.getmaxyx()
  log    = CursesPrinter(stdscr.derwin(h-1, w, 0,   0))
  status = CursesPrinter(stdscr.derwin(1,   w, h-1, 0))
  
  global setstatusline
  oldstdout = sys.stdout
  oldstderr = sys.stderr
  oldstatusline = setstatusline
  sys.stdout = log
  sys.stderr = log
  setstatusline = lambda s: status.replace(s)

  def cleanup():
    status.replace()
    curses.endwin()
    sys.stdout = oldstdout
    sys.stderr = oldstderr
    setstatusline =  oldstatusline
    log.dump()
  try:
    fn()
  except:
    cleanup()
    raise
  cleanup()
    
def test():
  import time
  for i in xrange(100):
    print "hello",i
    remaining(100-i)
    status("foo foo foo foo "+str(i))
    time.sleep(0.1)

if __name__ == "__main__":
  test()
  curseswrapper(test)



