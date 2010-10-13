import os, shutil, tempfile, csv, time
from tunerconfig import config

#try:
#  from os.path import relpath
#except:
def _relpath(path, root = None):
  if root is None:
    root = os.getcwd()
  return path[len(os.path.commonprefix((path,root))):]

class Timer:
  def __init__(self):
    self.t=0.0
    self.l=0.0
  def start(self):
    self.t-=time.time()
  def stop(self):
    self.t+=time.time()
  def wrap(self, fn):
    self.t-=time.time()
    try:
      rv=fn()
    finally:
      self.t+=time.time()
    return rv
  def lap(self):
    if self.t>=0.0:
      v=self.t-self.l
      self.l=self.t
    else:
      t=time.time()
      v=self.t+t-self.l
      self.l=self.t+t
    return v
  def total(self):
    if self.t>=0.0:
      return self.t
    else:
      return time.time()+self.t

class NullTimer:
  def start(self):
    pass
  def stop(self):
    pass
  def wrap(self, fn):
    return fn()
  def lap(self):
    return -1
  def total(self):
    return -1

class timers:
  total=Timer()
  inputgen=Timer()
  testing=Timer()


def getactivetimers():
  return dict(
           filter(lambda y: type(y[1]) is Timer,
             map(lambda x: (x, getattr(timers,x)),
               dir(timers))))
def disabletimers():
  for k in timers.getactive().keys():
    setattr(timers, k, NullTimer())

class StorageDirsTemplate:
  def __init__(self, root):
    self.root    = root
    self.candidated = os.path.join(root, 'candidate')
    self.inputd  = os.path.join(root, 'data')
    self.bestd   = os.path.join(root, 'best')
    self.statsd  = os.path.join(root, 'stats')
    os.mkdir(self.candidated)
    os.mkdir(self.statsd)
    os.mkdir(self.bestd)
    os.mkdir(self.inputd)
  
  def candidate(self, cid):
    d = os.path.join(self.candidated, "%05d" % cid)
    if not os.path.isdir(d):
      os.mkdir(d)
    return d
  
  def candidaterelative(self, cid, p='..'):
    d =  p+"/candidate/%05d" % cid
    return d

  def markBest(self, cid, n, acc):
    s=self.candidaterelative(cid)
    if acc is not None:
      d=os.path.join(self.bestd, "n%d_acc%d"%(n,acc))
    else:
      d=os.path.join(self.bestd, "n%d"%n)
    os.symlink(s,d)
  
  def results(self, acc=None):
    if acc is not None:
      return os.path.join(self.statsd, "timing_acc%d"%acc)
    else:
      return os.path.join(self.statsd, "timing")
  
  def inputpfx(self, size, number):
    return os.path.join(self.inputd, "n%010d_i%02d_" % (size, number))
      
  def clearInputs(self):
    for f in os.listdir(self.inputd):
      os.unlink(os.path.join(self.inputd, f))

  def openCsvStats(self, name, headerRow):
    w=csv.writer(open(os.path.join(self.statsd, name + ".csv"), "w"))
    if headerRow is not None:
      w.writerow(headerRow)
    return w

storage_dirs = None

def callWithLogDir(fn, root, delete):
  d = tempfile.mkdtemp(prefix='pbtunerun_', dir=os.path.expanduser(root))
  if not delete:
    print d
  global storage_dirs
  storage_dirs = StorageDirsTemplate(d)
  try:
    fn()
  finally:
    if delete:
      shutil.rmtree(d)

candidate    = lambda cid:          storage_dirs.candidate(cid)
inputpfx     = lambda size, number: storage_dirs.inputpfx(size, number)
clearInputs  = lambda :             storage_dirs.clearInputs()
openCsvStats = lambda name, header: storage_dirs.openCsvStats(name, header)

relpath      = lambda d: _relpath(d, storage_dirs.root)

