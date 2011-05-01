import os, shutil, tempfile, csv, time
import subprocess
import warnings
import tunerconfig
from tunerconfig import config

class dialect(csv.excel_tab):
  lineterminator="\n"

try:
  from os.path import relpath as _relpath
except:
  def _relpath(path, root = None):
    if root is None:
      root = os.getcwd()
    return path[len(os.path.commonprefix((path,root+'/'))):]

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
    self.mutatord   = os.path.join(root, 'mutator')
    self.inputd     = os.path.join(root, 'data')
    self.bestd      = os.path.join(root, 'best')
    self.statsd     = os.path.join(root, 'stats')
    self.configd    = os.path.join(root, 'tunerconfig')
    os.mkdir(self.candidated)
    os.mkdir(self.statsd)
    os.mkdir(self.bestd)
    os.mkdir(self.inputd)
    if config.mutatorlog:
      os.mkdir(self.mutatord)
    os.mkdir(self.configd)
  
  def candidate(self, cid):
    d = os.path.join(self.candidated, "%05d" % cid)
    if not os.path.isdir(d):
      os.mkdir(d)
    return d
  
  def mutatorlog(self, m):
    return os.path.join(self.mutatord, m.uniquename()+'.csv')
  
  def candidaterelative(self, cid, p='..'):
    d =  p+"/candidate/%05d" % cid
    return d

  def markBest(self, cid, n, acc):
    s=self.candidaterelative(cid)
    if acc is not None:
      d=os.path.join(self.bestd, "n%d_acc%d"%(n,acc))
    else:
      d=os.path.join(self.bestd, "n%d"%n)
    try:
      os.symlink(s,d)
    except:
      os.unlink(d)
      os.symlink(s,d)
  
  def results(self, acc=None):
    if acc is not None:
      return os.path.join(self.statsd, "timing_acc%d.csv"%acc)
    else:
      return os.path.join(self.statsd, "timing.csv")
  
  def inputpfx(self, size, number):
    return os.path.join(self.inputd, "n%010d_i%02d_" % (size, number))
      
  def clearInputs(self):
    for f in os.listdir(self.inputd):
      os.unlink(os.path.join(self.inputd, f))

  def openCsvStats(self, name, headerRow):
    w=csv.writer(open(os.path.join(self.statsd, name + ".csv"), "w"), dialect=dialect)
    if headerRow is not None:
      w.writerow(['#'+headerRow[0]]+list(headerRow[1:]))
    return w

  def saveFile(self, path):
    shutil.copy(path, self.configd)
  
  def dumpConfig(self):
    w=open(os.path.join(self.configd, 'config.py'), "w")
    tunerconfig.dump(w)
    w.close()

  def dumpGitStatus(self):
    try:
      w=open(os.path.join(self.configd, 'git.status'), "w")
      p=subprocess.Popen(['git', 'log', '-n1', '--pretty=format:%H: %s'], stdout=w)
      p.communicate()
      p=subprocess.Popen(['git', 'status'], stdout=w)
      p.communicate()
      w.close()
    except Exception, e:
      warnings.warn("Failed to record git status: "+e)

cur = None

def callWithLogDir(fn, root, delete):
  root_expanded = os.path.expanduser(root)
  if not os.path.isdir(root_expanded) and tunerconfig.config_defaults.output_dir == root:
    os.mkdir(root_expanded)
  
  d = tempfile.mkdtemp(prefix='pbtunerun_'+config.name+'_', dir=root_expanded)
  if not delete:
    print d
  global cur
  cur = StorageDirsTemplate(d)
  try:
    return fn()
  finally:
    if delete:
      shutil.rmtree(d)
    else:
      print d

candidate    = lambda cid:          cur.candidate(cid)
mutatorlog   = lambda m:            cur.mutatorlog(m)
inputpfx     = lambda size, number: cur.inputpfx(size, number)
clearInputs  = lambda :             cur.clearInputs()
openCsvStats = lambda name, header: cur.openCsvStats(name, header)
saveFile     = lambda path:         cur.saveFile(path)
relpath      = lambda d: _relpath(d, cur.root)

