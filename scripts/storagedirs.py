import os, shutil, tempfile, csv
from tunerconfig import config

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

