import os, shutil, tempfile, csv
from tunerconfig import config

class StorageDirsTemplate:
  def __init__(self, root):
    self.root    = root
    self.configd = os.path.join(root, 'config')
    self.inputd  = os.path.join(root, 'inputs')
    self.resultd = os.path.join(root, 'results')
    self.statsd  = os.path.join(root, 'stats')
    os.mkdir(self.configd)
    os.mkdir(self.resultd)
    os.mkdir(self.statsd)
    os.mkdir(self.inputd)

  def config(self, cid):
    return os.path.join(self.configd, "candidate%05d.cfg" % cid)
  
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
  d = tempfile.mkdtemp(prefix='pbtunerun_', dir=root)
  if not delete:
    print d
  global storage_dirs
  storage_dirs = StorageDirsTemplate(d)
  try:
    fn()
  finally:
    if delete:
      shutil.rmtree(d)


configfile   = lambda cid:          storage_dirs.config(cid)
inputpfx     = lambda size, number: storage_dirs.inputpfx(size, number)
clearInputs  = lambda :             storage_dirs.clearInputs()
openCsvStats = lambda name, header: storage_dirs.openCsvStats(name, header)

