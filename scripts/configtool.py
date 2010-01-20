#!/usr/bin/python
import re
import sys

CONFIGLINERE=re.compile("[ \t]*([a-z0-9_-]+)[ \t]*[=][ \t]*([0-9-]+)(.*)", re.IGNORECASE)
USAGE='''USAGE:
  configtool <FILE> set <key> <val>
  configtool <FILE> get <key>
  configtool <FILE> search <regexp>
  configtool <FILE> list
  configtool <FILE> print
'''

class ConfigFile:
  def __init__(self, filename = None):
    self.values = dict()
    if filename:
      self.load(filename)

  def load(self, filename):
    fd = open(filename)
    for n, line in enumerate(fd):
      try:
        key,val,com = CONFIGLINERE.match(line).group(1,2,3)
        self.add(key, val, com.strip())
      except:
        sys.stderr.write("WARNING: %s:%d -- failed to parse config line\n" % (filename,n))
    fd.close()

  def save(self, filename):
    fd = open(filename, "w")
    for k in self.values.keys():
      val, com = self.values[k]
      fd.write("%s = %d %s\n" % (k, val, com))
    fd.close()

  def __getitem__(self, k):
    return self.values[k][0]
  
  def __setitem__(self, k, v):
    self.values[k][0] = int(v)

  def add(self, k, v, com="# added in script"):
    self.values[k] = [int(v), com]

  def keys(self):
    return self.values.keys()

def getConfigVal(filename, key):
  try:
    return ConfigFile(filename)[key]
  except:
    return None

def setConfigVal(filename, key, val):
  cfg = ConfigFile() 
  try:
    cfg.load(filename)
  except:
    sys.stderr.write("WARNING: failed to load config file "+filename+"\n")
  try:
    cfg[key]=val
  except:
    sys.stderr.write("WARNING: missing val %s in %s\n" % (key, filename))
    cfg.add(key,val)
  cfg.save(filename)

def main(argv):
  #parse args
  try:
    IN=argv[1]
    OUT="/dev/null"
    cfg = ConfigFile(IN)
    i=2
    while i<len(argv):
      act=argv[i].lower()
      if act=="set":
        OUT=IN
        cfg[argv[i+1]] = argv[i+2]
        i+=3
      elif act=="get":
        print cfg[argv[i+1]]
        i+=2
      elif act=="search":
        r = re.compile(argv[i+1], re.IGNORECASE)
        for k in filter(lambda x: r.search(x), cfg.keys()):
          print k
        i+=2
      elif act=="list":
        for k in cfg.keys():
            print k
        i+=1
      elif act=="print":
        for k in cfg.keys():
            print k,'=',cfg[k]
        i+=1
      else:
        raise None
  except Exception, e:
    print e
    sys.stderr.write(USAGE)
  cfg.save(OUT)

if __name__ == "__main__":
  main(sys.argv)


