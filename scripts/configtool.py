#!/usr/bin/python
import re, sys, os, tempfile, subprocess, logging

CONFIGLINERE=re.compile("[ \t]*([a-z0-9_-]+)[ \t]*[=][ \t]*([.0-9-]+)(.*)", re.IGNORECASE)
USAGE='''USAGE:
  configtool <FILE> set <key> <val>
  configtool <FILE> get <key>
  configtool <FILE> search <regexp>
  configtool <FILE> list
  configtool <FILE> print
'''

class ConfigFile:
  '''manages a config file with dict()-like semantics, no further documentation needed'''
  def __init__(self, src):
    if type(src) == type(self):
      '''copy constructor'''
      self.values = dict(src.values)
    elif type(src) == type(""):
      '''construct from file'''
      self.values = dict()
      self.load(src)
    else:
      raise Exception("invalid arg:"+src)

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
    for k,valcom in sorted(self.values.iteritems()):
      val, com = valcom
      if type(val) is type(0.1):
        fd.write("%s = %.25e %s\n" % (k, val, com))
      else:
        fd.write("%s = %d %s\n" % (k, val, com))
    fd.close()
        
  def __str__(self):
    return "\n".join(map(lambda x: "%s = %d"%(x[0],x[1][0]), sorted(self.values.items())))

  def __getitem__(self, k):
    return self.values[k][0]
  
  def __setitem__(self, k, v):
    #logging.debug("configtool: changing %s from %d to %d", k, self[k], v)
    com=self.values[k][1]
    if type(v) is type(0.1) or "double" in com or "float" in com:
      self.values[k] = (float(v), com)
    else:
      self.values[k] = (int(v), com)

  def __hash__(self):
    return hash(str(self))
  
  def __cmp__(a, b):
    return cmp(a.values, b.values)

  def add(self, k, v, com="# added in script"):
    if type(v) is type(0.1) or "double" in com or "float" in com:
      self.values[k] = (float(v), com)
    else:
      self.values[k] = (int(v), com)

  def keys(self):
    return self.values.keys()

def defaultConfigFile(bin):
  fd, name = tempfile.mkstemp(suffix='.cfg')
  os.close(fd)
  cmd = [bin, '--config='+name, '--reset']
  subprocess.check_call(cmd)
  cfg = ConfigFile(name)
  os.unlink(name)
  return cfg

def getConfigVal(filename, key):
  '''legacy entry point to this file'''
  try:
    return ConfigFile(filename)[key]
  except:
    return None

def setConfigVal(filename, key, val):
  '''legacy entry point to this file'''
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
    OUT=None
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
        print str(cfg)
        i+=1
      else:
        raise None
    if OUT:
      cfg.save(OUT)
  except Exception, e:
    print e
    sys.stderr.write(USAGE)

if __name__ == "__main__":
  main(sys.argv)


