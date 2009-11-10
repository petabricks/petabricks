#!/usr/bin/python
import re
import sys

CONFIGLINERE=re.compile("[ \t]*([a-z0-9_-]+)[ \t]*[=][ \t]*([0-9-]+) (.*)", re.IGNORECASE)

USAGE='''USAGE:
  configtool <FILE> set <key> <val>
  configtool <FILE> get <key>
  configtool <FILE> search <regexp>
  configtool <FILE> list
  configtool <FILE> print
'''

def getfilter(key):
  key=key.encode('ascii')
  def f(k,v):
    if k==key:
      print v
    return k,v
  return f

def setfilter(key, val):
  key=key.encode('ascii')
  def f(k,v):
    if k==key:
      v=val
    return k,v
  return f

def listfilter():
  def f(k,v):
    print k
    return k,v
  return f

def printfilter():
  def f(k,v):
    print k,'=',v
    return k,v
  return f

def searchfilter(term):
  printer=printfilter()
  pat=re.compile(term)
  def f(k,v):
    m=pat.search(k)
    if m:
      printer(k,v)
    return k,v
  return f

def processConfigFile(IN, OUT, FILTERS):
  #preload entire file
  lines=[x for x in open(IN)]
  o=open(OUT, "w")
  #make output
  for line in lines:
    try:
      key,val,com = CONFIGLINERE.match(line).group(1,2,3)
      for f in FILTERS:
        key,val=f(key,val)
      o.write("%s = %s %s\n" % (key, val, com)) 
    except:
      o.write(line); 

def getConfigVal(file, key):
  val=[]
  def tmp(k,v):
    if k==key:
      val.append(v)
    return k,v
  processConfigFile(file, "/dev/null", [tmp])
  if len(val)>0:
    return val[0]
  else:
    return None

def setConfigVal(file, key, val):
  processConfigFile(file, file, [setfilter(key, val)])

def main(argv):
  IN="/dev/stdin"
  OUT="/dev/null"
  FILTERS=[]
  #parse args
  try:
    IN=argv[1]
    i=2
    while i<len(argv):
      act=argv[i].lower()
      if act=="set":
        OUT=IN
        FILTERS.append(setfilter(argv[i+1], argv[i+2]))
        i+=3
      elif act=="get":
        FILTERS.append(getfilter(argv[i+1]))
        i+=2
      elif act=="search":
        FILTERS.append(searchfilter(argv[i+1]))
        i+=2
      elif act=="list":
        FILTERS.append(listfilter())
        i+=1
      elif act=="print":
        FILTERS.append(printfilter())
        i+=1
      else:
        raise None
  except:
    sys.stderr.write(USAGE)
  processConfigFile(IN, OUT, FILTERS)


if __name__ == "__main__":
  main(sys.argv)


