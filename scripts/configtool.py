#!/usr/bin/python
import re
import sys

#raw options to parse
IN="/dev/stdin"
OUT="/dev/null"
FILTERS=[]

def getfilter(key):
  def f(k,v):
    if k==key:
      print v
    return k,v
  return f

def setfilter(key, val):
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

#parse args
try:
  IN=sys.argv[1]
  i=2
  while i<len(sys.argv):
    act=sys.argv[i].lower()
    if act=="set":
      OUT=IN
      FILTERS.append(setfilter(sys.argv[i+1], sys.argv[i+2]))
      i+=3
    elif act=="get":
      FILTERS.append(getfilter(sys.argv[i+1]))
      i+=2
    elif act=="search":
      FILTERS.append(searchfilter(sys.argv[i+1]))
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
  sys.stderr.write('''USAGE:
    configtool <FILE> set <key> <val>
    configtool <FILE> get <key>
    configtool <FILE> search <regexp>
    configtool <FILE> list
    configtool <FILE> print
''')
  sys.exit(1)

#preload entire file
lines=[x for x in open(IN)]

o=open(OUT, "w")
p=re.compile("[ \t]*([a-z0-9_-]+)[ \t]*[=][ \t]*([0-9-]+) (.*)", re.IGNORECASE)

#make output
for line in lines:
  try:
    key,val,com = p.match(line).group(1,2,3)
    for f in FILTERS:
      key,val=f(key,val)
    o.write("%s = %s %s\n" % (key, val, com)) 
  except:
    o.write(line);


