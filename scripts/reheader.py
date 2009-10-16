#!/usr/bin/python

import os
import sys
import re
from pprint import pprint 

template=sys.argv[1]
paths=sys.argv[2:]

for p in paths:
  for d,sd,sf in os.walk(p):
    for f in map(lambda x: os.path.join(d, x), sf):
      paths.append(f)

exts=["h","c","hpp","cpp","cxx","cc"]
mexts=["ypp","lpp","y","l"]
srcfile  = re.compile("^(.*[.])("+'|'.join(exts)+")$")
metafile = re.compile("^(.*[.])("+'|'.join(mexts)+")$")
pfxcomment = re.compile("^([/][*]([^*]|[*][^/])*[*][/])")

boxtopbot = re.compile("^[/*]+$")
isboxtopbot = lambda s: boxtopbot.match(s) is not None
boxrow = re.compile("^[*].*[*]$")
isboxrow = lambda s: boxrow.match(s) is not None

def pfxtest(f):
  s=open(f).read(1024*1024) #only check first 1kb
  m=pfxcomment.match(s)
  if m is None:
    print "no file prefix "+f
    return False
  lines=map(lambda s: s.strip(), m.group(1).split('\n'))
  if all(map(isboxrow, lines[1:-1])) and isboxtopbot(lines[0]) and isboxtopbot(lines[-1]):
    return True
  else:
    print "invalid file prefix "+f
    return False
  
def filenametest(f):
  if srcfile.match(f) is not None:
    pfx=srcfile.match(f).group(1)
    for e in mexts:
      if pfx+e in paths:
        return False
    return True
  elif metafile.match(f) is not None:
    return True
  else:
    return False

isvalidsrcfile = lambda f: os.path.isfile(f) and filenametest(f) and pfxtest(f)

assert os.path.isfile(template)
assert pfxtest(template)
template=open(template).read().strip()
assert pfxcomment.match(template).group(0)==template

paths.sort()
paths = filter(isvalidsrcfile, paths)

for path in paths:
  str = open(path).read()
  str = pfxcomment.sub(template, str)
  open(path,"w").write(str)

