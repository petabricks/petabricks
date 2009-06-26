#!/usr/bin/python

import re
import sys
import os
import getopt
import subprocess
from xml.dom.minidom import parse
from pprint import pprint

def tryAorB(A, B):
  def tryAorBinst(x):
    try:
      return A(x)
    except:
      return B(x)
  return tryAorBinst

#attempt to convert to an int or float
tryIntFloat = tryAorB(int, tryAorB(float, lambda x: x))

#parse timing results
def executeTimingRun(prog, n, args=[]):
  cmd = [ prog, "--n=%d"%n, "--time" ]
  for x in args:
    cmd.append(x);
  p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
  os.waitpid(p.pid, 0)
  rslt = parse(p.stdout)
  rslt = rslt.getElementsByTagName("timing")[0].attributes
  attrs=dict()
  for x in xrange(rslt.length):
    attrs[str(rslt.item(x).name)]=tryIntFloat(rslt.item(x).nodeValue)
  return attrs

if __name__ == "__main__":
  #test executeTimingRun
  pprint(executeTimingRun("./examples/add", 1000, ["--trials=10"]))

