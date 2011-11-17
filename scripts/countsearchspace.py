#!/usr/bin/python

import re
import math

BASE = 10

def countsearchspace(f):
  c = 0
  for line in f:
    m = re.search("valid range: (.*) to (.*)", line)
    c += math.log(float(m.group(2)) - float(m.group(1)), BASE)
  return c

if __name__ == '__main__':
  import sys
  print "$%d^{%.2f}$"%(BASE, countsearchspace(open(sys.argv[1])))

