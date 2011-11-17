#!/usr/bin/env python

try:
  import ply
  import numpy
#  import scipy
except Exception,e:
  import sys
  print >>sys.stderr
  print >>sys.stderr, "="*60
  print >>sys.stderr, "PYTHON ERROR:", e
  print >>sys.stderr, "              perhaps you want to run:"
  print >>sys.stderr, "              sudo apt-get install python-{numpy,scipy,ply}"
  print >>sys.stderr, "="*60
  sys.exit(1)


