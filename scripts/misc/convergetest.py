#!/usr/bin/python

import csv, sys
import numpy
import warnings 
from scipy.stats import ttest_ind
warnings.simplefilter('ignore',DeprecationWarning)

KEYT='# time'
KEYV='minperf'


def convergeExtract(fname):
  fd = open(fname)
  dr = csv.DictReader(fd)
  values = map(lambda x: (float(x[KEYT]), float(x[KEYV])), dr)
  minperf=min(*map(lambda x: x[1], values))
  for t,p in values:
    if p<1.05*minperf:
      return t, minperf

a=None
b=[]

for f in sys.argv[1:]:
  if f=='--':
    a=b
    b=[]
  else:
    b.append(convergeExtract(f))

atimes = map(lambda x: x[0], a)
btimes = map(lambda x: x[0], b)
aperf  = map(lambda x: x[1], a)
bperf  = map(lambda x: x[1], b)

print "TIME:", numpy.mean(atimes), numpy.mean(btimes), ttest_ind(atimes, btimes)[1]
print "TIME+-:", numpy.std(atimes), numpy.std(btimes)
print "PERF:", numpy.mean(aperf), numpy.mean(bperf), ttest_ind(aperf, bperf)[1]
print "PERF+-:", numpy.std(aperf), numpy.std(bperf)

