#!/usr/bin/python

import csv, sys
import numpy

inputs = map(csv.DictReader, map(open, sys.argv[1:]))
rows = map(csv.DictReader.next, inputs)
headers = inputs[0].fieldnames
output = csv.writer(sys.stdout)
output.writerow(headers)

def mkavg(k):
  try:
    values = map(lambda x: float(x[k]), rows)
    return "%f +- %f" % (numpy.mean(values), numpy.std(values))
  except:
    return 'error'

try:
  while True:
    output.writerow(map(mkavg, headers))
    rows = map(csv.DictReader.next, inputs)
except StopIteration:
  pass


