#!/usr/bin/python

import csv, sys
import numpy

dialect = csv.excel_tab
multi_file=len(sys.argv[1:])>1

inputs = map(lambda x: csv.DictReader(x, dialect=dialect), map(open, sys.argv[1:]))
rows = map(csv.DictReader.next, inputs)
headers = inputs[0].fieldnames
output = csv.writer(sys.stdout, dialect=dialect)
output.writerow(headers)

def mkavg(k):
  try:
    values = map(lambda x: float(x[k]), rows)
    return "%f +- %f" % (numpy.mean(values), numpy.std(values))
  except:
    return 'error'

if multi_file:
  try:
    while True:
      output.writerow(map(mkavg, headers))
      rows = map(csv.DictReader.next, inputs)
  except StopIteration:
    pass
else:
  counts=dict()
  sums=dict()
  for k in headers:
    try:
      sums[k]=float(rows[0][k])
    except:
      sums[k]=0.0
    counts[k]=1.0
  for row in inputs[0]:
    for k in headers:
      try:
        sums[k]+=float(row[k])
      except:
        sums[k]=0.0
      counts[k]+=1.0


  output.writerow(map(lambda k: sums[k]/counts[k], headers))


