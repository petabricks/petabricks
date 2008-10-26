#!/usr/bin/python
#
# Generate a W by H matrix filled with random values between 0 and 99
#
# genmatrix.py [W] [H]
#

from sys import argv
import random

W=16
H=16
MAX_DOUBLE=9.00719925e15

if len(argv) >= 2:
  H=W=int(argv[1])  

if len(argv) >= 3:
  H=int(argv[2])  

print "SIZE",W,H

for y in xrange(H):
  for x in xrange(W):
    print "%.8f "%((2 * (random.random() - .5)) * MAX_DOUBLE),
  print

