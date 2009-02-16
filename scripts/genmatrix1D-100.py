#!/usr/bin/python
#
# Generate a W by H matrix filled with random values between 0 and 99
#
# genmatrix.py [W] [H]
#

from sys import argv
import random

W=16
VALS=range(100)

if len(argv) >= 2:
  W=int(argv[1])  

print "SIZE",W

for x in xrange(W):
  print "%2d "%random.choice(VALS),

