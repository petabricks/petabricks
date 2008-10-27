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
VALS=range(100)

if len(argv) >= 2:
  H=W=int(argv[1])  

if len(argv) >= 3:
  H=int(argv[2])  

print "SIZE",W,H

for y in xrange(H):
  for x in xrange(W):
    if x==0 or x==W-1 or y==0 or y==H-1:
      print "%2d "%0,
    else:
      print "%2d "%random.choice(VALS),
  print

