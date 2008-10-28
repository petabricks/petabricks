#!/usr/bin/python
#
# Generate a W by H matrix filled with random values between 0 and 99
#
# genmatrix.py [W] [H]
#

from sys import argv
import random
from math import floor

W=16

if len(argv) >= 2:
  W=int(argv[1])  

print "SIZE",W

for x in xrange(W):
  print "%.8g "%((2 * (floor(random.random() * 2) - .5)) * (2 ** (floor(random.random() * 2046) - 1022)) * (random.random() + 1)),
print

