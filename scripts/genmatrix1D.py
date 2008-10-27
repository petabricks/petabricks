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
MAX_DOUBLE=9.00719925e15

if len(argv) >= 2:
  W=int(argv[1])  

print "SIZE",W

for x in xrange(W):
#  print "%.8f "%((2 * (random.random() - .5)) * MAX_DOUBLE),
  print "%.8g "%((2 * (floor(random.random() * 2) - .5)) * (2 ** (floor(random.random() * 2048) - 1023)) * (random.random() + 1)),
print

