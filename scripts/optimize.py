#!/usr/bin/python
import scipy.optimize

class BFGSOptimizer:

  def __init__(self, f, size, minVal, maxVal):
    self.f = f
    self.size = size
    self.minVal = minVal
    self.maxVal = maxVal

  def optimize(self, x0, candidate):

    # optimize
    args = (candidate, candidate.pop.testers[-1].n)
    result = scipy.optimize.fmin_bfgs(self.f, x0, args = args, full_output = 1)

    newVal = list(result[0])
    for j in xrange(0, self.size):
      newVal[j] = float(newVal[j])

    # enforce min and max
    for j in xrange(0, self.size):
      newVal[j] = min(self.maxVal[j], max(self.minVal[j], newVal[j]))

    return newVal

