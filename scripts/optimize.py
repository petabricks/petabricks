#!/usr/bin/python
import numpy
from numpy import atleast_1d, eye, mgrid, argmin, zeros, shape, empty, \
     squeeze, vectorize, asarray, absolute, sqrt, Inf, asfarray, isinf
from scipy.optimize import linesearch, line_search, approx_fprime

# variables and functions taken from scipy.optimize:

_epsilon = sqrt(numpy.finfo(float).eps)

def vecnorm(x, ord=2):
    if ord == Inf:
        return numpy.amax(abs(x))
    elif ord == -Inf:
        return numpy.amin(abs(x))
    else:
        return numpy.sum(abs(x)**ord,axis=0)**(1.0/ord)

def wrap_function(function, args):
    ncalls = [0]
    def function_wrapper(x):
        ncalls[0] += 1
        return function(x, *args)
    return ncalls, function_wrapper

# custom fmin_bfgs (modified from scipy.optimize):

def my_fmin_bfgs(f, x0, fprime=None, args=(), gtol=1e-5, norm=Inf,
                 epsilon=_epsilon, maxiter=None, full_output=0, disp=1,
                 retall=0, callback=None):
    """Minimize a function using the BFGS algorithm.

    :Parameters:

      f : callable f(x,*args)
          Objective function to be minimized.
      x0 : ndarray
          Initial guess.
      fprime : callable f'(x,*args)
          Gradient of f.
      args : tuple
          Extra arguments passed to f and fprime.
      gtol : float
          Gradient norm must be less than gtol before succesful termination.
      norm : float
          Order of norm (Inf is max, -Inf is min)
      epsilon : int or ndarray
          If fprime is approximated, use this value for the step size.
      callback : callable
          An optional user-supplied function to call after each
          iteration.  Called as callback(xk), where xk is the
          current parameter vector.

    :Returns: (xopt, {fopt, gopt, Hopt, func_calls, grad_calls, warnflag}, <allvecs>)

        xopt : ndarray
            Parameters which minimize f, i.e. f(xopt) == fopt.
        fopt : float
            Minimum value.
        gopt : ndarray
            Value of gradient at minimum, f'(xopt), which should be near 0.
        Bopt : ndarray
            Value of 1/f''(xopt), i.e. the inverse hessian matrix.
        func_calls : int
            Number of function_calls made.
        grad_calls : int
            Number of gradient calls made.
        warnflag : integer
            1 : Maximum number of iterations exceeded.
            2 : Gradient and/or function calls not changing.
        allvecs  :  list
            Results at each iteration.  Only returned if retall is True.

    *Other Parameters*:
        maxiter : int
            Maximum number of iterations to perform.
        full_output : bool
            If True,return fopt, func_calls, grad_calls, and warnflag
            in addition to xopt.
        disp : bool
            Print convergence message if True.
        retall : bool
            Return a list of results at each iteration if True.

    :Notes:

        Optimize the function, f, whose gradient is given by fprime
        using the quasi-Newton method of Broyden, Fletcher, Goldfarb,
        and Shanno (BFGS) See Wright, and Nocedal 'Numerical
        Optimization', 1999, pg. 198.

    *See Also*:

      scikits.openopt : SciKit which offers a unified syntax to call
                        this and other solvers.

    """
    x0 = asarray(x0).squeeze()
    if x0.ndim == 0:
        x0.shape = (1,)
    if maxiter is None:
        maxiter = len(x0)*200
    func_calls, f = wrap_function(f, args)
    if fprime is None:
        grad_calls, myfprime = wrap_function(approx_fprime, (f, epsilon))
    else:
        grad_calls, myfprime = wrap_function(fprime, args)
    print "Evaluating initial gradient ..."
    gfk = myfprime(x0)
    k = 0
    N = len(x0)
    I = numpy.eye(N,dtype=int)
    Hk = I
    old_fval = f(x0)
    old_old_fval = old_fval + 5000
    xk = x0
    if retall:
        allvecs = [x0]
    sk = [2*gtol]
    warnflag = 0
    gnorm = vecnorm(gfk,ord=norm)
    print "gtol  = %g" % gtol
    print "gnorm = %g" % gnorm
    while (gnorm > gtol) and (k < maxiter):
        print "Begin iteration %d line search..." % (k + 1)
        print "  gfk =", gfk
        print "  Hk = \n", Hk
        pk = -numpy.dot(Hk,gfk)
        print "  pk =", pk
        alpha_k, fc, gc, old_fval, old_old_fval, gfkp1 = \
           linesearch.line_search(f,myfprime,xk,pk,gfk,
                                  old_fval,old_old_fval)
        if alpha_k is None:  # line search failed try different one.
            print "Begin line search (method 2) ..."
            alpha_k, fc, gc, old_fval, old_old_fval, gfkp1 = \
                     line_search(f,myfprime,xk,pk,gfk,
                                 old_fval,old_old_fval)
            if alpha_k is None:
                # This line search also failed to find a better solution.
                print "Line search failed!"
                warnflag = 2
                break
        print "End line search, alpha = %g ..." % alpha_k

        xkp1 = xk + alpha_k * pk
        if retall:
            allvecs.append(xkp1)
        sk = xkp1 - xk
        xk = xkp1
        if gfkp1 is None:
            gfkp1 = myfprime(xkp1)

        yk = gfkp1 - gfk
        gfk = gfkp1
        if callback is not None:
            callback(xk)
        k += 1
        gnorm = vecnorm(gfk,ord=norm)
        print "gnorm = %g" % gnorm
        if (gnorm <= gtol):
            print "  tolerance reached!"
            break

        try: # this was handled in numeric, let it remaines for more safety
            rhok = 1.0 / (numpy.dot(yk,sk))
        except ZeroDivisionError:
            rhok = 1000.0
            print "Divide-by-zero encountered: rhok assumed large"
        if isinf(rhok): # this is patch for numpy
            rhok = 1000.0
            print "Divide-by-zero encountered: rhok assumed large"
        A1 = I - sk[:,numpy.newaxis] * yk[numpy.newaxis,:] * rhok
        A2 = I - yk[:,numpy.newaxis] * sk[numpy.newaxis,:] * rhok
        Hk = numpy.dot(A1,numpy.dot(Hk,A2)) + rhok * sk[:,numpy.newaxis] \
                 * sk[numpy.newaxis,:]

    if disp or full_output:
        fval = old_fval
    if warnflag == 2:
        if disp:
            print "Warning: Desired error not necessarily achieved" \
                  "due to precision loss"
            print "         Current function value: %f" % fval
            print "         Iterations: %d" % k
            print "         Function evaluations: %d" % func_calls[0]
            print "         Gradient evaluations: %d" % grad_calls[0]

    elif k >= maxiter:
        warnflag = 1
        if disp:
            print "Warning: Maximum number of iterations has been exceeded"
            print "         Current function value: %f" % fval
            print "         Iterations: %d" % k
            print "         Function evaluations: %d" % func_calls[0]
            print "         Gradient evaluations: %d" % grad_calls[0]
    else:
        if disp:
            print "Optimization terminated successfully."
            print "         Current function value: %f" % fval
            print "         Iterations: %d" % k
            print "         Function evaluations: %d" % func_calls[0]
            print "         Gradient evaluations: %d" % grad_calls[0]

    if full_output:
        retlist = xk, fval, gfk, Hk, func_calls[0], grad_calls[0], warnflag
        if retall:
            retlist += (allvecs,)
    else:
        retlist = xk
        if retall:
            retlist = (xk, allvecs)

    return retlist


class BFGSOptimizer:

  def __init__(self, f, size, minVal, maxVal):
    self.f = f
    self.size = size
    self.minVal = minVal
    self.maxVal = maxVal

  def optimize(self, x0, args = (), maxiter = None):

    # optimize
    result = my_fmin_bfgs(self.f, x0, args = args, maxiter = maxiter)

    # convert result to list of floats and enforce min and max
    newVal = list(result)
    for j in xrange(0, self.size):
      newVal[j] = min(self.maxVal[j], max(self.minVal[j], float(newVal[j])))

    return newVal

class CachedBFGSOptimizer(BFGSOptimizer):

  def __init__(self, f, size, minVal, maxVal):
    BFGSOptimizer.__init__(self, f, size, minVal, maxVal)
    self.bareF = f
    self.f = self.cachedF
    self.clearCache()

  def clearCache(self):
    self.cache = {}

  def cachedF(self, *args):
    print args
    key = tuple(map(float, args[0]))
    if key in self.cache:
      result = self.cache[key]
      print "cache hit: f(", args, ") =", result
    else:
      print "cache miss: f(", args, ")"
      result = self.bareF(*args)
      self.cache[key] = result
      print "cache stored: f(", args, ") =", result
    return result

  def optimize(self, x0, args = (), maxiter = None):

    # clear cache before optimization run
    self.clearCache()

    return BFGSOptimizer.optimize(self, x0, args = args, maxiter = maxiter)
