import warnings

class TunerWarning(UserWarning):
  '''base class for warning types'''
  def __str__(self):
    return self.__class__.__name__

class UnknownTunableType(TunerWarning):
  '''the program contains a tunable type with no corresponding mutator'''
  def __init__(self, name, category):
    self.name = name
    self.category = category
  def __str__(self):
    return "%s(%s)"%(self.name, self.category)

class ComparisonFailed(TunerWarning):
  '''couldn't get enough statistical confidence to decide a comparison'''
  def __init__(self, n, a, b):
    self.n = n
    self.a = a
    self.b = b
  def __str__(self):
    return "%s ?= %s" % (str(self.a), str(self.b))

class InconsistentOutput(TunerWarning):
  '''two candidates produced different answers'''
  def __init__(self, a, b, pfx):
    self.a=a
    self.b=b
    self.pfx=pfx
  def __str__(self):
    return "%s != %s" % (str(self.a), str(self.b))

class TargetNotMet(TunerWarning):
  '''an accuracy target was not attainable through search'''
  def __init__(self, n, acc):
    self.n = n
    self.acc = acc
  def __str__(self):
    return "accuracy %.2f not met for input size %d"%(self.acc, self.n)

class ProgramTimeout(TunerWarning):
  '''an accuracy target was not attainable through search'''
  def __init__(self, candidate, n, timeout):
    self.candidate = candidate
    self.n = n
    self.timeout = timeout
  def __str__(self):
    return "%s timed out in %.2f sec on input %d"%(self.candidate, self.timeout, self.n)

class AlwaysCrashes(TunerWarning):
  '''never found an initial candidate that didn't crash'''
  pass

class ProgramCrash(TunerWarning):
  '''base class for program crash warnings'''
  def __init__(self, crash):
    self.crash=crash
    crash.debugpause()
  def __str__(self):
    return TunerWarning.__str__(self)+\
        ": %s crashed n=%d trial=%d" % (str(self.crash.candidate), self.crash.n, self.crash.testNumber)

class InitialProgramCrash(ProgramCrash):
  '''the seed algorithm (default config) for the population crashed'''
  pass
class ExistingProgramCrash(ProgramCrash):
  '''a program already in the population crashed'''
  pass
class NewProgramCrash(ProgramCrash):
  '''a mutated child algorithm crashed'''
  pass

warnings.simplefilter('error',  TunerWarning)
warnings.simplefilter('always', InitialProgramCrash)
warnings.simplefilter('always', TargetNotMet)
warnings.simplefilter('always', UnknownTunableType)
warnings.simplefilter('always', NewProgramCrash)
warnings.simplefilter('always', ProgramTimeout)
warnings.simplefilter('ignore', ComparisonFailed)

