import warnings

class TunerWarning(UserWarning):
  '''base class for warning types'''
  def __str__(self):
    return self.__class__.__name__

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

class AlwaysCrashes(TunerWarning):
  '''never found an initial candidate that didn't crash'''
  pass

class ProgramCrash(TunerWarning):
  '''base class for program crash warnings'''
  def __init__(self, crash):
    self.crash=crash
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
# warnings.simplefilter('error',  AlwaysCrashes)
# warnings.simplefilter('error',  ExistingProgramCrash)
# warnings.simplefilter('error',  InconsistentOutput)
# warnings.simplefilter('error',  NewProgramCrash)
warnings.simplefilter('ignore', ComparisonFailed)

