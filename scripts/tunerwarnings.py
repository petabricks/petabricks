import warnings
from tunerconfig import config

class TunerWarning(UserWarning):
  '''base class for warning types'''
  def __str__(self):
    return self.__class__.__name__

class IgnoredTunerWarning(TunerWarning):
  '''base for those ignored by default'''

class FatalTunerWarning(TunerWarning):
  '''base for those that crash the program by default'''

warnings.simplefilter('always',  TunerWarning)
warnings.simplefilter('error',   FatalTunerWarning)
warnings.simplefilter('ignore',  IgnoredTunerWarning)

class UnknownTunableType(TunerWarning):
  '''the program contains a tunable type with no corresponding mutator'''
  def __init__(self, name, category):
    self.name = name
    self.category = category
  def __str__(self):
    return "%s(%s)"%(self.name, self.category)

class ComparisonFailed(IgnoredTunerWarning):
  '''couldn't get enough statistical confidence to decide a comparison'''
  def __init__(self, n, a, b):
    self.n = n
    self.a = a
    self.b = b
  def __str__(self):
    return "%s ?= %s" % (str(self.a), str(self.b))

class ComparisonSkipped(ComparisonFailed):
  pass

class InconsistentOutput(TunerWarning):
  '''two candidates produced different answers'''
  def __init__(self, a, b, pfx):
    self.a=a
    self.b=b
    self.pfx=pfx
    self.debugpause()
  def __str__(self):
    return "%s != %s" % (str(self.a), str(self.b))
  def debugpause(self):
    if config.pause_on_crash:
      print '-'*60
      print 'WARNING: Inconsistent output'
      print '-'*60
      print "config 1:", self.a.cfgfile()
      print "config 2:", self.b.cfgfile()
      print "input:",self.pfx
      print 'set config.pause_on_crash=False to disable this message'
      print '-'*60
      print
      import progress
      progress.pause('press any key to continue')

class TargetNotMet(TunerWarning):
  '''an accuracy target was not attainable through search'''
  def __init__(self, n, acc):
    self.n = n
    self.acc = acc
  def __str__(self):
    return "accuracy %.2f not met for input size %d"%(self.acc, self.n)

class ProgramTimeout(IgnoredTunerWarning):
  '''an accuracy target was not attainable through search'''
  def __init__(self, candidate, n, timeout):
    self.candidate = candidate
    self.n = n
    self.timeout = timeout
  def __str__(self):
    return "%s timed out in %.2f sec on input %d"%(self.candidate, self.timeout, self.n)

class MutateFailed(IgnoredTunerWarning):
  def __init__(self, candidate, n, tries):
    self.candidate = candidate
    self.n = n
    self.tries = tries
  def __str__(self):
    return "%s failed to mutate in %d tries on input %d"%(self.candidate, self.tries, self.n)

class AlwaysCrashes(FatalTunerWarning):
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

class NanAccuracy(TunerWarning):
  pass

class TooManyTrials(TunerWarning):
  '''base class for program crash warnings'''
  def __init__(self, trial):
    self.trials=trial
  def __str__(self):
    return TunerWarning.__str__(self)+\
        ": trials=%d" % self.trials


