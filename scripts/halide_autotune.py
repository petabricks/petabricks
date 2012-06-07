# Halide autotuning with PetaBricks

from highlevelconfig import *
import candidatetester
from candidatetester import CandidateTester, CrashException
from sgatuner import Population
from tunerconfig import config
from tunerwarnings import InitialProgramCrash,ExistingProgramCrash,NewProgramCrash,TargetNotMet
import pbutil
import operator
import warnings
import sgatuner
from configtool import ConfigFile
from storagedirs import timers

#from sgatuner import *
#import highlevelconfig
#import sgatuner

FUNC_ROOT   = 0
FUNC_INLINE = 1
FUNC_CHUNK  = 2         # Needs a variable in the caller
# Chunk not implemented yet

VAR_SERIAL             = 0
VAR_VECTORIZE          = 1
VAR_PARALLEL           = 2
VAR_UNROLL             = 3
VAR_TILE               = 4
VAR_SPLIT              = 5
# Tile and split not implemented yet (recursion not implemented). Also vectorize() and unroll() implicitly create a new variable so they recurse also.
# transpose() always there or sometimes not there
#
# unroll 2+
# vectorize up to 32

FUNC_MAX      = 2
VAR_MAX       = 4

class HalideConfigAccessor(ConfigFile):          # Allows setting of keys without calling .add() first
    def __init__(self, src=None):
        if src is None:
            self.values = {}
        else:
            self.values = src.values         
        
    def __setitem__(self, k, v):
        if not k in self.values:
            self.add(k, v)
        else:
            ConfigFile.__setitem__(self, k, v)

def permutation(L, i):
    L = list(L)
    ans = []
    for j in range(len(L)):
        k = i%len(L)
        ans.append(L[k])
        i = i / len(L)
        del L[k]
    return ans

def factorial(n):
    return reduce(operator.mul, range(1, n+1))

def pairwise_swaps(L, Lp):
    L = list(L)
    # Turn L into Lp using pairwise swaps (transposes)
    #print 'pairwise_swaps', L, Lp
    ans = []
    for i in range(len(L)):
        if L[i] != Lp[i]:
            j = Lp.index(L[i])
            #assert j < len(L), (L, Lp, i)
            ans.append((L[i], L[j]))
            (L[i], L[j]) = (L[j], L[i])
    return ans
    
def check_pairwise_swaps(L0):
    assert pairwise_swaps(L0, L0) == []
    for i in range(factorial(len(L0))):
        Lp = permutation(L0, i)
        L = list(L0)
        for (a, b) in pairwise_swaps(L0, Lp):
            ai = L.index(a)
            bi = L.index(b)
            assert ai != bi
            (L[ai], L[bi]) = (L[bi], L[ai])
    
def test_permutation():
    for n in range(1, 8):
        for j in range(10):
            L = [2*x+1 for x in range(n)]
            random.shuffle(L)
            #print L
            assert permutation(L, 0) == L, (permutation(L, 0), L)
            check_pairwise_swaps(L)
            nfac = factorial(n)
            setL = set(tuple(permutation(L,i)) for i in range(nfac))
            assert len(setL) == nfac, (n, L, nfac, len(setL), setL)
            
            
    print 'permutation: OK'

class FuncSchedule(Item):
    def __init__(self, info, name, vars):
        self.vars = vars
        Item.__init__(self, info, name)

    def copyValues(self, src, dst):
        if src is dst:
            return
        for t in src.values: #self.tunables():
            dst[t] = src[t]

    def subname(self, sub):
        return self.name + '_' + sub
        
    def randomize(self, cfg, n):
        cfg = HalideConfigAccessor(cfg)
        func = cfg[self.subname('func')] = random.randrange(FUNC_MAX)
        if func == FUNC_ROOT:
            pass
        elif func == FUNC_INLINE:
            pass
        elif func == FUNC_CHUNK:
            raise NotImplementedError
        else:
            raise ValueError('Unknown func schedule %d' % func)
        
        for i in range(len(self.vars)):
            cfg[self.subname('var%d_arg0'%i)] = 0
            
            var = cfg[self.subname('var%d'%i)] = random.randrange(VAR_MAX)
            if var == VAR_VECTORIZE:
                cfg[self.subname('var%d_arg0'%i)] = random.choice([2, 4, 8, 16, 32])
            elif var == VAR_UNROLL:
                cfg[self.subname('var%d_arg0'%i)] = random.choice([2, 4, 8, 16, 32, 64])
            elif var in [VAR_SERIAL, VAR_PARALLEL]:
                pass
            elif schedule in [VAR_TILE, VAR_SPLIT]:
                pass
            else:
                raise ValueError('Unknown var schedule %d' % var)

        cfg[self.subname('transpose')] = (0 if random.random() <= (2.0/3.0) else random.randrange(1,factorial(len(self.vars))))
        
    def str(self, cfg):
        func = cfg[self.subname('func')]
        if func == FUNC_ROOT:
            ans = '%s.root()' % self.name
        elif func == FUNC_INLINE:
            return '%s.inline()' % self.name
        else:
            raise ValueError('Unknown func schedule %d' % func)
        
        for i in range(len(self.vars)):
            var = cfg[self.subname('var%d'%i)]
            if var == VAR_SERIAL:
                pass
            elif var in [VAR_VECTORIZE, VAR_UNROLL]:
                ans += '.%s(%s, %d)' % ('vectorize' if var == VAR_VECTORIZE else 'unroll', self.vars[i], cfg[self.subname('var%d_arg0'%i)])
            elif var == VAR_PARALLEL:
                ans += '.parallel(%s)' % (self.vars[i])
            else:
                raise ValueError('Unknown var schedule %d' % var)
        
        transpose = cfg[self.subname('transpose')]
        if transpose > 0:
            #print transpose, self.vars, permutation(self.vars, transpose)
            #assert self.vars == ['x', 'y'], self.vars
            for (a, b) in pairwise_swaps(self.vars, permutation(self.vars, transpose)):
                ans += '.transpose(%s,%s)' % (a, b)
        return ans

class HalideHighLevelConfig(HighLevelConfig):
    def __init__(self, info, func_var_list):
        self.items = []
        
        for (func, vars) in func_var_list:
            self.items.append(FuncSchedule(info, func, vars)) #, 0, SCHEDULE_MAX))
    
    def randomize(self, cfg, n):
        for item in self.items:
            item.randomize(cfg, n)
            
    def str(self, cfg):
        return '\n'.join([x.str(cfg) for x in self.items])

def runCommand(cmd, cfg):
    print 'run |%s|, %s' % (cmd, ' '.join(str(x) for x in cfg.values.items()))
    return pbutil.executeRun(cmd, config.metrics)
    
class HalideCandidateTester: #(CandidateTester):
    def __init__(self, app, n):
        self.app = app
        self.n = n + config.offset
        self.timeoutCount = 0
        self.crashCount = 0
        self.testCount = 0
    
    def nextTester(self):
        return self
        
    def cleanup(self):
        pass
        
    def test(self, candidate, limit=None):
        self.testCount += 1
        cfgfile = candidate.cfgfile()
        testNumber = candidate.numTests(self.n)
        if testNumber>=config.max_trials:
            warnings.warn(tunerwarnings.TooManyTrials(testNumber+1))
#    cmd = list(self.cmd)
#    cmd.append("--config="+cfgfile)
#    cmd.extend(timers.inputgen.wrap(lambda:self.getInputArg(testNumber)))
#    if limit is not None:
#      cmd.append("--max-sec=%f"%limit)
#    cmd.extend(getMemoryLimitArgs())
        cfg = ConfigFile(cfgfile)
        try:
            results = timers.testing.wrap(lambda: runCommand(self.app, cfg))
            for i,result in enumerate(results):
                if result is not None:
                    v=result['average']
                    if numpy.isnan(v) or numpy.isinf(v):
                        warnings.warn(tunerwarnings.NanAccuracy())
                        raise pbutil.TimingRunFailed(None)
                    candidate.metrics[i][self.n].add(v)
            return True
        except pbutil.TimingRunTimeout:
            assert limit is not None
            warnings.warn(tunerwarnings.ProgramTimeout(candidate, self.n, limit))
            candidate.metrics[config.timing_metric_idx][self.n].addTimeout(limit)
            self.timeoutCount += 1
            return False
        except pbutil.TimingRunFailed, e:
            self.crashCount += 1
            raise CrashException(testNumber, self.n, candidate, self.app)

class HalidePopulation(sgatuner.Population):  
  def test(self, count):
    '''test each member of the pop count times'''
    self.failed=set()
    tests = []
    for z in xrange(count):
      tests.extend(self.members)
    random.shuffle(tests)
    for m in tests:
      sgatuner.check_timeout()
      if m not in self.failed and m.numTests(self.inputSize())<config.max_trials:
        try:
          self.testers[-1].test(m)
        except candidatetester.CrashException, e:
          if m.numTotalTests()==0:
            warnings.warn(InitialProgramCrash(e))
          else:
            warnings.warn(ExistingProgramCrash(e))
          self.failed.add(m)
          self.members.remove(m)


def test():
    info = {}
    cfg = {}
    c = HalideHighLevelConfig(info, [('blur_x', ['x', 'y']),
                                     ('blur_y', ['x', 'y'])])
    seen = set()
    for i in range(20):
        for item in c.items:
            item.randomize(cfg, 1)
        s = c.str(cfg)
        print s
        print s in seen
        print
        seen.add(s)

def main():
    info = {}
    cfg = HalideConfigAccessor()
    hl = HalideHighLevelConfig(info, [('blur_x', ['x', 'y']),
                                      ('blur_y', ['x', 'y'])])
    hl.randomize(cfg, 1)
        
    
    test_permutation()

    sgatuner.main(tester_lambda=HalideCandidateTester, pop_lambda=HalidePopulation,
                  hlconfig_lambda=lambda: hl, config_lambda=lambda: cfg)
    
if __name__ == '__main__':
    main()
    