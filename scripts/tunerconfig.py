
class OperatorSelectionMethod:
  UNIFORM_RANDOM = 1
  WEIGHTED_SUM   = 2
  ROC_AREA       = 3
  ROULETTE       = 4
  ABS_ROC        = 5

class config_defaults:
  #how long to train for
  max_input_size           = 2**30
  min_input_size           = 1
  max_time                 = 60*150
  rounds_per_input_size    = 3
  final_rounds             = 3

  #number of trials to run
  confidence_pct   = 0.75
  min_trials       = 3
  max_trials       = 7
  '''guessed stddev when only 1 test is taken'''
  prior_stddev_pct      = 0.15
  '''percentage change to be viewed as insignificant when testing if two algs are equal'''
  same_threshold_pct    = 0.02
  '''confidence for generating execution time limits'''
  limit_conf_pct        = 0.95
  '''multiply generated time limits by a factor'''
  limit_multiplier      = 6.0
  '''offset added to input sizes'''
  offset                = 0

  window_size           = 50
  bandit_c              = .5
  bandit_verbose        = False
  os_method             = OperatorSelectionMethod.WEIGHTED_SUM


  #how mutation to do
  mutations_per_mutator    = 3
  population_high_size     = 10
  population_low_size      = 1
  multimutation            = True
  mutate_retries           = 10
  rand_retries             = 10

  #storage and reporting
  debug                  = False
  output_dir             = "~/tunerout"
  min_input_size_nocrash = 32
  delete_output_dir      = False
  print_log              = True
  candidatelog           = True
  mutatorlog             = True
  pause_on_crash         = False
  '''confidence intervals when displaying numbers'''
  display_confidence    = 0.90
  '''print raw timing values instead of mean and confidence interval'''
  print_raw             = False
  '''store random inputs and share between runs'''
  use_iogen             = True
  '''delete iogen inputs at the end of each round'''
  cleanup_inputs        = True
  '''check output hash against peers, requires use_iogen'''
  check                 = False

  name=''
  score_decay = 0.9
  bonus_round_score = 1.0
  memory_limit_pct = 0.8
  min_std_pct = 0.000001
  online_baseline = False
  fixed_safe_alg = False
  accuracy_target = None
  output_cfg = None
  timing_target = None
  n = None
  seed = None
  main = None
  max_gen = None
  race_multiplier = 1.0
  race_multiplier_lowacc = 8.0
  reweight_interval = 4
  threads = None
  abort_on = ""
  race_split_ratio = 0.5

  threshold_multiplier_min = 100.0
  threshold_multiplier_max = 1000.0
  threshold_multiplier_default=400.0

  recompile = True

  #types of mutatators to generate
  lognorm_tunable_types       = ['system.cutoff.splitsize', 'system.cutoff.sequential', 'system.cutoff.distributed', 'system.flag.localmem', 'system.size.blocksize']
  uniform_tunable_types       = []
  autodetect_tunable_types    = ['user.tunable']
  lognorm_sizespecific_tunable_types = ['user.tunable.accuracy.array', 'system.tunable.accuracy.array', 'user.tunable.array']
  optimize_tunable_types      = ['user.tunable.double', 'user.tunable.double.array']
  sizespecific_tunable_types  = ['user.tunable.accuracy.array', 'system.tunable.accuracy.array', 'user.tunable.double.array', 'user.tunable.array']
  ignore_tunable_types        = ['algchoice.cutoff', 'algchoice.alg', 'system.runtime.threads', 'system.flag.unrollschedule']
  
  #metric information, dont change
  metrics               = ['timing', 'accuracy']
  metric_orders         = [1, -1] #1 = minimize, -1 = maximize
  timing_metric_idx     = 0
  accuracy_metric_idx   = 1

  #mutators config, dont change
  fmt_cutoff     = "%s_%d_lvl%d_cutoff"
  fmt_rule       = "%s_%d_lvl%d_rule"
  fmt_bin        = "%s__%d"
  fmt_array      = "%s_i%d_%s"
  fmt_bin2D      = "%s_i%d_%s__%d"
  first_lvl      = 1
  cutoff_max_val = 2**30

class config(config_defaults):
  pass


#################################################################
#################################################################

def applypatch(patch):
  '''copy a given set of config values from patch to config'''
  def copycfg(src, dst):
    for n in dir(src):
      if (n[0:2],n[-2:]) != ("__","__"):
        if hasattr(dst, n):
          setattr(dst, n, getattr(src,n))
        else:
          print "MISSING CONFIG", n
          assert False
  copycfg(patch, config)

def dump(f):
  names = filter(lambda x: x[0:2]!='__', dir(config))
  values = map(lambda x: getattr(config, x), names)
  print >>f, 'class config:'
  for name, value in zip(names, values):
    print >>f, "  %s = %s" % (name, repr(value))

def option_callback(option, opt, value, parser):
  opt=str(option).split('/')[0]
  while opt[0]=='-':
    opt=opt[1:]
  assert hasattr(config, opt)
  setattr(config, opt, value)

#################################################################
#################################################################

class patch_check:
  '''settings for automated regression checks'''
  #required flags
  use_iogen                = True
  check                    = True

  #run for 30 sec or 2**13 input size
  max_input_size           = 256
  max_time                 = 60
  rounds_per_input_size    = 1

  #bigger pop size
  population_low_size      = 3

  # wait longer for results, higher time limits
  limit_multiplier         = 15

  #run two trials per alg
  confidence_pct   = 0.0
  max_trials       = 2
  min_trials       = 2

class patch_accuracy_target:
  def __init__(self, v):
    self.accuracy_target = v

class patch_noninteractive:
  '''settings for disabling outputs'''
  cleanup_inputs           = True
  debug                    = False
  delete_output_dir        = True
  output_cfg               = None
  print_log                = False
  pause_on_crash           = False
  candidatelog             = False
  mutatorlog               = False
  output_dir               = "/tmp"

class patch_regression(patch_noninteractive, patch_check):
  pass


class patch_pbbenchmark(patch_noninteractive):
  pass

class patch_reset:
  accuracy_target = None
  timing_target = None
  output_cfg = None
  n = None
  seed = None
  main = None
  max_gen = None

class patch_debug:
  '''settings for debugging'''
  cleanup_inputs           = False
  debug                    = True
  print_log                = True
  pause_on_crash           = True
  candidatelog             = True

class patch_onlinelearning:
  use_iogen  = False
  min_trials = 5
  max_trials = 30

class patch_n:
  def __init__(self, n):
    from math import log
    self.max_input_size = config.offset+2**int(round(log(n, 2)))
    self.max_time = 2**30
    self.n = n

class patch_n_offset:
  def __init__(self, n):
    from math import log
    self.offset = n - 2**int(round(log(n, 2)))
    self.max_input_size = n
    self.max_time = 2**30
    self.n = n

