
class config_defaults:
  #how long to train for
  max_rounds               = 64
  max_time                 = 60*10

  #number of trials to run
  compare_confidence_pct   = 0.95
  offspring_confidence_pct = 0.95
  compare_min_trials       = 3
  offspring_min_trials     = 3
  compare_max_trials       = 15
  offspring_max_trials     = 10
  '''guessed stddev when only 1 test is taken'''
  prior_stddev_pct      = 0.20
  '''percentage change to be viewed as insignificant when testing if two algs are equal'''
  same_threshold_pct    = 0.01
  '''confidence for generating execution time limits'''
  limit_conf_pct        = 0.95
  '''multiply generated time limits by a factor'''
  limit_multiplier      = 4.0
  '''offset added to input sizes'''
  offset                = 0

  #how mutation to do
  mutations_per_mutator    = 2
  multimutation            = True
  mutate_retries           = 10
  population_high_size     = 20
  population_low_size      = 1

  #storage and reporting
  debug                    = True
  output_dir               = "/tmp"
  min_input_size_nocrash   = 32
  delete_output_dir        = True
  print_log                = True
  pause_on_crash           = False
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

  #types of mutatators to generate
  lognorm_tunable_types       = ['system.cutoff.splitsize', 'system.cutoff.sequential']
  uniform_tunable_types       = ['system.flag.unrollschedule']
  autodetect_tunable_types    = ['user.tunable']
  lognorm_array_tunable_types = ['user.tunable.accuracy.array']
  ignore_tunable_types        = ['algchoice.cutoff', 'algchoice.alg']
  
  #metric information, dont change for now:
  metrics               = ['timing', 'accuracy']
  metric_orders         = [1, -1] #1 = minimize, -1 = maximize
  timing_metric_idx     = 0
  accuracy_metric_idx   = 1

  #mutators config:
  fmt_cutoff     = "%s_%d_lvl%d_cutoff"
  fmt_rule       = "%s_%d_lvl%d_rule"
  fmt_bin        = "%s__%d"
  first_lvl      = 1
  cutoff_max_val = 2**30
  rand_retries   = 10

class config(config_defaults):
  pass


#################################################################
#################################################################

def copycfg(src, dst):
  for n in dir(src):
    if (n[0:2],n[-2:]) != ("__","__"):
      assert hasattr(dst, n)
      setattr(dst, n, getattr(src,n))

def applypatch(patch):
  copycfg(patch, config)

#################################################################
#################################################################

class patch_check:
  '''settings for automated regression checks'''
  #required flags
  use_iogen                = True
  check                    = True
  
  #run for 30 sec or 2**13 input size
  max_rounds               = 13
  max_time                 = 30

  # pop size of 3... with extra mutation
  mutations_per_mutator    = 2.5
  population_low_size      = 3

  # wait longer for results, higher time limits
  limit_conf_pct           = 0.65
  limit_multiplier         = 8 
  
  #run two trials per alg
  compare_confidence_pct   = 0.0
  compare_max_trials       = 2
  compare_min_trials       = 2
  offspring_confidence_pct = 0.0
  offspring_max_trials     = 2
  offspring_min_trials     = 2

class patch_noninteractive:
  '''settings for disabling outputs'''
  cleanup_inputs           = True
  debug                    = False
  delete_output_dir        = True
  print_log                = False
  pause_on_crash           = False

class patch_regression(patch_noninteractive, patch_check):
  pass


