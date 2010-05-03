
class config_defaults:
  #candidatetester config:
  metrics               = ['timing', 'accuracy']
  timing_metric_idx     = 0
  accuracy_metric_idx   = 1
  offset                = 0
  '''confidence intervals when displaying numbers'''
  display_confidence    = 0.90
  '''guessed stddev when only 1 test is taken'''
  prior_stddev_pct      = 0.20
  '''percentage change to be viewed as insignificant when testing if two algs are equal'''
  same_threshold_pct    = 0.01
  '''confidence for generating execution time limits'''
  limit_conf_pct        = 0.95
  '''multiply generated time limits by a factor'''
  limit_multiplier      = 2.0
  '''print raw timing values instead of mean and confidence interval'''
  print_raw             = False
  '''store random inputs and share between runs'''
  use_iogen             = True
  '''delete iogen inputs at the end of each round'''
  cleanup_inputs        = True
  '''check output hash against peers, requires use_iogen'''
  check                 = False

  #sgatuner config
  debug                    = True
  mutate_retries           = 10
  compare_confidence_pct   = 0.95
  compare_max_trials       = 15
  compare_min_trials       = 3
  offspring_confidence_pct = 0.95
  offspring_max_trials     = 10
  offspring_min_trials     = 3
  mutations_per_mutator    = .85
  population_high_size     = 20
  population_low_size      = 1
  multimutation            = True
  lognorm_tunable_types    = ['system.cutoff.splitsize', 'system.cutoff.sequential']
  uniform_tunable_types    = ['system.flag.unrollschedule']
  autodetect_tunable_types = ['user.tunable']
  ignore_tunable_types     = ['algchoice.cutoff','algchoice.alg']
  max_rounds               = 64
  max_time                 = 60*10
  output_dir               = "/tmp"
  delete_output_dir        = True
  print_log                = True


  #mutators config:
  fmt_cutoff     = "%s_%d_lvl%d_cutoff"
  fmt_rule       = "%s_%d_lvl%d_rule"
  first_lvl      = 1
  cutoff_max_val = 2**30
  rand_retries   = 10

class patch_check:
  '''settings for automated regression checks'''
  #required flags
  use_iogen                = True
  check                    = True
  
  #run for 30 sec or 2**13 input size
  max_rounds               = 13
  max_time                 = 30

  # pop size of 3... with extra mutation
  mutations_per_mutator    = 1.0
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

class patch_regression(patch_noninteractive, patch_check):
  pass

class config(config_defaults):
  pass

def copycfg(src, dst):
  for n in dir(src):
    if (n[0:2],n[-2:]) != ("__","__"):
      assert hasattr(dst, n)
      setattr(dst, n, getattr(src,n))

def applypatch(patch):
  copycfg(patch, config)



