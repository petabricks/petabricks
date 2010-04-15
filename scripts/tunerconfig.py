
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

class config_regression_check_fast:
  '''settings for automated regression checks'''
  limit_conf_pct           = 0.65
  use_iogen                = True
  cleanup_inputs           = True
  check                    = True
  debug                    = False
  compare_confidence_pct   = 0.0
  compare_max_trials       = 1
  compare_min_trials       = 1
  offspring_confidence_pct = 0.0
  offspring_max_trials     = 1
  offspring_min_trials     = 1
  mutations_per_mutator    = .85
  population_low_size      = 3
  max_rounds               = 13
  max_time                 = 30
  delete_output_dir        = True
  print_log                = False

class config(config_defaults):
  pass

def copycfg(src, dst):
  for n in dir(src):
    if (n[0:2],n[-2:]) != ("__","__"):
      assert hasattr(dst, n)
      setattr(dst, n, getattr(src,n))



