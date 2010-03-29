
class config:
  #candidatetester config:
  fmt_cutoff = "%s_%d_lvl%d_cutoff"
  fmt_rule   = "%s_%d_lvl%d_rule"
  metrics               = ['timing', 'accuracy']
  timing_metric_idx     = 0
  offset                = 0
  tmpdir                = "/tmp"
  '''confidence intervals when displaying numbers'''
  display_confidence    = 0.95
  '''confidence intervals when comparing results'''
  compare_confidence    = 0.95
  '''guessed stddev when only 1 test is taken'''
  prior_stddev_pct      = 0.15
  '''percentage change to be viewed as insignificant when testing if two algs are equal'''
  same_threshold_pct    = 0.01
    
  limit_conf_pct   = 0.95
  limit_multiplier = 1.35

  max_result = 2.0 ** 30
  print_raw=False

  use_iogen = True
  cleanup_inputs = True
  check = True

  #sgatuner config
  debug = True
  mutate_retries = 10
  compare_confidence_pct = 0.95
  compare_max_trials = 25
  compare_min_trials = 3
  offspring_confidence_pct = 0.95
  offspring_max_trials = 15
  offspring_min_trials = 3
  mutations_per_mutator = 0.6
  population_high_size = 20
  population_low_size  = 1
  rounds = 23
  multimutation = True
  lognorm_tunable_types = ['system.cutoff.splitsize', 'system.cutoff.sequential']
  uniform_tunable_types = []
  ignore_tunable_types  = ['algchoice.cutoff','algchoice.alg']
  rounds_per_level = 1
  
  
  #mutators config:
  fmt_cutoff = "%s_%d_lvl%d_cutoff"
  fmt_rule   = "%s_%d_lvl%d_rule"
  first_lvl = 1
  cutoff_max_val = 2**30
  rand_retries = 10

