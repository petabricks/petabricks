#!/usr/bin/python

"""Module docstring

Usage: ./autotune.py <options> <program>

Options:
  -p             Number of threads to target 
                   - Must be greater than 1
                   - Default is worker_threads (from config file)
  -n, --random   Size of random data to optimize on
                   - Default is 100000
  --min          Min size to autotune on
                   - Default is 64
  --max          Max size to autotune on
                   - Default is 4096
  -h, --help     This help screen

"""


import re
import sys
import os
import getopt
import subprocess
import pbutil
from pprint import pprint

from xml.dom.minidom import parse

config_tool_path = ""
app = None
cfg = None
parallel_autotune = False


NULL=open("/dev/null", "w")

maxint = 2147483647

ignore_list = []

def getConfigVal(key):
  run_command = [config_tool_path, app + ".cfg", "get", key]
  p = subprocess.Popen(run_command, stdout = subprocess.PIPE)
  os.waitpid(p.pid, 0)
  return p.stdout.read()

def setConfigVal(key, val):
  run_command = [config_tool_path, app + ".cfg", "set", key, str(val)]
  p = subprocess.Popen(run_command)
  os.waitpid(p.pid, 0)

# def getConfigVal(key):
#   print "get", key
#   return pbutil.getConfigVal("%s.cfg"%app, key)
# 
# def setConfigVal(key, val):
#   print "set", key, val
#   return pbutil.setConfigVal("%s.cfg"%app, key, val)


def reset():
  ignore_vals = []
  for ignore in ignore_list:
    ignore_vals.append((ignore, getConfigVal(ignore)))

  run_command = ["./" + app, "--reset"]
  p = subprocess.Popen(run_command)
  os.waitpid(p.pid, 0)

  for ignores in ignore_vals:
    setConfigVal(ignores[0], ignores[1])


def getTunables(xml, type):
  transforms = xml.getElementsByTagName("transform")

  algchoices = []
  for transform in transforms:
    algchoices += transform.getElementsByTagName("tunable")

  tunables = []
  for algchoice in algchoices:
    choice = str(algchoice.getAttribute("name"))
    if choice not in ignore_list:
      if algchoice.getAttribute("type") == type:
        tunables.append(choice)
  return tunables

def getAlgChoices(xml):
  transforms = xml.getElementsByTagName("transform")

  algchoices = []
  for transform in transforms:
    algchoices += transform.getElementsByTagName("algchoice")

  static_choices = []
  dynamic_choices = []
  for algchoice in algchoices:
    choice = str(algchoice.getAttribute("name"))
    if choice not in ignore_list:
      if algchoice.getAttribute("type") == "sequential":
        static_choices.append(choice)
      else:
        dynamic_choices.append(choice)
  return (static_choices, dynamic_choices)

def autotune(choice, trials, min, max):
  print "Autotuning:", choice
  run_command = ["./" + app, "--autotune=%s"%choice, "--min=%d"%min, "--max=%d"%max, "--trials=%d"%trials]
  if parallel_autotune:
    run_command.append("--multigrid")
  #print run_command
  p = subprocess.Popen(run_command, stdout=subprocess.PIPE, stderr=NULL)
  os.waitpid(p.pid, 0)
  lines = p.stdout.readlines()
  print "Result:" + lines[-int(getConfigVal("autotune_alg_slots"))],

def getIgnoreList():
  try:
    f = open(app + ".ignore")
    try:
      for line in f:
        ignore_list.append(line.replace("\n", ""))
    finally:
      f.close()
  except:
    pass

def optimize(tunable, size):
  print "Optimizing:", tunable
  run_command = ["./" + app, "--optimize=%s"%tunable, "--random=%d"%size]
  p = subprocess.Popen(run_command, stdout=subprocess.PIPE, stderr=NULL)
  os.waitpid(p.pid, 0)
  print "Result:", getConfigVal(tunable),


def main(argv):
  if len(argv) == 1:
    print "Error.  For help, run:", argv[0], "-h"
    sys.exit(2)

  global config_tool_path
  global app
  global ignore_list
  global parallel_autotune

  config_tool_path = os.path.split(argv[0])[0] + "/configtool.py"
  app = argv[-1]
  cfg = app+".cfg"
  num_threads = -1 # -1 -> use config file
  data_size = 100000
  min = 64
  max = 4096
  fast = False

  try:
    opts, args = getopt.getopt(argv[1:-1], "hn:p:", 
        ["help","random=","min=","max=","config=","parallel_autotune","fast"])
  except getopt.error, msg:
    print "Error.  For help, run:", argv[0], "-h"
    sys.exit(2)
  # process options
  for o, a in opts:
    if o in ["-h", "--help"]:
      print __doc__
      sys.exit(0)
    if o == "-p":
      num_threads = int(a)
    if o in ["-n", "--random"]:
      data_size = int(a)
    if o in ["-c", "--config"]:
      cfg = a
    if o == "--parallel_autotune":
      parallel_autotune = True
    if o == "--min":
      min = int(a)
    if o == "--max":
      max = int(a)
    if o == "--fast":
      fast = True

  getIgnoreList()

  try:
    infoxml = parse(app + ".info")
  except:
    print "Cannot parse:", app + ".info"
    sys.exit(-1)

  print "Reseting config entries"
  reset()

  # Set worker num
  if num_threads == -1:
    num_threads = int(getConfigVal("worker_threads")) 
  else:
    setConfigVal("worker_threads", num_threads)

  print "Tuning", app, "with", num_threads, "threads...", "(fast =", str(fast) + ")" 

  (static_choices, dynamic_choices) = getAlgChoices(infoxml)
  seq_cutoff_tunables = getTunables(infoxml, "system.seqcutoff")
  splitsize_tunables = getTunables(infoxml, "system.splitsize")
  user_tunables = getTunables(infoxml, "user.tunable")

  # Autotune sequential code
  for tunable in seq_cutoff_tunables:
    setConfigVal(tunable, maxint)
  for choice in static_choices:
    autotune(choice, 1, min, max / 8)

  # Autotune parallel code
  for tunable in seq_cutoff_tunables:
    setConfigVal(tunable, 50)
  for choice in dynamic_choices:
    autotune(choice, 1, min, max)

  # Optimize sequential cutoffs
  for tunable in seq_cutoff_tunables:
    optimize(tunable, data_size)

  # Optimize split sizes
  for tunable in splitsize_tunables:
    optimize(tunable, data_size)

  # Optimize user tunables
  for tunable in user_tunables:
    optimize(tunable, data_size)

  # Autotune parallel code
  for choice in dynamic_choices:
    autotune(choice, 1, min, max)

  if fast:
    sys.exit(0)

  # Optimize sequential cutoffs
  for tunable in seq_cutoff_tunables:
    optimize(tunable, data_size)

  # Optimize split sizes
  for tunable in splitsize_tunables:
    optimize(tunable, data_size)

  # Optimize user tunables
  for tunable in user_tunables:
    optimize(tunable, data_size)

  # Autotune parallel code
  for choice in dynamic_choices:
    autotune(choice, 1, min, max)

if __name__ == "__main__":
    main(sys.argv)

