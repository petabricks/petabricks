#!/usr/bin/env python

"""Print a human-readable report of the auto-tuner's decisions.

Usage: tuning_report.py <program>

"""

import sys
from xml.dom import minidom

import configtool
import pbutil


def get_choice2id2label(transform):
  rules_elts = transform.getElementsByTagName("rules")
  choice2id2label = {}
  for rules_elt in rules_elts:
    choicename = rules_elt.getAttribute("choicename")
    rule_elts = rules_elt.getElementsByTagName("rule")
    id2label = dict((int(r.getAttribute("index")), r.getAttribute("label"))
                    for r in rule_elts)
    choice2id2label[choicename] = id2label
  return choice2id2label


def print_tunables(benchmark, choice2id2label):
  cfg_file = pbutil.benchmarkToCfg(benchmark)
  def fix_rule_numbers(key, val):
    """If the tunable is an algorithm choice, convert the index to the label."""
    if key in choice2id2label:
      return (key, choice2id2label[key][int(val)])
    else:
      return (key, val)
  configtool.processConfigFile(cfg_file, "/dev/stdout", [fix_rule_numbers])


def print_report(benchmark, infoxml):
  transforms = infoxml.getElementsByTagName("transform")
  choice2id2label = {}
  for transform in transforms:
    choice2id2label.update(get_choice2id2label(transform))
  print_tunables(benchmark, choice2id2label)


def main(argv):
  if len(argv) != 2:
    print "No program specified."
    print
    print __doc__
    sys.exit(1)
  app = argv[1]
  app = pbutil.normalizeBenchmarkName(app)

  info_file = pbutil.benchmarkToInfo(app)
  try:
    infoxml = minidom.parse(info_file)
  except:
    print "Parse error while parsing .info XML file:", info_file
    sys.exit(1)

  print_report(app, infoxml)


if __name__ == '__main__':
  main(sys.argv)
