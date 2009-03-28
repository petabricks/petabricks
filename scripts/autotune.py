#!/usr/bin/python
import re
import sys
import os

from xml.dom.minidom import parse

def main(argv):
  if len(argv) == 1:
    print "usage:", argv[0], "<program>"
    sys.exit(-1)

  app = argv[-1]

  print app

  infoxml = parse(app + ".info")

  transforms = infoxml.getElementsByTagName("transform")

  algchoices = []
  for transform in transforms:
    algchoices = algchoices + transform.getElementsByTagName("algchoice")

  for algchoice in algchoices:
    choice = algchoice.getAttribute("name")[:-1]
    print "Tuning:", choice
    os.spawnvp(os.P_WAIT,"./" + app ,["","--autotune", choice,"--trials","1"])



if __name__ == "__main__":
    main(sys.argv)
