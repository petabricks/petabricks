#!/usr/bin/env python

# usage: pbsrun.py $PBS_NODEFILE ./examples/multigrid/Poisson2DSOR -n 16384 --config=sor-dev.cfg

import os
import socket
import sys
import time

os.chdir(os.environ["PBS_O_WORKDIR"])
nodeId = int(os.environ["PBS_VNODENUM"])
nodefilename = sys.argv[1] # In some case, os.environ["PBS_NODEFILE"] is not set.

port = 22550 # magic number --> config.h:LISTEN_PORT_FIRST

nodefile = open(nodefilename, "r")
parent = nodefile.readline().strip()
nodefile.close()

cmd = " ".join(sys.argv[2:])
cmd += (" --hosts=" + nodefilename)
cmd += " --time --pbs"

if nodeId == 0:
    # parent
    cmd += " --noisolation"

else:
    # slave
    cmd += " --slave-host " + parent + " --slave-port " + str(port)

# print socket.gethostname() + " : " + cmd
os.system(cmd);
