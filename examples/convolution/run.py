#!/usr/bin/env python

import subprocess
from xml.dom.minidom import parseString

stddevThreshold = 0.1 # tolerate up to 10% variation
nTrials = 5
nThreads = 2

def run(cmd):
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    ret = proc.poll()
    if ret:
        print '%s: returned failure %d' % (cmd[0], ret)
        print '-----------------------------'
        print '%s' % err
        print '%s' % out
        error = subprocess.CalledProcessError(ret, cmd)
        error.output = out
        raise error
    return out

def compile(radius):
    cmd = [
        "../../src/pbc",
        "--preproc=/usr/bin/cpp -DKERNEL_RADIUS=%d" % radius,
        "--output=convolutionSeparable.r%d" % radius,
        "convolutionSeparable.pbcc"
    ]
    run(cmd)

def test(radius, size, mode):
    # TODO: copy config to dest location?
    cmd = [
        "./convolutionSeparable.r%d" % radius,
        "--time",
        "--trials=%d" % nTrials,
        "--isolation",
        "--threads=%d" % nThreads,
        "-n", "%d" % size,
        "--config=./convolutionSeparable2.%s.cfg" % mode
    ]
    res = run(cmd)
    x = parseString(res)
    timing = x.getElementsByTagName('timing')[0]
    stddev = float(timing.getAttribute('stddev'))
    t = float(timing.getAttribute('median'))
    # For now, just warn if it seems unreasonable
    if stddev > t * stddevThreshold:
        print 'WARNING: stddev for %s with n=%d, was high: %f' % (mode, size, stddev)

    return t,stddev

def test_radius(radius, sizes=[3520]):
    print 'Testing radius=%d' % radius
    compile(radius)
    res = []
    for size in sizes:
        # TODO: add localmem option
        for sep in ['2d', 'sep']:
            for local in ['local', 'nolocal']:
                mode = '%s.%s' % (sep,local)
                t,stddev = test(radius, size, mode)
                res.append( (radius, size, mode, t, stddev) )
                #print '%d^2, %s takes %f (stddev: %f)' % (size, mode, t, stddev)
    return res

# Run all tests
res = []
for radius in [1,2,3,4,5,6,7,8,9,10]:
    t = test_radius(radius)
    res.extend( t )

for r,s,m,t,dev in res:
    print 'R=%d, %dx%d, %s takes:\t%f (stddev: %f)' % (r, s, s, m, t, dev)
