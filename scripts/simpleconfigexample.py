
import configtool, traininginfo, pbutil
import tempfile, os


n = 10000
benchmark = "sort/Sort"
cfg = configtool.ConfigFile("./examples/sort/Sort.cfg")
info = traininginfo.TrainingInfo("./examples/sort/Sort.info")

tunables = info.tunablesDict()

for k in sorted(cfg.keys()):
  try:
    print k.ljust(50), str(cfg[k]).ljust(10), tunables[k]['min'], tunables[k]['max'], tunables[k]['type']
  except:
    print '???'


def runCfg(benchmark, cfg, n, args=['--trials=5']):
  fd, tmp = tempfile.mkstemp('.cfg')
  try:
    os.close(fd)
    cfg.save(tmp)
    perf, acc = pbutil.executeTimingRun(pbutil.benchmarkToBin(benchmark),
                                        int(n),
                                        args+['--config='+tmp, '--accuracy'],
                                        None,
                                        ['timing', 'accuracy'])
    return perf['average'], acc['average']
  finally:
    os.unlink(tmp)

perf, acc = runCfg(benchmark, cfg, n)

print 'perf',perf
print 'accuracy',acc


