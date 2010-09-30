import sys, random

n = int(sys.argv[1])

data = range(n)
random.shuffle(data)
data = map(str,data)

print "SIZE", str(n)
print "  ".join(data)
