import subprocess
import sys
import time

TIME = time.time()

inf = int(sys.argv[1])
sup = int(sys.argv[2])
assert(inf<=sup)

def parse(target):
	FILE = open(target)
	T = FILE.read().split('\n')
	if "UN" in T[0]:
		return tuple()
	else:
		T0 = [False if '-' in w else True for w in T[1].split(' ')]
		T0 = ''.join(map(str, map(int, T0)))
		TS = T0[:inf-1]
		TB = T0[inf-1:sup]
		return (TS, TB, int(''.join(reversed(TB)), 2))

def extend(MIN, SOL):
	"""MIN = [xn, x(n-1), ..., x(n-k)]2
		 SOL = [yn, y(n-1), ..., y0]2
		 and \\forall 0<=i<=k, x(n-i) = y(n-i)"""
	assert(len(MIN)<=len(SOL))
	assert(MIN == SOL[0:len(MIN)])
	j = 0
	for c in SOL[len(MIN):]:
		if c == '0' : j+=1
		else : break
	return (j, MIN+('0'*j))

cnf = sys.argv[3]
old = cnf + ".old.cnf"
new = cnf + ".new.cnf"
log = cnf + ".sat-log"
out = cnf + ".out"
dic = {"cnf":cnf, "old":old, "new":new, "log":log, "out":out}
import pprint
pprint.pprint(dic)


subprocess.call("cp {cnf} {old}".format(**dic), shell = True)
subprocess.call("time minisat {old} {log} &> /dev/null".format(**dic), shell = True)
RES = parse(log)
if len(RES) == 0 :
	FILE = open(out, 'w')
	FILE.write("UNSAT")
	FILE.close()
else:
	SOL = RES[0]
	MIN = RES[1]
	RMIN = ''.join(reversed(MIN))
	j, Min = extend("", RMIN)
	print('-'+RMIN)
	for i in range(0, j):
		subprocess.call("echo \"-{0} 0\" >> {old}".format(sup-i, **dic), shell=True)
	idx = sup - j
	while idx >= inf :
		print("time = "+str(time.time()-TIME))
		print("Min[{0}] = {1}".format(idx, Min))
		subprocess.call("cp {old} {new}; echo \"-{0} 0\" >> {new}; minisat {new} {log} &> /dev/null".format(idx, **dic), shell = True)
		RES = parse(log)
		if len(RES) == 0:
			j, Min = extend(Min+'1', RMIN)
			subprocess.call("echo \"{0} 0\" >> {old}".format(idx, **dic), shell = True)
		else:
			SOL = RES[0]
			MIN = RES[1]
			RMIN = ''.join(reversed(MIN))
			j, Min = extend(Min+'0', RMIN)
			print('-'+RMIN)
			subprocess.call("mv {new} {old}".format(**dic), shell = True)
		for i in range(1, j+1):
			subprocess.call("echo \"-{0} 0\" >> {old}".format(idx-i, **dic), shell=True)
		idx -= j+1
	FILE = open(out, 'w')
	FILE.write("SAT\n"+SOL+'\n'+MIN+'\n')
	FILE.close()
	
