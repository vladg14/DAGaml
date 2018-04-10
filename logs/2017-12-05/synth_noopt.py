import sys
FILE = open(sys.argv[1])
TXT = [line.split('\t') for line in FILE.read().split('\n') if line!='']
GEN_TIME = [float(l[1]) for l in TXT]
OPT_TIME = [float(l[3]) for l in TXT]
NSAT = [int(l[4]) for l in TXT]
NSOL = [int(l[5]) for l in TXT]
NREJ = [x-y for x, y in zip(NSAT, NSOL)]
OPT_TIME_10 = [int(x/10) for x in OPT_TIME]
def hist(seq):
	MAX = max(seq)
	HIST = [0 for _ in range(MAX+1)]
	for x in seq: HIST[x]+=1
	return HIST
HIST_TIME_10 = hist(OPT_TIME_10)
print("hist:opt time [10]")
print('\t'.join(map(str, HIST_TIME_10)))
HIST_NSAT = hist(NSAT)
HIST_NSOL = hist(NSOL)
HIST_NREJ = hist(NREJ)
print("hist:#{call to minisat}")
print('\t'.join(map(str, HIST_NSAT)))
print("hist:#{new solution}")
print('\t'.join(map(str, HIST_NSOL)))
print('hist:#{rejection}')
print('\t'.join(map(str, HIST_NREJ)))
def stats(seq):
	MEAN = sum(seq)/len(seq)
	ETYP = (sum([(x-MEAN)**2 for x in seq])/len(seq))**0.5
	MIN = min(seq)
	MAX = max(seq)
	SORT = list(sorted(seq))
	DQ1 = int(len(seq)/4)
	QUART1 = SORT[DQ1]
	MEDIAN = SORT[int(len(seq)/2)]
	QUART3 = SORT[-DQ1]
	return 'mean:         ' + str(MEAN) +'\n'+\
	       'stdd:         ' + str(ETYP)+'\n'+\
		   'min:          ' + str(MIN)+'\n'+\
	       'max:          ' + str(MAX)+'\n'+\
	       'mediane:      ' + str(MEDIAN)+'\n'+\
	       '1st quartile: ' + str(QUART1)+'\n'+\
	       '3rd quartile: ' + str(QUART3)
print('stats:gen')
print(stats(GEN_TIME))
print('stats:opt time')
print(stats(OPT_TIME))
print('stats:#{call to minisat}')
print(stats(NSAT))
print('stats:#{new solution}')
print(stats(NSOL))
print('stats:#{rejection}')
print(stats(NREJ))
TOTAL = [sum(line) for line in zip(GEN_TIME, OPT_TIME)]
print('stats:total')
print(stats(TOTAL))
