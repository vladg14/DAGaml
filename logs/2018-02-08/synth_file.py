import sys
FILE = open(sys.argv[1])
TXT = [line.split('\t') for line in FILE.read().split('\n') if line!='']
DEF_SIZE = [int(l[0]) for l in TXT]
FOR_SIZE = [int(l[1]) for l in TXT]
WAP_SIZE = [int(l[2]) for l in TXT]
RWA_SIZE = [int(l[3]) for l in TXT]
def hist(seq):
	MAX = max(seq)
	HIST = [0 for _ in range(MAX+1)]
	for x in seq: HIST[x]+=1
	return HIST
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
print('\nstats:default')
print(stats(DEF_SIZE))
print('\nstats:FORCE order')
print(stats(FOR_SIZE))
print('\nstats:WAP order')
print(stats(WAP_SIZE))
print('\nstats:reverse WAP order')
print(stats(RWA_SIZE))
def diff(X, Y) : return [x-y for x, y in zip(X, Y)]
print('\nstats:default vs FORCE')
print(stats(diff(DEF_SIZE, FOR_SIZE)))
print('\nstats:FORCE order vs WAP order')
print(stats(diff(FOR_SIZE, WAP_SIZE)))
print('\nstats:FORCE order vs reverse WAP order')
print(stats(diff(FOR_SIZE, RWA_SIZE)))
print('\nstats:WAP order vs reverse WAP order')
print(stats(diff(WAP_SIZE, RWA_SIZE)))
