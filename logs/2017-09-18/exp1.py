def sec_of_unix(s):
     l = s.split('m')
     m = int(l[0])
     l = l[1].split('s')
     s = float(l[0])
     return 60*m+s
import sys

def round(x):
	if x%1 >= 0.5 : return int(x)+1
	else:			return int(x)

def intlow(x): return int(x//1)

import math

def sign(x, l):
	if x == 0 : return ' 0'+' '*(l-1)
	else:
		sn, x = ('+', x) if x > 0 else ('-', -x)
		fx = str(x)
		return sn + fx + ' '*(l-len(fx))

def sci(x):
	def aux0(x):
		x = round(x*1000)
		sf = str(x%1000)
		si = str(x//1000)
		sf = sf+' '*(3-len(sf))
		si = ' '*(3-len(si))+si
		return si+'.'+sf
	def aux1(x):
		n = 3*int((math.log(x, 10)// 3))
		return aux0(x*(10**(-n)))+'E'+sign(n, 3)
	if x > 0:   return '+'+aux1( x)
	elif x < 0: return '-'+aux1(-x)
	else:		return '   0.   '

def summarize(M):
	A = [sum(line[i] for line in M)/len(M) for i in range(len(M[0]))]
	S = [math.sqrt(sum((line[i]-A[i])**2 for line in M)/len(M)) for i in range(len(M[0]))]
	return A, S

def matrixify(L, N):
	return [L[(N*i):(N*(i+1))] for i in range(len(L)//N)]

def print_sci_matrix(M):
	print('\n'.join((' | '.join(map(sci, line)) for line in M)))
	

FILE = open(sys.argv[1])
L = FILE.read().split('\n')
FILE.close()
M0 = [line.split(',\t')[1:] for line in L if line!='']
M1 = [[sec_of_unix(x) if 'm' in x else int(x) for x in line] for line in M0]
for line in M1 :
	L = line[0::5]
	for x in L[1:] : assert(x == L[0])
M1B = [[x for i, x in enumerate(line) if i%5!=0] for line in M1]
print('summarizing the set of benchmark')
M1A, M1S = summarize(M1B)
N = 4
print('average')
print_sci_matrix(matrixify(M1A, N))
print('standard deviation')
print_sci_matrix(matrixify(M1S, N))
def proc0(line):
	M = matrixify(line, N)
	return [min(line[i] for line in M) for i in range(len(M[0]))]
print('minimal runtime per benchmark')
M2 = list(map(proc0, M1B))
M2A, M2S = summarize(M2)
print('average')
print_sci_matrix(matrixify(M2A, N))
print('standard deviation')
print_sci_matrix(matrixify(M2S, N))

