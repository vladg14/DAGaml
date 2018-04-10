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
		n = 3*(math.log(10, x) // 3)
		return aux0(x*(10**(-n)))+'E'+sign(n, 3)
	if x > 0:   return '+'+aux1( x)
	elif x < 0: return '-'+aux1(-x)
	else:		return '   0.   '
	

FILE = open(sys.argv[1])
L = FILE.read().split('\n')
FILE.close()
M = [line.split(',\t') for line in L if line!='']
M1 = [[line[0], line[1]==line[2]]+[sec_of_unix(x) for x in line[3:9]]+list(map(int, line[9:11])) for line in M]
for line in M1 : assert(line[1])
M4 = [sum(line[i] for line in M1)/len(M1) for i in range(2, 10)]
print(M4)
M2 = [[line[0]]+[y-x for x, y in zip(line[2:5], line[5:8])]+[line[9]-line[8]] for line in M1]
M3 = [sum(line[i] for line in M2)/len(M2) for i in range(1, len(M2[0]))]
print(M3)
M5 = [100*(x/y) for x, y in zip(M3, M4[0:3]+[M4[6]])]
print(M5)
def print_sci_list(L) : print('['+','.join(map(sci, L))+']')
print_sci_list(M4)
print_sci_list(M3)
print('ratios')
print_sci_list(M5)


