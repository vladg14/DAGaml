import sys
#number of nodes
n = int(sys.argv[1])
#input file name
fin   = sys.argv[2]

FILE = open(fin)
L = FILE.read()
L1 = (L.split('\n')[1]).split(' ')
L2 = [False if '-' in w else True for w in L1[:(n*(n+1))]]
print(L2)
M = [L2[i*n:(i+1)*n] for i in range(n+1)]
S = M[:n]
B = M[n]
print('\n'.join(map(lambda line : ''.join(map(lambda x : 'X' if x else '.', line)), S)))
print('0b'+''.join(map(lambda x : str(int(x)), B)))


