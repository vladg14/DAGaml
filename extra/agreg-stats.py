import sys
from operator import add
from functools import reduce

FILE = open(sys.argv[1])
M = [line.split(',') for line in FILE.read().split('\n') if line!='']
M1 = [[line[0]]+list(map(int, line[1:])) for line in M if len(line)==15]
M2 = [[X[4]/X[2], X[6]/X[2], X[8]/X[2], X[12]/X[2]] for X in M1]
def M_to_V(MAT):
    def post(x):
        x = round(100*(x/len(MAT)-1))
        if x > 0:   return '+'+str(x)+'%'
        elif x < 0: return str(x)+'%'
        else:       return '0'
    return list(map(post, reduce(lambda X, Y: map(add, X, Y), MAT)))
V2 = M_to_V(M2)
M3 = [[Y[1]+X[5]/176/X[2], Y[2]+X[7]/176/X[2], Y[3]+X[11]/176/X[2]] for X, Y in zip(M1, M2)]
V3 = M_to_V(M3)
print(',\t#node,\tmemo')
print(   'Z,\t{0},\t{1}'.format(V2[0], V2[0]))
print(  'NU,\t{0},\t{1}'.format(V2[1], V3[0]))
print( 'NNI,\t{0},\t{1}'.format(V2[2], V3[1]))
print('NU-X,\t{0},\t{1}'.format(V2[3], V3[2]))
