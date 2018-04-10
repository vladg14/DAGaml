import sys
def main(size, target):
	FILE = open(target)
	TXT = FILE.read().split('\n')
	FILE.close()
	SAT = TXT[0]
	SOL = TXT[1]
	COST = TXT[2]
	matrix = [[int(SOL[i+size*j]) for j in range(size)] for i in range(size)]
	print('\n'.join(map(lambda l : ''.join(map(lambda x : 'X' if x else '.', l)), matrix)))
	print(int(''.join(reversed(COST)), 2))

main(int(sys.argv[1]), sys.argv[2])

