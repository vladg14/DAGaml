import sys

args = sys.argv[1:]

strip = ''

if args[0] == '--strip':
	strip = args[1]
	args = args[2:]

sep = '\n'

if args[0] == '--sep':
	sep = args[1]
	args = args[2:]

ns = args[:-1]
L = args[-1].split('\n')


def splits(L, seq):
	for s in seq:
		L = sum((string.split(s) for string in L if string!=''), [])
	return [string for string in L if string!='']

if strip!='':
	L = splits(L, strip)

def myget(L, n):
	if ':' in n:
		ln = n.split(':')
		assert(len(ln)>1)
		if len(ln) == 2:
			if ln[0] == '' and ln[1]=='' : return L
			elif ln[0] == '' : return L[:int(ln[1])]
			elif ln[1] == '' : return L[int(ln[0]):]
			else : return L[int(ln[0]):int(ln[1])]
		elif len(ln) == 3:
			print('not implemented yet')
			assert(False)
		else:
			assert(False)
	else:
		return [L[int(n)]]

print(sep.join(sum((myget(L, n) for n in ns), [])))
