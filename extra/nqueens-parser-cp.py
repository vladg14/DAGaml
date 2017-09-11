import sys
def onefile(NAME):
	FILE = open(NAME, 'r')
	TEXT = FILE.read().split('\n')
	FILE.close()
	L = [13, 11, 21, 19, 4, 7, 49, 51]
	def select(l):
		U = [x for x in TEXT[l-1].split(' ') if x!='']
		U = [x for x in U[-1].split('\t') if x!='']
		return U[-1].replace(',', '.')

	print(",\t".join(map(select, L)))

for name in sys.argv[1:]:
	onefile(name)
