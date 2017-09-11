import sys
def onefile(NAME):
	FILE = open(NAME, 'r')
	TEXT = FILE.read().split('\n')
	FILE.close()
	L = [15, 13, 22, 20, 30, 28, 6, 9, 65, 67]
	def select(l):
		U = [x for x in TEXT[l-1].split(' ') if x!='']
		U = [x for x in U[-1].split('\t') if x!='']
		return U[-1].replace(',', '.')

	print(",\t".join(map(select, L)))

for name in sys.argv[1:]:
	onefile(name)
