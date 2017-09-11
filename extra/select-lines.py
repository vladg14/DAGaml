import sys
print('\n')
L = eval(sys.argv[1])
print(L)
def onefile(NAME):
	FILE = open(NAME, 'r')
	TEXT = FILE.read().split('\n')
	FILE.close()
	def select(l):
		if l == -1 : return NAME
		U = TEXT[l-1].replace('[', ' ').replace(']', ' ').replace(';', ' ').replace('\t', ' ')
		U = [x for x in U.split(' ') if x!='']
		return U[-1].replace(',', '.')
	try:
		print(",\t".join(list(map(select, L))))
	except:
		print(NAME)

for name in sys.argv[2:]:
	onefile(name)
print('\n')
