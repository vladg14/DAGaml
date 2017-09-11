import sys
def onefile(NAME):
	FILE = open(NAME, 'r')
	TEXT = FILE.read().split('\n')
	FILE.close()
	L = [3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]
	def select(l):
		U = [x for x in TEXT[l-1].split(' ') if x!='']
		U = [x for x in U[-1].split('\t') if x!='']
		return U[-1].replace(',', '.')
	try:
		print(",\t".join([TEXT[0]]+list(map(select, L))))
	except:
		print(TEXT[0])

print('\n')
for name in sys.argv[1:]:
	onefile(name)
print('\n')
