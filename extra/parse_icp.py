import sys
def main(target):
    NAME = target.split('/')[-1].split('-')[1].split('.')[0]
    FILE = open(target)
    TXT = FILE.read().split('\n')
    FILE.close()
    def parsetime(time):
        MIN = int(time.split('m')[0])
        SEC = float(time.split('m')[1].split('s')[0])
        return MIN*60+SEC
    TIME = [parsetime(line.split('\t')[1]) for line in TXT if 'user' in line]
    def parseand(line):
        L = list(filter(lambda x : x!='', ''.join(map(lambda x : x if x.isdigit() else ' ', line)).split(' ')))
        return L[-2]
    NGATE = [parseand(line) for line in TXT if 'and' in line]
    NSAT = len([line for line in TXT if 'Min' in line]) + 1 
    NSOL = len([line for line in TXT if len(line) > 0 and line[0]=='-'])
    print('\t'.join(map(str, [NAME]+TIME+[NSAT]+[NSOL]+NGATE)))

for target in sys.argv[1:]: main(target)

