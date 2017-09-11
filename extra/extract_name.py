import sys
name = sys.argv[1]
path = name.split('/')
folder = '/'.join(path[1:-1])
name = path[-1].replace('topure-', '').split('.')[0]
print('workdir/'+folder+'/'+name)

