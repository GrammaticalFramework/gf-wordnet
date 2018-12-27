from scipy import sparse
from sklearn.decomposition import NMF
import numpy
import math

dims = {}
funs = []
rows = []
cols = []
vals = []

def add(f):
	i = dims.get(f)
	if i == None:
		i = len(dims)
		dims[f] = i
		funs.append(f)
	return i

stopList = [
   "_Prep", "_Conj", "_DConj", "_Subj", "RP", 
   "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9",
   "D_0", "D_1", "D_2", "D_3", "D_4", "D_5", "D_6", "D_7", "D_8", "D_9",
   "pot01", "pot41", "pot31", "_Pron",
   "_IAdv", "_CAdv", "FullStop", "QuestMark", "ExclMark", 
   "DefArt", "IndefArt", "_Quant", "_Det", "_IDet", "_Predet",
   "UseCopula", "_VP", "_Card", "_ACard"
   ]

def stopword(fun):
	for s in stopList:
		if fun.endswith(s):
			return True
	return False

count = 0
f = open("Parse.bigram.probs")
for line in f:
	[h,m,v] = line.split()	
	if stopword(h) or stopword(m):
		continue

	rows.append(add(h))
	cols.append(add(m))
	vals.append(float(v))
	count = count + 1
f.close()

matrix = sparse.csr_matrix((vals,(rows,cols)),shape=(len(dims),len(dims)))

del rows
del cols
del vals
del add

print(matrix.shape)

nmf = NMF(n_components=100)
h = nmf.fit_transform(matrix)
m = numpy.transpose(nmf.components_)
print(h.shape)
print(m.shape)

del matrix

hs = h.sum(axis=0)
ms = m.sum(axis=0)

h = h / hs
m = m / ms

c = numpy.multiply(hs,ms);
c = c / sum(c)

f = open("embedding.txt", "w+") 
f.write(" ".join(map (lambda p: '%.18E' % p, c))+"\n")
f.write("\n")

max_neighbours = 100

for fun1 in funs:
	if not (fun1 in dims):
		continue

	h1 = h[dims[fun1]]
	m1 = m[dims[fun1]]
	v1 = (h1 + m1)/2

	f.write(fun1+"\n")
	f.write(" ".join(map (lambda p: '%.18E' % p, h1))+"\n")
	f.write(" ".join(map (lambda p: '%.18E' % p, m1))+"\n")
	f.write("\n")

f.close()
