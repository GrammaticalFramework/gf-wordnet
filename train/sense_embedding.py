import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk

from scipy import sparse
from sklearn import manifold
from sklearn.decomposition import NMF
import numpy
import sys
import math

training = len(sys.argv) > 1 and sys.argv[1] == "train"

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

count = 0
f = open("../Parse.bigram.probs")
for line in f:
	[h,m,v] = line.split()
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

sys.setrecursionlimit(10000)

class Viewer:
	def __init__(self, h, c, m, dims, funs):
		self.h      = h
		self.c      = c
		self.m      = m
		self.dims   = dims
		self.funs   = funs

		self.window = Gtk.Window()
		self.window.connect("destroy", self.destroy)
		self.window.resize(1000,700)

		entry = Gtk.Entry()
		btn1  = Gtk.RadioButton.new_from_widget(None)
		btn1.set_label("20")
		btn2  = Gtk.RadioButton.new_from_widget(btn1)
		btn2.set_label("50")
		btn3  = Gtk.RadioButton.new_from_widget(btn2)
		btn3.set_label("100")
		darea = Gtk.DrawingArea()

		btn1.size =  20
		btn2.size =  50
		btn3.size = 100

		btn3.set_active(True)
		self.size = 100

		entry.connect("activate", self.on_search,  darea)
		btn1.connect("toggled", self.on_selected, darea)
		btn2.connect("toggled", self.on_selected, darea)
		btn3.connect("toggled", self.on_selected, darea)
		darea.connect("draw", self.draw)
		
		hbox = Gtk.HBox()
		hbox.pack_start(entry, True, True, 0)
		hbox.pack_start(btn1, False, True, 0)
		hbox.pack_start(btn2, False, True, 0)
		hbox.pack_start(btn3, False, True, 0)

		vbox = Gtk.VBox()
		vbox.pack_start(hbox, False, True, 0)
		vbox.pack_start(darea, True, True, 0)

		self.window.add(vbox)

		self.window.show_all()
		
	def destroy(self, widget, data=None):
		Gtk.main_quit()

	def main(self):
		Gtk.main()

	def on_search(self, widget, darea):
		q = widget.get_text()

		self.points = []
		if False:
			qvector = self.h[dims[q]]
			for i in range(self.m.shape[0]):
				prod = numpy.inner(qvector,self.c*self.m[i])
				for j in range(len(self.points)):
					if self.points[j][0] < prod:
						self.points.insert(j,(prod,i))
						if len(self.points) > 100:
							self.points.pop()
						break
				if len(self.points) < 100:
					self.points.append((prod,i))
		else:
			qvector = (self.h[dims[q]] + self.m[dims[q]])/2
			for i in range(self.m.shape[0]):
				vector = (self.h[i] + self.m[i])/2
				dist   = numpy.linalg.norm(qvector-vector)
				for j in range(len(self.points)):
					if self.points[j][0] > dist:
						self.points.insert(j,(dist,i))
						if len(self.points) > 100:
							self.points.pop()
						break
				if len(self.points) < 100:
					self.points.append((dist,i))

		tmp = numpy.array([self.m[i] for (prod,i) in self.points[:self.size]])
		self.X = manifold.TSNE(n_components=2, verbose=1).fit_transform(tmp)
		darea.queue_draw()

	def on_selected(self, widget, darea):
		self.size = widget.size
		tmp = numpy.array([self.m[i] for (prod,i) in self.points[:self.size]])
		self.X = manifold.TSNE(n_components=2, verbose=1).fit_transform(tmp)
		darea.queue_draw()

	def draw(self, widget, cr):
		if not hasattr(self, 'points'):
			return

		cr.set_line_width(9)
		cr.set_source_rgb(0.7, 0.2, 0.0)

		w,h = self.window.get_size()
		(minx,maxx) = (float('inf'),float('-inf'))
		(miny,maxy) = (float('inf'),float('-inf'))
		for [x,y] in self.X:
			minx = min(minx,x)
			maxx = max(maxx,x)
			miny = min(miny,y)
			maxy = max(maxy,y)
		sx = w/(maxx-minx)
		sy = h/(maxy-miny)
		s = min(sx,sy)

		cr.translate(w/2, h/2)
		cr.set_source_rgb(0.3, 0.4, 0.6)
		for i in range(len(self.X)):
			p = self.X[i]
			cr.move_to(p[0]*s, p[1]*s)
			cr.show_text(self.funs[self.points[i][1]])

if training:
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

	numpy.savetxt("h.txt", h)
	numpy.savetxt("c.txt", c)
	numpy.savetxt("m.txt", m)
else:
	h = numpy.loadtxt("h.txt")
	c = numpy.loadtxt("c.txt")
	m = numpy.loadtxt("m.txt")
	
	print(h[dims["apple_1_N"]])
	print(h[dims["pear_1_N"]])

	viewer = Viewer(h,c,m,dims,funs)
	viewer.main()
