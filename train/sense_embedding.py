from scipy import sparse
from scipy.sparse import linalg
from scipy import spatial
from sklearn import manifold
import numpy
import gtk
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

m = sparse.csr_matrix((vals,(rows,cols)),shape=(len(dims),len(dims)))

del rows
del cols
del vals
del add

sys.setrecursionlimit(10000)

class Viewer:
	def __init__(self, X, dims, funs):
		self.X      = X
		self.dims   = dims
		self.funs   = funs
		self.kdtree = spatial.KDTree(X)

		self.r      = 2

		self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
		self.window.connect("destroy", self.destroy)
		self.window.resize(1000,700)

		entry = gtk.Entry()
		btn1  = gtk.RadioButton(None, "Radius 0.5")
		btn2  = gtk.RadioButton(btn1, "Radius 1")
		btn3  = gtk.RadioButton(btn2, "Radius 2")
		darea = gtk.DrawingArea()

		btn1.r = 0.5
		btn2.r = 1
		btn3.r = 2

		btn3.set_active(True)

		entry.connect("activate", self.on_search,  darea)
		btn1.connect("toggled", self.on_selected, darea)
		btn2.connect("toggled", self.on_selected, darea)
		btn3.connect("toggled", self.on_selected, darea)
		darea.connect("expose-event", self.expose)
		
		hbox = gtk.HBox()
		hbox.pack_start(entry, True)
		hbox.pack_start(btn1, False)
		hbox.pack_start(btn2, False)
		hbox.pack_start(btn3, False)

		vbox = gtk.VBox()
		vbox.pack_start(hbox, False)
		vbox.pack_start(darea, True)

		self.window.add(vbox)

		self.window.show_all()
		
	def destroy(self, widget, data=None):
		gtk.main_quit()

	def main(self):
		gtk.main()
		
	def on_search(self, widget, darea):
		query = widget.get_text()
		
		self.q      = X[dims[query]]
		self.points = self.kdtree.query_ball_point(self.q,self.r)
		darea.queue_draw()

	def on_selected(self, widget, darea):
		self.r = widget.r
		darea.queue_draw()

	def expose(self, widget, event):
		if not hasattr(self, 'q'):
			return

		cr = widget.window.cairo_create()

		cr.set_line_width(9)
		cr.set_source_rgb(0.7, 0.2, 0.0)

		w = self.window.allocation.width
		h = self.window.allocation.height
		s = min(w,h)/(2*self.r)

		cr.translate(w/2-self.q[0]*s, h/2-self.q[1]*s)
		cr.set_source_rgb(0.3, 0.4, 0.6)
		for i in range(len(self.points)):
			p = self.X[self.points[i]]
			cr.move_to(p[0]*s, p[1]*s)
			cr.show_text(self.funs[self.points[i]])


if training:
	print m.shape

	(u,s,v) = linalg.svds(m,5000)
	v = numpy.transpose(v)
	print u.shape
	print s.shape
	print v.shape

	del m

	print numpy.inner(u[dims["regatta_N"]], numpy.diag(s).dot(v[dims["lurid_4_A"]]))
	print numpy.inner(u[dims["head_14_N"]], numpy.diag(s).dot(v[dims["both7and_DConj"]]))
	print numpy.inner(u[dims["apple_1_N"]], numpy.diag(s).dot(v[dims["crimson_1_A"]]))

	numpy.save("u.npy", u)
	numpy.save("s.npy", s)
	numpy.save("v.npy", v)
	
	X = manifold.TSNE(n_components=2, verbose=1).fit_transform(u)
	numpy.save("X.npy", X)
else:
#	u = numpy.load("u.npy")
#	s = numpy.load("s.npy")
#	v = numpy.load("v.npy")
	X = numpy.load("X.npy")

	viewer = Viewer(X,dims,funs)
	viewer.main()
