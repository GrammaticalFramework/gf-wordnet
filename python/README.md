The library is supposed to be used by embedding the GF WordNet grammar
in the module `wordnet`. For example:
```Python
import pgf

gr=pgf.readNGF("Parse.ngf")
gr.embed("wordnet")
```
will make the raw abstract syntax available from the module `wordnet`.
For example:
```Python
> import wordnet as w
> print(w.MassNP(w.AdjCN(w.PositA(w.red_1_A),w.UseN(w.apple_1_N))))
MassNP (AdjCN (PositA red_1_A) (UseN apple_1_N))
```

Note that the wordnet module is quite big which makes imports like:
```Python
from wordnet import *
```
quite slow. It is better to use qualified imports and give the module
a short alias, like `w` in the above example.

The real benefit is that by using this library you can even use
the RGL API directly from Python. For example:
```Python
import wordnet as w
from wordnet.api import *

print(mkNP(mkCN(w.red_1_A,w.apple_1_N)))
```
will print the same abstract syntax tree as in the original example.
