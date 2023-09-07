"""
To use this library you need to place the WordNet grammar, Parse.ngf,
in a place where Python would look for modules.
The API is mostly the same as for the GF Resource Library. 

- The abstract syntax of the WordNet lexicon can be used by using
the variable "w", e.g. w.apple_1_N.

- The PGF object can be accessed through the "grammar" variable.

- For convenience, there is also the function "linearize" which
is a wrapper around the standard PGF API.
"""

import pgf

def download(langs=None):
    """
    Downloads the precompiled wordnet grammar from the GF server.
    If langs is None, then all languages will be downloaded.
    Alternatively, langs must contain the list of language codes
    for which the grammar should be fetched.
    """

    import urllib.request
    import contextlib
    import os
    import sys
    import importlib

    if langs != None:
        langs = "&lang="+" ".join(map(lambda lang: "Parse"+lang.title(), langs))
    else:
        langs = ""

    path = os.path.dirname(__file__) + "/../"

    with contextlib.closing(urllib.request.urlopen("https://www.grammaticalframework.org/robust/Parse.ngf?command=download"+langs)) as fp:
        size = 0
        def readinto(buf):
            nonlocal size
            n = fp.readinto(buf)
            size += n
            print("\rDownload and boot the grammar "+str(size//(1024*1024))+"MB",end=" ...")
            return n
        pgf.bootNGF(readinto, path+"Parse.ngf")
        os.chmod(path+"Parse.ngf", 0o666)
        size = os.path.getsize(path+"Parse.ngf")
        print("\b\b\b(Expanded to "+str(size//(1024*1024))+"MB)")

    def reporthook(blocks, bs, size):
        print("\rDownload the semantics database "+str((blocks*bs)//(1024*1024))+"MB",end=" ...")
        sys.stdout.flush()
    urllib.request.urlretrieve("https://www.grammaticalframework.org/robust/semantics.db", path+"semantics.db", reporthook)
    os.chmod(path+"semantics.db", 0o666)
    print("\b\b\bdone")

    print("Reload wordnet")
    importlib.reload(sys.modules[__name__])

try:
    import Parse
except ModuleNotFoundError as e:
    print("Either use wordnet.download(['ISO 639‑2 code1', ...]) to download the grammar,\n"
          "or add the path to an existing grammar in sys.path. If download() is called\n"
          "without an argument it will download all languages.")
else:
    from wordnet._api import *
