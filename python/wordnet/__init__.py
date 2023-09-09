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

def download(langs=None, path=None):
    """
    Downloads the precompiled wordnet grammar from the GF server.
    If langs is None, then all languages will be downloaded.
    Alternatively, langs must contain the list of language codes
    for which the grammar should be fetched.

    If path is not given the grammar will be downloaded in the package's
    installation folder.
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

    if not path:
        path = os.path.dirname(__file__) + "/../"

    with contextlib.closing(urllib.request.urlopen("https://cloud.grammaticalframework.org/robust/Parse.ngf?command=download"+langs)) as fp:
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
    urllib.request.urlretrieve("https://cloud.grammaticalframework.org/robust/semantics.db", path+"semantics.db", reporthook)
    os.chmod(path+"semantics.db", 0o666)
    print("\b\b\bdone")

    print("Reload wordnet")
    importlib.reload(sys.modules[__name__])

try:
    import Parse as w
except ModuleNotFoundError as e:
    print("Either use wordnet.download(['ISO 639‑2 code1', ...]) to download the grammar,\n"
          "or add the path to an existing grammar in sys.path. If download() is called\n"
          "without an argument it will download all languages.")

    __all__ = ["download"]
else:
    from wordnet._api import *
    from wordnet._semantics import Synset, Lexeme, Status, get_lexeme as lexeme, get_wikilexemes as wikilexemes, get_lexemes as lexemes, get_synset as synset, get_synsets as synsets, synonyms

    __all__ = ['Lexeme', 'Synset', 'Status', 'aPl_Det', 'aSg_Det', 'a_Det',
               'a_Quant', 'anteriorAnt', 'comparAP', 'conditionalTense',
               'download', 'exclMarkPunct', 'fullStopPunct',
               'futureTense', 'genericCl', 'grammar', 'he_NP', 'i_NP',
               'it_NP', 'lets_Utt', 'lexeme', 'wikilexemes', 'lexemes',
               'synonyms', 'linearize', 'mkAP',
               'mkAdN', 'mkAdv', 'mkCN', 'mkCard', 'mkCl', 'mkClSlash',
               'mkComp', 'mkDecimal', 'mkDet', 'mkDigits', 'mkIAdv',
               'mkIComp', 'mkIDet', 'mkIP', 'mkImp', 'mkList', 'mkNP',
               'mkNum', 'mkNumeral', 'mkOrd', 'mkPConj', 'mkPhr',
               'mkQCl', 'mkQS', 'mkQuant', 'mkRCl', 'mkRP', 'mkRS',
               'mkS', 'mkSC', 'mkSSlash', 'mkSub100', 'mkSub1000',
               'mkTemp', 'mkUtt', 'mkUttImpPl', 'mkUttImpPol',
               'mkUttImpSg', 'mkVP', 'mkVPSlash', 'mkVoc', 'n0_Dig',
               'n1000_Digits', 'n1000_Numeral', 'n100_Digits',
               'n100_Numeral', 'n10_Digits', 'n10_Numeral', 'n1_Dig',
               'n1_Digits', 'n1_Numeral', 'n20_Digits', 'n20_Numeral',
               'n2_Dig', 'n2_Digits', 'n2_Numeral', 'n3_Dig',
               'n3_Digits', 'n3_Numeral', 'n4_Dig', 'n4_Digits',
               'n4_Numeral', 'n5_Dig', 'n5_Digits', 'n5_Numeral',
               'n6_Dig', 'n6_Digits', 'n6_Numeral', 'n7_Dig',
               'n7_Digits', 'n7_Numeral', 'n8_Dig', 'n8_Digits',
               'n8_Numeral', 'n9_Dig', 'n9_Digits', 'n9_Numeral',
               'negativePol', 'noPConj', 'noVoc', 'passiveVP',
               'pastTense', 'pluralNum', 'positivePol', 'presentTense',
               'progressiveVP', 'questMarkPunct', 'reflAP',
               'reflexiveVP', 'she_NP', 'simultaneousAnt',
               'singularNum', 'synset', 'synsets', 'tenfoldSub100',
               'that_Det', 'that_NP', 'thePl_Det', 'theSg_Det', 'the_Det',
               'the_Quant', 'these_Det', 'these_NP', 'they_NP',
               'this_Det', 'this_NP', 'those_Det', 'those_NP',
               'thousandfoldNumeral', 'w', 'we_NP', 'what_IP',
               'whichPl_IDet', 'whichSg_IDet', 'which_IDet',
               'which_RP', 'who_IP', 'youPl_NP', 'youPol_NP',
               'you_NP']
