# GF WordNet

1. [The Lexicon](#the-lexicon)
2. [WordNet Domains](#wordnet-domains)
3. [Treebank](#treebank)
4. [VerbNet](#verbnet)
5. [Wikipedia Images](#wikipedia-images)
6. [Browsing and Editing](#browsing-and-editing)
7. [The Python Interface](#the-python-interface)
     - [Words](#words)
     - [Synsets](#synsets)
     - [Lexemes](#lexemes)
     - [Verb Frames](#verb-frames)
     - [Verb Frames](#verb-frames)
     - [Similarity](#similarity)
     - [Synset Closures](#synset-closures)

The GF WordNet is a lexicon based on the [Princeton WordNet](https://wordnet.princeton.edu/) and [Wikidata](https://www.wikidata.org/wiki/Wikidata:Main_Page)
but adapted to integrate with the [GF Resource Grammars Library](https://github.com/GrammaticalFramework/gf-rgl). Following the GF model, the lexicon consists
of an abstract syntax with one abstract identifier for each word sense. The concrete syntaxes define the corresponding linearizations in each language.
A synset, then, consists of a set of abstract identifiers instead words. 

The lexicon includes nouns, verb, adjectives and adverbs from WordNet as well as people and place names from Wikidata. Some structural words such as prepositions
and conjunctions are also included. The overal size is summarized in the table bellow:
| WordNet  | adjectives, nouns, verbs, etc. | 100 thousand |
|----------|--------------------------------|--------------|
| Wikidata | Given names                    | 64 thousand  |
|          | Family names                   | 531 thousand |
|          | Place names                    | 3.7 million  |
|          | total                          | 4.3 million  |

The initial development was mostly focused on English, Swedish and Bulgarian. WordNets for all other languages were bootstrapped
from existing resources and aligned by using statistical methods. They are only partly checked by either matching with Wikipedia or by human feedback.
Many of the translations may be correct but inconsistancies can be expected as well. For details check:

* [Krasimir Angelov. A Parallel WordNet for English, Swedish and Bulgarian. LREC 2020](http://www.lrec-conf.org/proceedings/lrec2020/pdf/2020.lrec-1.368.pdf)
* [Krasimir Angelov, Gleb Lobanov. Predicting Translation Equivalents in Linked WordNets. COLING 2016.](https://www.aclweb.org/anthology/W16-4504.pdf)

Unlike the original WordNet we focus on grammatical, morphological as well as semantic features. All this is simply necessary to make the lexicon
compatible with the Resource Grammars Library (RGL).

## The Lexicon

Each entry in the lexicon represents the full morphology, precise syntactic
category as well as one particular sense of a word. When words
across different languages share the same meaning then 
they are represented as a single cross lingual id. For example
in [WordNetEng.gf](WordNetEng.gf) we have all those definitions of blue in English:

```GF
lin blue_1_A = mkA "blue" "bluer";
lin blue_2_A = mkA "blue" "bluer";
lin blue_3_A = mkA "blue" "bluer";
lin blue_4_A = mkA "blue" "bluer";
lin blue_5_A = mkA "blue" "bluer";
lin blue_6_A = mkA "blue" "bluer";
lin blue_7_A = mkA "blue" "bluer";
lin blue_8_A = mkA "blue" "bluer";
```

since they represent different senses and thus different translations
in [WordNetSwe.gf](WordNetSwe.gf) and [WordNetBul.gf](WordNetBul.gf):

```GF
lin blue_1_A = L.blue_A ;
lin blue_2_A = L.blue_A ; --guessed
lin blue_3_A = mkA "deppig" ;
lin blue_4_A = mkA "vulgär" ;
lin blue_5_A = mkA "pornografisk" ;
lin blue_6_A = mkA "aristokratisk" ;
lin blue_7_A = L.blue_A ; --guessed
lin blue_8_A = L.blue_A ; --guessed
```

```GF
lin blue_1_A = mkA086 "син" ;
lin blue_2_A = mkA086 "син" ; --guessed
lin blue_3_A = mkA076 "потиснат" ;
lin blue_4_A = mkA079 "вулгарен" ;
lin blue_5_A = mkA078 "порнографски" ;
lin blue_6_A = mkA079 "аристократичен" ;
lin blue_7_A = mkA086 "син" ; --guessed
lin blue_8_A = mkA086 "син" ; --guessed
```

The definitions are using the standard RGL syntactic categories
which are a lot more descriptive than the tags ´n´, ´v´, ´a´, and ´r´
in the WordNet. In addition we use the RGL paradigms to implement the morphology.

Note also that not all translations are equally reliable for all languages.
In the example above, the comment `--guessed` means that the translation
was extracted from an existing translation lexicon, but we are not sure if
it accurately represents the right sense. Similarly sometimes you can
also see the comment `--unchecked`, which means that the chosen translation
comes from an existing WordNet but still further checking is needed to guarantee
that this is the most idiomatic translation.

The English lexicon contains also information about the gender.
All senses that refer to a human being are tagged with either 
´masculine´, ´feminine´ or ´human´ gender. In some cases where the word
is either masculine or feminine then it is further split into two senses.
In those cases there is usually a different translation in many languages.
The information about which words refer to humans is based on
the WordNet hierarchy. In the English RGL the gender information
is relevant, for instance when choosing between who/which and herself/himself/itself.

The abstract syntax [WordNet.gf](WordNet.gf) defines all abstract ids in 
the lexicon. Almost all definitions are also followed by a comment
which consists of, first the corresponding WordNet 3.1 synset offset,
followed by dash and then the wordnet tag. After that there is a tab
followed by the WordNet gloss. Like in WordNet the gloss is followed
by a list of examples, but here we retain only the examples relevant
to the current lexical id. In other words, if a synset contains several
words, only the examples including the current abstract id are retained.

The verbs in the lexicon are also distinguished based on their valency,
i.e. transitive, intransitive, ditransitive, etc. The valency
of a verb in a given sense is determined by its example, but there is
also a still partial integration of [VerbNet](https://verbs.colorado.edu/verbnet/).

Some of the nouns and the adjectives are typically accompanied by
prepositions and a noun phrase. In those cases they are classified as
N2 and A2. This helps in parsing and also let us to choose the
right preposition in every language.


## WordNet Domains

The data also integrates [WordNet Domains](http://wndomains.fbk.eu/). If the synset for 
the current entry has domain(s) in WordNet Domains, then they
are listed in the abstract syntax, at the beginning of the gloss, surrounded by square brackets.
In addition to those, some more domains are added by analysing the glosses
in the original WordNet. The taxonomy of the domains is stored in
[domains.txt](domains.txt)

## Treebank

In order to make the lexical development more stable we have also started
a treebank consisting of all examples from the Princeton WordNet (see [examples.txt](examples.txt)).
The examples are automatically parsed with the [Parse.gf](Parse.gf) grammar in GF.
For each example there is also the original English sentence, as well
as the Swedish and Bulgarian seed translations from Google Translate.

Some of the examples are already checked. This means that 
the abstract syntax tree is corrected, the right senses are used
and the seed translations are replaced with the translations obtained
by using the Parse grammar. The translations are also
manually checked for validity.

The format of a treebank entry is as follows:
```
abs: PhrUtt NoPConj (UttS (UseCl (TTAnt TPast ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN storm_1_N)) (UseV abate_2_V)))) NoVoc
eng: The storm abated
swe: stormen avtog
bul: бурята отслабна
key: 1 abate_2_V 00245945-v
```

If the first line starts with "abs*" instead of "abs:" then
the entry is not checked yet. If the line starts with "abs^" then
the entry is checked but some more work is needed. 

The last line in the entry contains
the abstract ids for which this example is intended. The number 1
here means that there is only one id but there could be more if they
share examples. After the abstract ids is the synset offset in WordNet 3.1.
If the same synset has translations in either BulTreebank WordNet
or Svenskt OrdNät then they are also listed after the offset. 
When possible these translations should be used.

## VerbNet

[VerbNet](https://verbs.colorado.edu/verbnet/) is invaluable in ensuring
that the verb valencies are as consistent as possible. The VerbNet also
gives us the semantics of the verbs as a first-order logical formula.

The VerbNet classes and frames are stored in [examples.txt](examples.txt).
For instance the following record encodes class `begin-55.1`.
```
class: begin-55.1
role:  Agent +animate +organization
role:  Theme
role:  Instrument
```
After the class definition, there are one or more frame definitions:
```
frm: PredVP Agent (ComplVV Verb ? ? Theme)
sem: begin(E,Theme), cause(Agent,E)
key: go_on_VV proceed_VV begin_to_1_VV start_to_1_VV commence_to_1_VV resume_to_1_VV undertake_1_VV get_34_VV set_about_3_VV set_out_1_VV get_down_to_VV
```
The line marked with `frm:` represents the syntax of the frame
expressed in the abstract syntax. After that `sem:` contains
the first-order formula. Finally `key:` lists all abstract
syntax entries belonging to that frame.

After a frame, there might be one or more examples which illustrate
how the frame is applied to different members of the frame.

## Wikipedia Images

Quite often the glosses are not clear enough to understand the meaning of
a synset. In the initial project development, about 1/5 of the lexical entries
in the lexicon were linked to the corresponding Wikipedia articles. This has
several benefits: the articles are way more informative than the glosses;
they are also translated which helps in the bootstrapping of other languages.
Finally we can now also show images associated with several of the
lexemes which, as we know, is worth more than thousands words.

Later the links created in this project were merged with the links that Wikidata
provides via property [P8814](https://www.wikidata.org/wiki/Property:P8814). This
resulted in a large set of links which is also of a supperior quality than what
GF WordNet and Wikidata had in advance.

In order to speed up compilation the set on links is cached in the file
[images.txt](images.txt) which can be regenerated at any time by running the script
[bootstrap/images.hs](bootstrap/images.hs). The file looks as follows:
```
gothenburg_1_LN	Q25287;Gothenburg;commons/3/30/Flag_of_Gothenburg.svg	Q25287;Gothenburg;commons/a/a8/Gothenburg_new_montage_2012.png
```
This is a space separated record. The value in the first field is the abstract lexical id,
which is followed by one field per image. The image field consists of three parts separated
by semicolumns. The first part is the Wikidata Qid, the second is 
the relative page location in the English Wikipedia, and the last one is the relative path to
the thumbnail image.


## Browsing and Editing

An online search interface for the lexicon is available here:

https://cloud.grammaticalframework.org/wordnet/

If you have editing rights to the [GF WordNet repository](https://github.com/GrammaticalFramework/gf-wordnet), then
it is also possible to log in and edit the data via the Web interface.
Both the lexicon and the corpus are editable. Once you finish a batch of changes you can also commit directly.


## The Python Interface

You can use the WordNet as a regular GF grammar or you can also use it
as a standalone Python library similar in style to `nltk.corpus.wordnet`.
The added benefit is that in GF WordNet you also have the RGL
abstract syntax trees which lets you to compose sentences in serveral
languages.

The easiest way to get the library is via pip:
```console
$ pip3 install gf-wordnet
```
This will install the library with its dependencies but it will not install the WordNet grammar.
You can download the latest precompiled version of the grammar as follows:
```Python
>>> import wordnet
Either use wordnet.download(['ISO 639‑2 code1', ...]) to download the grammar,
or add the path to an existing grammar in sys.path. If download() is called
without an argument it will download all languages.
>>> wordnet.download(['eng'])
Download and boot the grammar 355MB (Expanded to 2637MB)
Download the semantics database 2733MB done
Reload wordnet
```
When there is no grammar installed, the library prints a warning and then the `download` function
is the only one that you can use. If you want more languages, add them in the list. If you call
`download` with no arguments, all languages will be downloaded. Expect the grammar to be
around 50GB in that case.

The wordnet library searches for a file called `Parse.pgf` or `Parse.ngf` in the path for Python modules and
uses it as a grammar. You can change the path by either manipulating `sys.path` from Python or by
setting the `PYTHONPATH` environment variable. This is useful if you already have the grammar
stored somewhere else and you need to tell Python where to find it.

**Note:** When you use the `download` function, the grammar will be downloaded in the folder where the Python
library is installed. This means that if you have installed the library globally, then it will try to
store the grammar under `/usr/local/lib/python3.X`. For that to work you need to run the python shell
as root during the download. After that you can use the library and the grammar from all users.
On the other hand, by default `pip` installs libraries under `/home/krasimir/.local/lib/python3.X`,
so you don't need to do anything special.

After the download is finished, you can import the library like this:
```Python
>>> import wordnet
```
For more compact code, we recommend:
```Python
>>> from wordnet import *
```

### Words

Look up a word using synsets(); this function has an optional cat
argument which lets you constrain the category of the word:
```Python
>>> synsets('eng','dog')
[Synset('02086723-n'), Synset('10133978-n'), Synset('10042764-n'),
Synset('09905672-n'), Synset('07692347-n'), Synset('03907626-n'),
Synset('02712903-n'), Synset('02005890-v')]
>>> synsets('eng','dog', cat='V2')
[Synset('02005890-v')]
```
You can use any category defined in the RGL. A synset is most often
identified with its offset in Princeton WordNet 3.1. For words that
are not there we use the Qid in WikiData if possible.

TODO: Expand on Morphology
TODO: there are more examples per lemma

```Python
>>> synset('02086723-n')
Synset('02086723-n')
>>> print(synset('02086723-n').definition())
a member of the genus Canis (probably descended from the common wolf) that has been domesticated by man since prehistoric times; occurs in many breeds
>>> len(synset('02086723-n').examples())
21
>>> print(synset('02086723-n').examples()[0])
PhrUtt NoPConj (UttNP (AdvNP (DetCN (DetQuant DefArt NumSg) (AdjCN (PositA absurd_2_A) (UseN excuse_1_N))) (SubjS that_Subj (UseCl (TTAnt TPast ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN dog_1_N)) (ComplSlash (SlashV2a eat_3_V2) (DetCN (DetQuant (PossPron he_Pron) NumSg) (UseN homework_N)))))))) NoVoc
>>> synset('02086723-n').lexemes()
[Lexeme('dog_1_N')]
>>> [lexeme.linearization("eng") for lexeme in wn.synset('02086723-n').lexemes()]
['dog']
>>> lexeme('dog_1_N').synset()
Synset('02086723-n')
```

```Python
>>> langs()
['afr', 'bul', 'cat', 'chi', 'dut', 'eng', 'est', 'fin', 'fre', 'ger', 'ita', 'kor', 'mlt', 'pol', 'por', 'ron', 'rus', 'slv', 'som', 'spa', 'swa', 'swe', 'tha', 'tur', 'zul']
>>> synsets('swe','hund')
[Synset('02086723-n'), Synset('10042764-n'), Synset('09905672-n'), Synset('02087384-n'), Synset('Q31385072'), Synset('Q37575615')]
>>> Synset('02086723-n').linearizations('swe')
['hund']
>>> Synset('02086723-n').linearizations('bul')
['куче']
>>> lexemes('bul','куче')
[Lexeme('canine_2_N'), Lexeme('dog_1_N'), Lexeme('dog_3_N'), Lexeme('dog_4_N'), Lexeme('pooch_N'), Lexeme('tike_1_N'), Lexeme('tyke_2_N'), Lexeme('cuche_7_SN'), Lexeme('kuče_3_LN'), Lexeme('küche_3_LN'), Lexeme('küche_4_SN')]
>>> sorted(wn.synset('dog.n.01').lemmas('dan'))
[Lemma('dog.n.01.hund'), Lemma('dog.n.01.k\xf8ter'),
Lemma('dog.n.01.vovhund'), Lemma('dog.n.01.vovse')]
>>> sorted(wn.synset('dog.n.01').lemmas('por'))
[Lemma('dog.n.01.cachorra'), Lemma('dog.n.01.cachorro'), Lemma('dog.n.01.cadela'), Lemma('dog.n.01.c\xe3o')]
>>> dog_lemma = wn.lemma(b'dog.n.01.c\xc3\xa3o'.decode('utf-8'), lang='por')
>>> dog_lemma
Lemma('dog.n.01.c\xe3o')
>>> dog_lemma.lang()
'por'
>>> len(list(wordnet.all_lemma_names(pos='n', lang='jpn')))
66031
```

The synonyms of a word are returned as a list of sets for the different senses of the input word in the given language, since these different senses are not mutual synonyms:
```Python
>>> synonyms('eng','car')
[{'machine', 'auto', 'automobile', 'motorcar'}, {'railcar'}, {'gondola'}, {'cable-car'}]
>>> synonyms('spa', 'coche')
[{'vagón'}, {'vagón'}, {'coche'}, {'auto', 'carro', 'máquina', 'automóvil'}, {'coche'}, {'carmelita', 'vagón'}]
```

### Synsets
Synset: a set of synonyms that share a common meaning.

```Python
>>> dog = synset('02086723-n')
>>> dog.hypernyms()
[Synset('02085998-n'), Synset('01320032-n')]
>>> dog.hyponyms()
[Synset('01325095-n'), Synset('02087384-n'), Synset('02087513-n'), Synset('02087924-n'), ...]

>>> dog.member_holonyms()
[Synset('02086515-n'), Synset('08011383-n')]
>>> dog.root_hypernyms()
[Synset('entity.n.01')]
>>> wn.synset('dog.n.01').lowest_common_hypernyms(wn.synset('cat.n.01'))
[Synset('carnivore.n.01')]
```
Each synset contains one or more abstract identifiers, which represent a specific sense of a specific word.

Note that some relations are defined by WordNet only over Lemmas:
```Python
>>> good = synset('01126910-a')
>>> good.antonyms()
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'Synset' object has no attribute 'antonyms'. Did you mean: 'hyponyms'?
>>> good.lemmas()[0].antonyms()
[Lexeme('bad_1_A')]
```
The relations that are currently defined in this way are antonyms, participle, alsosee, and derived.

### Lexemes

```Python
>>> for lexeme in synset('01182162-v').lexemes():
...     print(lexeme, lexeme.prob())
...
Lexeme('eat_3_V2') 8.415824890136719
Lexeme('feed_6_V') 8.981807708740234
>>> for lexeme in lexemes('eng', 'eat', 'V2'):
...     print(lexeme, lexeme.prob())
...
Lexeme('eat_1_V2') 7.722677707672119
Lexeme('eat_3_V2') 8.415824890136719
Lexeme('eat_4_V2') 9.802119255065918
Lexeme('eat_5_V2') 9.802119255065918
Lexeme('eat_6_V2') 9.802119255065918
Lexeme('eat_away_1_V2') 9.802119255065918
Lexeme('eat_away_2_V2') 9.802119255065918
Lexeme('eat_into_V2') 9.802119255065918
Lexeme('eat_up_1_V2') 9.802119255065918
Lexeme('eat_up_2_V2') 9.802119255065918
Lexeme('eat_up_3_V2') 9.802119255065918
>>> lexeme('jump_11_V2')
Lexeme('jump_11_V2')
```
Lexemes can also have relations between them:
```Python
>>> vocal = lexeme('vocal_1_A')
>>> vocal.derived()
[Lexeme('voice_2_N'), Lexeme('vocalize_2_V2')]
>>> vocal.antonyms()
[Lexeme('instrumental_1_A')]
```
The relations above exist only on lemmas, not on synsets.

### Verb Frames

```Python
>>> wn.synset('think.v.01').frame_ids()
[5, 9]
>>> for lemma in wn.synset('think.v.01').lemmas():
...     print(lemma, lemma.frame_ids())
...     print(" | ".join(lemma.frame_strings()))
...
Lemma('think.v.01.think') [5, 9]
Something think something Adjective/Noun | Somebody think somebody
Lemma('think.v.01.believe') [5, 9]
Something believe something Adjective/Noun | Somebody believe somebody
Lemma('think.v.01.consider') [5, 9]
Something consider something Adjective/Noun | Somebody consider somebody
Lemma('think.v.01.conceive') [5, 9]
Something conceive something Adjective/Noun | Somebody conceive somebody
>>> wn.synset('stretch.v.02').frame_ids()
[8]
>>> for lemma in wn.synset('stretch.v.02').lemmas():
...     print(lemma, lemma.frame_ids())
...     print(" | ".join(lemma.frame_strings()))
...
Lemma('stretch.v.02.stretch') [8, 2]
Somebody stretch something | Somebody stretch
Lemma('stretch.v.02.extend') [8]
Somebody extend something
```

### Syntax

TODO: Using the RGL API

### Similarity
```Python
>>> dog = synset('02086723-n')
>>> cat = synset('02124272-n')
>>> hit = synset('01407698-v')
>>> slap = synset('01419525-v')
```
'synset1.path_similarity(synset2)': Return a score denoting how similar
two word senses are, based on the shortest path that connects the senses
in the is-a (hypernym/hypnoym) taxonomy. The score is in 
the range 0 to 1. By default, there is now a fake root node added
to verbs so for cases where previously a path could not be found—and
None was returned—it should return a value. The old behavior
can be achieved by setting simulate_root to be False. A score of 1
represents identity i.e. comparing a sense with itself will return 1.

```Python
>>> dog.path_similarity(cat)
0.2...
>>> hit.path_similarity(slap)
0.142...
>>> wn.path_similarity(hit, slap)
0.142...
>>> print(hit.path_similarity(slap, simulate_root=False))
None
>>> print(wn.path_similarity(hit, slap, simulate_root=False))
None
```
'synset1.lch_similarity(synset2)': Leacock-Chodorow Similarity:
Return a score denoting how similar two word senses are, based on
the shortest path that connects the senses (as above) and
the maximum depth of the taxonomy in which the senses occur.
The relationship is given as -log(p/2d) where p is
the shortest path length and d the taxonomy depth.

```Python
>>> dog.lch_similarity(cat)
2.028...
>>> hit.lch_similarity(slap)
1.312...
>>> wn.lch_similarity(hit, slap)
1.312...
>>> print(hit.lch_similarity(slap, simulate_root=False))
None
>>> print(wn.lch_similarity(hit, slap, simulate_root=False))
None
```
'synset1.wup_similarity(synset2)': Wu-Palmer Similarity:
Return a score denoting how similar two word senses are, based on
the depth of the two senses in the taxonomy and that of
their Least Common Subsumer (most specific ancestor node). Note that at
this time the scores given do not always agree with those given by
Pedersen’s Perl implementation of Wordnet Similarity.

The LCS does not necessarily feature in the shortest path connecting
the two senses, as it is by definition the common ancestor deepest in
the taxonomy, not closest to the two senses. Typically, however, 
it will so feature. Where multiple candidates for the LCS exist,
that whose shortest path to the root node is the longest
will be selected. Where the LCS has multiple paths to the root,
the longer path is used for the purposes of the calculation.

```Python
>>> dog.wup_similarity(cat)
0.857...
>>> hit.wup_similarity(slap)
0.25
>>> wn.wup_similarity(hit, slap)
0.25
>>> print(hit.wup_similarity(slap, simulate_root=False))
None
>>> print(wn.wup_similarity(hit, slap, simulate_root=False))
None
```

### Synset Closures
Compute transitive closures of synsets
```Python
>>> dog = wn.synset('dog.n.01')
>>> hypo = lambda s: s.hyponyms()
>>> hyper = lambda s: s.hypernyms()
>>> list(dog.closure(hypo, depth=1)) == dog.hyponyms()
True
>>> list(dog.closure(hyper, depth=1)) == dog.hypernyms()
True
>>> list(dog.closure(hypo))
[Synset('basenji.n.01'), Synset('corgi.n.01'), Synset('cur.n.01'),
 Synset('dalmatian.n.02'), Synset('great_pyrenees.n.01'),
 Synset('griffon.n.02'), Synset('hunting_dog.n.01'), Synset('lapdog.n.01'),
 Synset('leonberg.n.01'), Synset('mexican_hairless.n.01'),
 Synset('newfoundland.n.01'), Synset('pooch.n.01'), Synset('poodle.n.01'), ...]
>>> list(dog.closure(hyper))
[Synset('canine.n.02'), Synset('domestic_animal.n.01'), Synset('carnivore.n.01'), Synset('animal.n.01'),
Synset('placental.n.01'), Synset('organism.n.01'), Synset('mammal.n.01'), Synset('living_thing.n.01'),
Synset('vertebrate.n.01'), Synset('whole.n.02'), Synset('chordate.n.01'), Synset('object.n.01'),
Synset('physical_entity.n.01'), Synset('entity.n.01')]
```

