# A Python Interface to GF WordNet

The [GF WordNet](https://cloud.grammaticalframework.org/wordnet/)
(on [GitHub](https://github.com/GrammaticalFramework/gf-wordnet)).
can be used as a regular GF grammar or you can also use
it as a standalone Python library similar in style to `nltk.corpus.wordnet`.
The added benefit is that contrary to the usual WordNets in GF WordNet
you also have the RGL abstract syntax trees which lets you to 
compose sentences in serveral languages.

The easiest way to get the library is via pip:
```console
$ pip3 install gf-wordnet
```
This will install the library with its dependencies but it will not install the WordNet grammar.
You can download the latest precompiled version of the grammar as follows:
```Python
>>> import wordnet
Either use wordnet.download(['ISO 639‑2 code1', ...]) to download the grammar,
or use wordnet.symlink('path to a folder') to link the library to an existing grammar.
If download() is called without an argument it will download all languages.
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

## Words

Look up the senses of a word form by using `synsets()`; this function
has an optional `cat` argument which lets you constrain the category of
the word:
```Python
>>> synsets('eng','dog')
[Synset('02086723-n'), Synset('10133978-n'), Synset('10042764-n'),
Synset('09905672-n'), Synset('07692347-n'), Synset('03907626-n'),
Synset('02712903-n'), Synset('02005890-v')]
>>> synsets('eng','dog', cat='V2')
[Synset('02005890-v')]
>>> synsets('eng','dogged')
[Synset('02005890-v')]
```

Since in GF WordNet we have inflection tables and not just lemmas,
the look up works on any inflection form and not only on the lemma.
On the other hand, some languages (Finnish, Zulu) store in the inflection tables
stems rather than full forms. In that case the lookup will work on the stems instead.

A synset is most often identified with its offset in Princeton WordNet 3.1,
other senses are identified by their Qid in WikiData. Some senses have
both WordNet offset and a Qid:
```Python
>>> synset('02086723-n')
Synset('02086723-n')
>>> print(synset('02086723-n').definition())
a member of the genus Canis (probably descended from the common wolf) that has been domesticated by man since prehistoric times; occurs in many breeds
>>> len(synset('02086723-n').examples())
21
>>> print(synset('02086723-n').examples()[0])
PhrUtt NoPConj (UttNP (AdvNP (DetCN (DetQuant DefArt NumSg) (AdjCN (PositA absurd_2_A) (UseN excuse_1_N))) (SubjS that_Subj (UseCl (TTAnt TPast ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN dog_1_N)) (ComplSlash (SlashV2a eat_3_V2) (DetCN (DetQuant (PossPron he_Pron) NumSg) (UseN homework_N)))))))) NoVoc
>>> linearize('eng',synset('02086723-n').examples()[0])
the absurd excuse that the dog ate his homework
>>> synset('02086723-n').lexemes()
[Lexeme('dog_1_N')]
>>> [lexeme.linearization("eng") for lexeme in synset('02086723-n').lexemes()]
['dog']
>>> lexeme('dog_1_N').synset()
Synset('02086723-n')
>>> synset('Q1075128').lexemes()
[Lexeme('izvornik_8_LN')]
>>> synset('Q1075128').definition()
'village of Bulgaria'
```
Note that Princeton WordNet contains only one example for the sense dog_1_N. In GF WordNet, on the other hand,
the corpus is parsed and sense disambiguated. Thanks to that we have identified 22 examples, so far.

All functions and methods which work on a specific language take the language code as their first
argument. You can find the list of languages with the function `lang`. After that you can lookup a word,
in any of the languages:
```Python
>>> langs()
['afr', 'bul', 'cat', 'chi', 'dut', 'eng', 'est', 'fin', 'fre', 'ger', 'ita', 'kor', 'mlt', 'pol', 'por', 'ron', 'rus', 'slv', 'som', 'spa', 'swa', 'swe', 'tha', 'tur', 'zul']
>>> synsets('swe','hund')
[Synset('02086723-n'), Synset('10042764-n'), Synset('09905672-n'), Synset('02087384-n'), Synset('Q31385072'), Synset('Q37575615')]
>>> synset('02086723-n').linearizations('swe')
['hund']
>>> synset('02086723-n').linearizations('bul')
['куче']
>>> lexemes('bul','куче')
[Lexeme('canine_2_N'), Lexeme('dog_1_N'), Lexeme('dog_3_N'), Lexeme('dog_4_N'), Lexeme('pooch_N'), Lexeme('tike_1_N'), Lexeme('tyke_2_N'), Lexeme('cuche_7_SN'), Lexeme('kuče_3_LN'), Lexeme('küche_3_LN'), Lexeme('küche_4_SN')]
```

You can also search for synonyms which are returned as a list of sets for the different senses of the input word
in the given language, since these different senses are not mutual synonyms:
```Python
>>> synonyms('eng','car')
[{'machine', 'auto', 'automobile', 'motorcar'}, {'railcar'}, {'gondola'}, {'cable-car'}]
>>> synonyms('spa', 'coche')
[{'vagón'}, {'vagón'}, {'coche'}, {'auto', 'carro', 'máquina', 'automóvil'}, {'coche'}, {'carmelita', 'vagón'}]
```

## Synsets
Synset: a set of synonyms that share a common meaning.

```Python
>>> dog = synset('02086723-n')
>>> dog.hypernyms()
[Synset('02085998-n'), Synset('01320032-n')]
>>> dog.hyponyms()
[Synset('01325095-n'), Synset('02087384-n'), Synset('02087513-n'), Synset('02087924-n'), ...]

>>> dog.member_holonyms()
[Synset('02086515-n'), Synset('08011383-n')]
>>> cat = synset('02124272-n')
>>> lowest_common_hypernyms(dog, cat)
[Synset('02077948-n')]
>>> synset('02077948-n').definition()
a terrestrial or aquatic flesh-eating mammal
```

Note that some relations are defined in WordNet only over Lemmas:
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

## Lexemes

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

Some lexemes are linked to a Wikidata Qid and Wikipedia pages:
```Python
>>> sweden = lexeme('sweden_LN')
>>> sweden.qid()
'Q34'
>>> sweden.links()
[('Q34', 'Sweden', 'commons/0/06/EU-Sweden.svg'), ('Q34', 'Sweden', 'commons/2/28/Sweden_on_the_globe_(Europe_centered).svg'), ('Q34', 'Sweden', 'commons/3/30/Sweden_(orthographic_projection).svg'), ('Q34', 'Sweden', 'commons/4/4c/Flag_of_Sweden.svg'), ('Q34', 'Sweden', 'commons/7/7a/LocationSweden.svg'), ('Q34', 'Sweden', 'commons/a/a1/Shield_of_arms_of_Sweden.svg'), ('Q34', 'Sweden', 'commons/e/e5/Great_coat_of_arms_of_Sweden.svg')]
```

## Similarity

Synsets can be compared for similarity:
```Python
>>> dog   = synset('02086723-n')
>>> cat   = synset('02124272-n')
>>> human = synset('02474924-n')
>>> shortest_path_distance(dog,cat)
4
>>> path_similarity(dog,cat)
0.2
>>> shortest_path_distance(dog,human)
6
>>> path_similarity(dog,human)
0.14285714285714285
```

You can also search for similar lexemes by first finding the lowest common
hypernum:
```Python
>>> dog = synset('02086723-n')
>>> cat = synset('02124272-n')
>>> [carnivore] = lowest_common_hypernyms(dog, cat)
>>> carnivore.definition()
'a terrestrial or aquatic flesh-eating mammal'
>>> similar = carnivore.full_hyponyms()
>>> len(similar)
370
>>> [synset.lexemes() for synset in similar]
[[Lexeme('dog_1_N')], [Lexeme('puppy_1_N')], 
 [Lexeme('bow_wow_2_N'), Lexeme('doggie_N'), Lexeme('doggy_N'), Lexeme('pooch_N'), ...]
 ...
]
```

## Syntax

You can use the lexicon to compose phrases:
```Python
>>> expr = mkCN(lexeme('red_1_A').expression(), lexeme('apple_1_N').expression())
>>> linearize('eng', expr)
'red apple'
>>> linearize('swe', expr)
'rött äpple'
```
Since looking up a lexeme and composing an expression with it is very common, there is also a simpler way:
```Python
>>> expr = mkCN(w.red_1_A, w.apple_1_N)
>>> linearize('eng', expr)
'red apple'
>>> linearize('swe', expr)
'rött äpple'
```

The API for building phrases is mostly the same as the [RGL API](https://www.grammaticalframework.org/lib/doc/synopsis/index.html).
