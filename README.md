# A WordNet in GF

This is an attempt to port the Princeton WordNet to GF. In parallel
with the pure English WordNet we also build a version for Swedish
and Bulgarian. Unlike the original WordNet we focus on grammatical,
morphological as well as semantic features. The result should be
a lexicon directly usable with the Resource Grammars Library (RGL).

## The Lexicon

Each entry in the lexicon represents the full morphology, precise syntactic
category as well as one particular sense of a word. If three words
in three different languages share the same meaning then 
they are represented as a single cross lingual id. For example
in [WordNetEng.gf](WordNetEng.gf) we have all those definitions of blue in English:

```
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

```
lin blue_1_A = L.blue_A ;
lin blue_2_A = L.blue_A ;
lin blue_3_A = mkA "deppig" ;
lin blue_4_A = mkA "vulgär" ;
lin blue_5_A = mkA "pornografisk" ;
lin blue_6_A = mkA "aristokratisk" ;
lin blue_7_A = L.blue_A ;
lin blue_8_A = L.blue_A ;
```

```
lin blue_1_A = mkA086 "син" ;
lin blue_2_A = mkA086 "син" ;
lin blue_3_A = mkA076 "потиснат" ;
lin blue_4_A = mkA079 "вулгарен" ;
lin blue_5_A = mkA078 "порнографски" ;
lin blue_6_A = mkA079 "аристократичен" ;
lin blue_7_A = mkA086 "син" ;
lin blue_8_A = mkA086 "син" ;
```

The definitions are using the standard RGL syntactic categories
with are a lot more descriptive than the tags ´n´, ´v´, ´a´, and ´r´
in the WordNet. In addition we use the RGL paradigms to implement the morphology.

In the abstract syntax [WordNet.gf](WordNet.gf) are defined all abstract ids in 
the lexicon. Almost all definitions are also followed by a comment
which consists of first the corresponding WordNet 3.1 synset offset
followed by dash and then the wordnet tag. After that there is a tab
followed by the WordNet gloss. Like in WordNet the gloss is followed
by a list of examples, but here we retain only the examples relevant
to the current lexical id. In other words if a synset contains several
words the corresponding examples are split to the relevant abstract ids.

In English the lexicon contains also information for the gender.
All senses that refer to a human being are tagged with either 
´masculine´, ´feminine´ or ´human´ gender. In some cases where the word
is either masculine or feminine then it is further split into two senses.
In those cases there is usually a different translation in Bulgarian.

## Treebank

In order to make the lexical development more stable we have also started
a treebank consisting of all examples from the Princeton WordNet (see [examples.txt](examples.txt)).
The examples are automatically parsed with the translation grammar in GF.
For each example there is also the original English sentence, as well
as the Swedish and Bulgarian seed translations from Google Translate.

Some of the examples are already checked. This means that 
the abstract syntax tree is corrected, the right senses are used
and the seed translations are replaced with the translations obtained
by using the Parse grammar in this folder. The translations are also
manually checked for validity.

The format of a treebank entry is as follows:

```
abs: PhrUtt NoPConj (UttS (UseCl (TTAnt TPast ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN storm_1_N)) (UseV abate_2_V)))) NoVoc
eng: The storm abated
swe: stormen minskade
bul: бурята отслабва
key: 1 abate_2_V 00245945-v
```

If the first line starts with "abs*" instead of "abs:" then
the entry is not checked yet. If the line starts with "abs^" then
the entry is checked by some more work is needed. 

The last line in the entry contains
the abstract ids for which this example is intended. The number 1
here means that there is only one id but there could be more if they
share examples. After the abstract ids is the synset offset in WordNet 3.1.
If the same synset has translations in either BulTreebank WordNet
or Svenska OrdNät then they are also listed after the offset. 
When possible these translations should be used.

## Mapping

The file [mapping.txt](mapping.txt) contains a mapping from the translation lexicon
in GF to the WordNet lexicon. Hopefully later this could be used
to easily port the other languages in the translation lexicon.

## Editing

The Python script [wordnet-ide](wordnet-ide) is a simple IDE for editing the lexicon.
It lets us to see simultaneously the abstract syntax together with the
three concrete syntaxes and the corresponding treebank entries.
All the information could be edited and then saved in the original files.

The IDE automatically compiles the grammar in the beginning but later
when some changes are made only the affected part of the grammar is recompiled
to speed up the compilation. Otherwise compiling the whole lexicon every
time is far too slow.

When changing entries from the lexicon this can affect several entries
in the lexicon. It is a good idea to run the [sanity.hs](sanity.hs) script regularly.
It creates a second file `examples2.txt` with two kinds of annotations:

1. For those entries that are already checked it verifies that the grammar
still produces the same linearization as the one in the treebank. 
If it does'n then the wrong linearizations are marked with `FIX:`.
Either the grammar or the linearization for those entries needs to be fixed.

2. For those entries that are not checked yet, it verifies whether the
grammar produces the same output as the output from Google Translate.
If they match the the abstract syntax tree is marked with `DONE:`.
When that happen then maybe the GF lexicon is now better and it has
managed to produce the right linearization. However, this might be bogus
since the translations from Google Translate are not perfect either.
