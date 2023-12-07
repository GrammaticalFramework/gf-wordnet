# GF WordNet

1. [The Lexicon](#the-lexicon)
2. [WordNet Domains](#wordnet-domains)
3. [Treebank](#treebank)
4. [VerbNet](#verbnet)
5. [Wikipedia Images](#wikipedia-images)
6. [Browsing and Editing](#browsing-and-editing)
7. [The Python Interface](#the-python-interface)

The GF WordNet is a lexicon based on the [Princeton WordNet](https://wordnet.princeton.edu/) and [Wikidata](https://www.wikidata.org/wiki/Wikidata:Main_Page)
but adapted to integrate with the [GF Resource Grammars Library](https://github.com/GrammaticalFramework/gf-rgl) (RGL). Following the GF model, the lexicon consists
of an abstract syntax with one abstract identifier for each word sense. The concrete syntaxes define the corresponding linearizations in each language.
A synset, then, consists of a set of abstract identifiers instead words. 

The lexicon includes nouns, verb, adjectives and adverbs from WordNet as well as people and place names from Wikidata. Some structural words such as prepositions
and conjunctions are also included. The overal size is summarized in the table bellow:
<table>
     <tr><th>WordNet</th><td>adjectives, nouns, verbs, etc.</td><td>100 thousand</td></tr>
     <tr><th rowspan=3>Wikidata</th><td>Given names</td><td>64 thousand</td></tr>
     <tr>                           <td>Family names</td><td>531 thousand</td></tr>
     <tr>                           <td>Place names</td><td>3.7 million</td></tr>
     <tr><th colspan=2>total                       </th><td>4.3 million</td></tr>
</table>

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

You can also use the WordNet as Python library similar in style to
`nltk.corpus.wordnet`, see [here](python/README.md).
