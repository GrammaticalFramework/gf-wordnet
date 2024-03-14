import os
import pgf
from Parse import __pgf__ as grammar, __file__ as pgf_path
from daison import *
from typing import Optional, Union
from enum import Enum
from dataclasses import dataclass
import atexit

db = openDB(os.path.realpath(os.path.dirname(pgf_path)+"/semantics.db"))
atexit.register(lambda: db.close())

def get_concr(lang):
    return grammar.languages["Parse"+lang.title()]

@dataclass
class Antonym:
    pass

@dataclass
class Hypernym:
    pass

@dataclass
class InstanceHypernym:
    pass

@dataclass
class Hyponym:
    pass

@dataclass
class InstanceHyponym:
    pass

class HolonymyType(Enum):
    Member = 1
    Substance = 2
    Part = 3

@dataclass
class Holonym:
    type: HolonymyType

@dataclass
class Meronym:
    type: HolonymyType

@dataclass
class Attribute:
    pass

class DomainType(Enum):
    Topic = 1
    Region = 2
    Usage = 3

@dataclass
class DomainOfSynset:
    type: DomainType

@dataclass
class MemberOfDomain:
    type: DomainType

@dataclass
class Entailment:
    pass

@dataclass
class Cause:
    pass

@dataclass
class AlsoSee:
    pass

@dataclass
class VerbGroup:
    pass

@dataclass
class SimilarTo:
    pass

@dataclass
class Derived:
    pass

@dataclass
class Participle:
    pass

PointerSymbol = Union[Antonym,Hypernym,InstanceHypernym,Hyponym,InstanceHyponym,Holonym,Meronym,Attribute,DomainOfSynset,MemberOfDomain,Entailment,Cause,AlsoSee,VerbGroup,SimilarTo,Derived,Participle]

@dataclass
class Synset:
    synsetOffset: str
    pointers: list[tuple[PointerSymbol,int]]
    children: list[tuple[int,int]]
    gloss: str

    def __repr__(self):
        return "Synset("+repr(self.synsetOffset)+")"

    def definition(self) -> str:
        return self.gloss

    def lexemes(self):
        result = []
        with db.run("r") as t:
            for lexeme_id, lexeme in t.indexCursor(lexemes_synset, self.id):
                lexeme.id = lexeme_id
                result.append(lexeme)
        return result

    def linearizations(self, lang: str) -> list[str]:
        concr = get_concr(lang)
        result = set()
        with db.run("r") as t:
            for lexeme_id, lexeme in t.indexCursor(lexemes_synset, self.id):
                if concr.hasLinearization(lexeme.lex_fun):
                    result.add(concr.linearize(pgf.ExprFun(lexeme.lex_fun)))
        return list(result)

    def examples(self) -> list[pgf.Expr]:
        result = []
        with db.run("r") as t:
            for lexeme_id, lexeme in t.indexCursor(lexemes_synset, self.id):
                for _, (example,_) in t.indexCursor(examples_fun, lexeme.lex_fun):
                    result.append(example)
        return result

    def __pointers__(self, tp) -> list:
        result = []
        with db.run("r") as t:
            for ptr,id in self.pointers:
                if isinstance(ptr, tp):
                    for synset in t.cursor(synsets, id):
                        result.append(synset)
        return result

    def __pointers2__(self, tp, tp2) -> list:
        result = []
        with db.run("r") as t:
            for ptr,id in self.pointers:
                if isinstance(ptr, tp) and ptr.type == tp2:
                    for synset in t.cursor(synsets, id):
                        result.append(synset)
        return result

    def hyponyms(self) -> list:
        return self.__pointers__(Hyponym)

    def instance_hyponyms(self) -> list:
        return self.__pointers__(InstanceHyponym)

    def full_hyponyms(self) -> list:
        result = []
        with db.run("r") as t:
            for start, end in self.children:
                for id in range(start, end+1):
                    for synset in t.cursor(synsets, id):
                        synset.id = id
                        result.append(synset)
        return result

    def hypernyms(self) -> list:
        return self.__pointers__(Hypernym)

    def instance_hypernyms(self) -> list:
        return self.__pointers__(InstanceHypernym)

    def member_holonyms(self) -> list:
        return self.__pointers2__(Holonym,HolonymyType.Member)

    def substance_holonyms(self) -> list:
        return self.__pointers2__(Holonym,HolonymyType.Substance)

    def part_holonyms(self) -> list:
        return self.__pointers2__(Holonym,HolonymyType.Part)

    def member_meronyms(self) -> list:
        return self.__pointers2__(Meronym,HolonymyType.Member)

    def substance_meronyms(self) -> list:
        return self.__pointers2__(Meronym,HolonymyType.Substance)

    def part_meronyms(self) -> list:
        return self.__pointers2__(Holonym,HolonymyType.Part)

    def _collect_hypernyms(self,stats,level,i,n,t):
        for ptr,id in self.pointers:
            if isinstance(ptr, Hypernym) or isinstance(ptr, InstanceHypernym):
                stat = stats.get(id)
                if stat:
                    hypernym, levels = stat
                    if levels[i] == None or levels[i] > level:
                        levels[i] = level
                        hypernym._collect_hypernyms(stats,level+1,i,n,t)
                else:
                    for hypernym in t.cursor(synsets, id):
                        hypernym.id = id
                        levels = [None]*n
                        levels[i] = level
                        stats[id] = (hypernym,levels)
                        hypernym._collect_hypernyms(stats,level+1,i,n,t)

    def store(self):
        with db.run("w") as t:
            self.id = t.store(synsets, self.id, self)

synsets = table("synsets",Synset)

synsets_offset = index(synsets,"offset",lambda synset: synset.synsetOffset,str)
synsets.addIndex(synsets_offset)

class Status(Enum):
    Guessed = 1
    Unchecked = 2
    Checked = 3

@dataclass
class Lexeme:
    lex_fun: str
    lex_prob: float
    status: list[tuple[str,Status]]
    synset_id: Optional[int]
    domain_ids: list[int]
    example_ids: list[int]
    frame_ids: list[int]
    lex_pointers: list[tuple[PointerSymbol,int]]
    images: list[tuple[str,str,str]]

    def __repr__(self):
        return "Lexeme("+repr(self.lex_fun)+")"

    def linearization(self,lang: str) -> str:
        concr = get_concr(lang)
        if concr.hasLinearization(self.lex_fun):
            return concr.linearize(pgf.ExprFun(self.lex_fun))
        else:
            return None

    def function(self) -> str:
        return self.lex_fun;

    def expression(self) -> pgf.Expr:
        return pgf.ExprFun(self.lex_fun);

    def links(self) -> list[tuple[str,str,str]]:
        """ Returns a list of triples with qid, wikipage, image """
        return self.images

    def qid(self) -> str:
        for qid,_,_ in self.images:
            return qid
        return None

    def synset(self) -> Optional[Synset]:
        if not self.synset_id:
            return None
        with db.run("r") as t:
            for synset in t.cursor(synsets, self.synset_id):
                synset.id = self.synset_id
                return synset

    def examples(self) -> list[pgf.Expr]:
        result = []
        with db.run("r") as t:
            for _, (example,_) in t.indexCursor(examples_fun, self.lex_fun):
                result.append(example)
        return result

    def __pointers__(self, tp) -> list:
        result = []
        with db.run("r") as t:
            for ptr,id in self.lex_pointers:
                if isinstance(ptr, tp):
                    for lexeme in t.cursor(lexemes, id):
                        result.append(lexeme)
        return result

    def antonyms(self) -> list:
        return self.__pointers__(Antonym)

    def participle(self) -> list:
        return self.__pointers__(Participle)

    def alsosee(self) -> list:
        return self.__pointers__(AlsoSee)

    def derived(self) -> list:
        return self.__pointers__(Derived)

    def prob(self) -> float:
        return grammar.functionProbability(self.lex_fun)

    def store(self):
        with db.run("w") as t:
            self.id = t.store(lexemes, self.id, self)

lexemes = table("lexemes",Lexeme)

lexemes_fun = index(lexemes,"fun",lambda lexeme: lexeme.lex_fun,str)
lexemes.addIndex(lexemes_fun)

lexemes_synset = maybeIndex(lexemes,"synset",lambda lexeme: lexeme.synset_id,int)
lexemes.addIndex(lexemes_synset)

lexemes_domain = listIndex(lexemes,"domain",lambda lexeme: lexeme.domain_ids,int)
lexemes.addIndex(lexemes_domain)

lexemes_frame = listIndex(lexemes,"frames",lambda lexeme: lexeme.frame_ids,int)
lexemes.addIndex(lexemes_frame)

lexemes_qid = listIndex(lexemes,"qid",lambda lexeme: set([qid for (qid,_,_) in lexeme.images]),str)
lexemes.addIndex(lexemes_qid)

FrameInstance = tuple[int,list[tuple[str,int]]]

examples = table("examples",tuple[pgf.Expr,list[FrameInstance]])

examples_fun = listIndex(examples,"fun",lambda e: set(exprFunctions(e[0])),str)
examples.addIndex(lexemes_fun)

def get_synset(id : str) -> Optional[Synset]:
    with db.run("r") as t:
        if id[:1] == "Q":
            for lexeme_id, lexeme in t.indexCursor(lexemes_qid, id):
                for synset in t.cursor(synsets, lexeme.synset_id):
                    synset.id = lexeme.synset_id
                    return synset
        else:
            for synset_id, synset in t.indexCursor(synsets_offset, id):
                synset.id = synset_id
                return synset
    return None

def get_synsets(lang : str, word : str, cat=None) -> list[Synset]:
    result = []
    synset_ids = set()
    with db.run("r") as t:
        for fun,_,_ in get_concr(lang).lookupMorpho(word):
            if cat != None and cat != grammar.functionType(fun).cat:
                continue

            for lexeme_id, lexeme in t.indexCursor(lexemes_fun, fun):
                if lexeme.synset_id and lexeme.synset_id not in synset_ids:
                    synset_ids.add(lexeme.synset_id)
                    for synset in t.cursor(synsets, lexeme.synset_id):
                        synset.id = lexeme.synset_id
                        result.append(synset)
    return result

def get_lexeme(fun : str) -> Optional[Lexeme]:
    with db.run("r") as t:
        for lexeme_id, lexeme in t.indexCursor(lexemes_fun, fun):
            lexeme.id = lexeme_id
            return lexeme
    return None

def get_wikilexemes(qid : str) -> Lexeme:
    result = []
    with db.run("r") as t:
        for lexeme_id, lexeme in t.indexCursor(lexemes_qid, qid):
            lexeme.id = lexeme_id
            result.append(lexeme)
    return result

def get_lexemes(lang : str, word : str, cat=None) -> list[Lexeme]:
    result = []
    lexeme_ids = set()
    with db.run("r") as t:
        for fun,_,_ in get_concr(lang).lookupMorpho(word):
            if cat != None and cat != grammar.functionType(fun).cat:
                continue

            for lexeme_id, lexeme in t.indexCursor(lexemes_fun, fun):
                if lexeme_id not in lexeme_ids:
                    lexeme_ids.add(lexeme_id)
                    lexeme.id = lexeme_id
                    result.append(lexeme)
    return result

def synonyms(lang : str, word : str, cat=None) -> list[list[str]]:
    result = []
    synset_ids = set()
    concr = get_concr(lang)
    with db.run("r") as t:
        for fun,_,_ in concr.lookupMorpho(word):
            if cat != None and cat != grammar.functionType(fun).cat:
                continue

            for lexeme_id, lexeme in t.indexCursor(lexemes_fun, fun):
                if lexeme.synset_id and lexeme.synset_id not in synset_ids:
                    synonyms = set()
                    for synonym_id, synonym in t.indexCursor(lexemes_synset, lexeme.synset_id):
                        if synonym.lex_fun != fun and concr.hasLinearization(synonym.lex_fun):
                            lin = concr.linearize(pgf.ExprFun(synonym.lex_fun))
                            synonyms.add(synonym.lex_fun)
                    if synonyms:
                        result.append(synonyms)
                    synset_ids.add(lexeme.synset_id)
    return result

def lowest_common_hypernyms(*synsets):
    stats = {}
    with db.run("r") as t:
        for i,synset in enumerate(synsets):
            stat = stats.get(synset.id)
            if stat:
                levels = stat[1]
                levels[i] = 0
            else:
                levels = [None]*len(synsets)
                levels[i] = 0
                stats[synset.id] = (synset,levels)
                synset._collect_hypernyms(stats,1,i,len(synsets),t)

    common = [(synset,sum(levels)) for synset,levels in stats.values() if all(level != None for level in levels)]
    min_dist = min(dist for _,dist in common)
    return [synset for synset,dist in common if dist==min_dist]

def shortest_path_distance(synset1, synset2):
    if synset1.id == synset2.id:
        return 0
    stats = {synset1.id: (synset1, [0,None]),
             synset2.id: (synset2, [None,0])}
    with db.run("r") as t:
        synset1._collect_hypernyms(stats,1,0,2,t)
        synset2._collect_hypernyms(stats,1,1,2,t)
    return min(sum(levels) for synset,levels in stats.values() if all(level != None for level in levels))

def path_similarity(synset1, synset2):
    return 1/(shortest_path_distance(synset1, synset2)+1);
