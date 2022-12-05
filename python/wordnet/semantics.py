from daison import *
from typing import Optional, Union
from enum import Enum
from dataclasses import dataclass

@dataclass
class Synset:
    synsetOffset: str
    pointers: list[tuple[int,int]]
    children: list[tuple[int,int]]
    gloss: str
    images: list[tuple[str,str,str]]

synsets = table("synsets",Synset)
synsets_qid = listIndex(synsets,"qid",lambda synset: set([qid for (qid,_,_) in synset.images]),str)
synsets.addIndex(synsets_qid)

class Status(Enum):
    Guessed = 1
    Unchecked = 2
    Checked = 3

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
class Lexeme:
    lex_fun: str
    lex_prob: float
    status: list[tuple[str,Status]]
    synset: Optional[int]
    domain_ids: list[int]
    example_ids: list[int]
    frame_ids: list[int]
    lex_pointers: list[tuple[PointerSymbol,int]]

lexemes = table("lexemes",Lexeme)

lexemes_fun = index(lexemes,"fun",lambda lexeme: lexeme.lex_fun,str)
lexemes.addIndex(lexemes_fun)

lexemes_synset = maybeIndex(lexemes,"synset",lambda lexeme: lexeme.synset,int)
lexemes.addIndex(lexemes_synset)

lexemes_domain = listIndex(lexemes,"domain",lambda lexeme: lexeme.domain_ids,int)
lexemes.addIndex(lexemes_domain)

lexemes_frame = listIndex(lexemes,"frames",lambda lexeme: lexeme.frame_ids,int)
lexemes.addIndex(lexemes_frame)
