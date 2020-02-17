--# -path=.:../abstract:../common:../api
concrete ParsePor of Parse =
  NounPor - [PPartNP, UseN2, RelNP, DetNP], --*
  VerbPor - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula], --*
  AdjectivePor - [ReflA2,CAdvAP],
  AdverbPor - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentencePor - [EmbedVP],
  QuestionPor,
  RelativePor,
  ConjunctionPor,
  PhrasePor - [UttAP,UttVP],
  IdiomPor,
  TenseX - [Temp,Pol,SC,Tense,TCond,TFut,TPast,TPres,TTAnt,PNeg,PPos],
  TensePor,
  ParseExtendPor,
  WordNetPor,
  ConstructionPor,
  DocumentationPor
  ** {

flags
  case_sensitive = off;

} ;
