--# -path=.:../abstract:../common:../api
concrete ParseFre of Parse =
  NounFre - [PPartNP, UseN2, RelNP, DetNP],
  VerbFre - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveFre - [ReflA2,CAdvAP],
  AdverbFre - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceFre - [EmbedVP],
  QuestionFre,
  RelativeFre - [IdRP],
  ConjunctionFre,
  PhraseFre - [UttAP, UttVP],
  IdiomFre,
  TenseX - [SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,PPos,PNeg],
  TenseFre,
  ParseExtendFre,
  WordNetFre,
  ConstructionFre,
  DocumentationFre
  ** {

flags
  case_sensitive = off;

} ;
