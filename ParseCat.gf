--# -path=.:../abstract:../common:../api
concrete ParseCat of Parse =
  NounCat - [PPartNP, UseN2, RelNP, DetNP],
  VerbCat - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP],
  AdjectiveCat - [ReflA2,CAdvAP],
  AdverbCat - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceCat - [EmbedVP],
  QuestionCat,
  RelativeCat - [IdRP],
  ConjunctionCat,
  PhraseCat - [UttAP, UttVP],
  TextX - [SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,PPos,PNeg],
  IdiomCat,
  TenseX - [SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,PPos,PNeg],
  TenseCat,
  WordNetCat,
  ConstructionCat,
  DocumentationCat
  ** {

-- INJECT

} ;
