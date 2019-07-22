--# -path=.:../abstract:../common:../api:lib/src/finnish
concrete ParseSpa of Parse =
  NounSpa - [PPartNP, UseN2, RelNP, DetNP],
  VerbSpa - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP],
  AdjectiveSpa - [ReflA2,CAdvAP],
  AdverbSpa - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceSpa - [EmbedVP],
  QuestionSpa,
  RelativeSpa - [IdRP],
  ConjunctionSpa,
  PhraseSpa - [UttAP, UttVP],
  IdiomSpa,
  TenseX - [SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,PPos,PNeg],
  TenseSpa,
  ParseExtendSpa,
  WordNetSpa,
  ConstructionSpa,
  DocumentationSpa
  ** {

-- INJECT

} ;
