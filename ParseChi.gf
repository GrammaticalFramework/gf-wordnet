--# -path=.:../abstract:../common:../api
concrete ParseChi of Parse =
  NounChi - [PPartNP, UseN2, RelNP, DetNP], --*
  VerbChi - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP], --*
  AdjectiveChi - [ReflA2,CAdvAP],
  AdverbChi - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceChi - [EmbedVP],
  QuestionChi,
  RelativeChi,
  ConjunctionChi,
  PhraseChi - [UttAP,UttVP],
  TextX - [Temp,Pol,SC,Tense,Adv,Ant,TCond,TFut,TPast,TPres,TTAnt,AAnter,ASimul,PNeg,PPos],
  IdiomChi,
  TenseX - [Temp,Pol,SC,Tense,Adv,Ant,TCond,TFut,TPast,TPres,TTAnt,AAnter,ASimul,PNeg,PPos],
  WordNetChi,
  ConstructionChi,
  DocumentationChi
  ** {

-- INJECT

} ;
