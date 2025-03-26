--# -path=.:../abstract:../common:../api
concrete ParseRon of Parse =
  NounRon - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbRon - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveRon - [ReflA2,CAdvAP,AdjOrd],
  AdverbRon - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceRon - [EmbedVP],
  QuestionRon,
  RelativeRon,
  ConjunctionRon,
  PhraseRon - [UttAP, UttVP],
  IdiomRon,
  TenseX - [CAdv,SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,PPos,PNeg],
  TenseRon,
  NamesRon,
  ParseExtendRon,
  WordNetRon,
  DocumentationRon
  ** {

flags
  case_sensitive = off;

} ;
