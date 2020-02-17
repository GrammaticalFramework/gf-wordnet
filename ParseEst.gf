--# -path=.:../abstract:../common:../api
concrete ParseEst of Parse =
  NounEst - [PPartNP, UseN2, RelNP, DetNP], --*
  VerbEst - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula], --*
  AdjectiveEst - [ReflA2,CAdvAP],
  AdverbEst - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceEst - [EmbedVP],
  QuestionEst,
  RelativeEst,
  ConjunctionEst,
  PhraseEst - [UttAP,UttVP],
  IdiomEst,
  TenseX,
  WordNetEst,
  DocumentationEst
  ** {

flags
  case_sensitive = off;

} ;

