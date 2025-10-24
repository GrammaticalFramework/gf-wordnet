--# -path=.:../abstract:../common:../api
concrete ParseEus of Parse =
  NounEus - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbEus - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveEus - [ReflA2,CAdvAP,AdjOrd],
  AdverbEus - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceEus - [EmbedVP],
  QuestionEus,
  RelativeEus,
  ConjunctionEus,
  PhraseEus - [UttAP, UttVP],
  IdiomEus,
  TenseX - [Adv,CAdv],
  WordNetEus ** open ResLav in {

flags
  case_sensitive = off;

} ;
