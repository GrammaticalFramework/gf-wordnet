--# -path=.:../abstract:../common:../api
concrete ParseHin of Parse =
  NounHin - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbHin - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveHin - [ReflA2,CAdvAP,AdjOrd],
  AdverbHin - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceHin - [EmbedVP],
  QuestionHin,
  RelativeHin,
  ConjunctionHin,
  PhraseHin - [UttAP, UttVP],
  IdiomHin,
  TenseX - [Adv, AdN, SC],
  ParseExtendHin,
  WordNetHin,
  DocumentationHin ** {

flags
  case_sensitive = off;

} ;
