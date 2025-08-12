--# -path=.:../abstract:../common:../api
concrete ParseHun of Parse =
  NounHun - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbHun - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveHun - [ReflA2,CAdvAP,AdjOrd],
  AdverbHun - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceHun - [EmbedVP],
  QuestionHun,
  RelativeHun,
  ConjunctionHun,
  PhraseHun - [UttAP, UttVP],
  IdiomHun,
  TenseX - [Adv, AdN, SC],
  ParseExtendHun,
  WordNetHun ** {

flags
  case_sensitive = off;

} ;
