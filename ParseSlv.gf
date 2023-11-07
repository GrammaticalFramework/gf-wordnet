--# -path=.:../abstract:../common:../api
concrete ParseSlv of Parse =
  NounSlv - [PPartNP, UseN2, RelNP, DetNP, NumDigits], --*
  VerbSlv - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula], --*
  AdjectiveSlv - [ReflA2,CAdvAP,AdjOrd],
  AdverbSlv - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceSlv - [EmbedVP],
  QuestionSlv,
  ConjunctionSlv,
  PhraseSlv - [UttAP,UttVP],
  IdiomSlv,
  TenseX,
  NamesSlv,
  ParseExtendSlv,
  WordNetSlv,
  DocumentationSlv
  ** {

flags
  case_sensitive = off;

} ;

