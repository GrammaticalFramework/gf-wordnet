--# -path=.:../abstract:../common:../api
concrete ParseTur of Parse =
  NounTur - [PPartNP, UseN2, RelNP, DetNP, NumDigits], --*
  VerbTur - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula], --*
  AdjectiveTur - [ReflA2,CAdvAP,AdjOrd],
  AdverbTur - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceTur - [EmbedVP],
  QuestionTur,
  RelativeTur,
  ConjunctionTur,
  PhraseTur - [UttAP,UttVP],
  IdiomTur,
  TenseX - [CAdv,AdN],
  NamesTur,
  ParseExtendTur,
  WordNetTur,
  DocumentationTur ** {

flags
  case_sensitive = off;

} ;

