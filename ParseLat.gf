--# -path=.:../abstract:../common:../api
concrete ParseLat of Parse =
  NounLat - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbLat - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveLat - [ReflA2,CAdvAP,AdjOrd],
  AdverbLat - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceLat - [EmbedVP],
  QuestionLat,
  RelativeLat,
  ConjunctionLat,
  PhraseLat - [UttAP, UttVP],
  TenseX - [Adv, AdN, SC],
  ParseExtendLat,
  WordNetLat,
  DocumentationLat ** {

flags
  case_sensitive = off;

} ;
