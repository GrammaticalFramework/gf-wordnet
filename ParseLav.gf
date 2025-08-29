--# -path=.:../abstract:../common:../api
concrete ParseLav of Parse =
  NounLav - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbLav - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveLav - [ReflA2,CAdvAP,AdjOrd],
  AdverbLav - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceLav - [EmbedVP],
  QuestionLav,
  RelativeLav,
  ConjunctionLav,
  PhraseLav - [UttAP, UttVP],
  IdiomLav,
  TenseX - [Adv,CAdv],
  NamesLav,
  ParseExtendLav,
  WordNetLav,
  DocumentationLav ** open ResLav in {

flags
  case_sensitive = off;

} ;
