--# -path=.:../abstract:../common
concrete ParseBul of Parse =
  NounBul - [PPartNP, UseN2, RelNP, DetNP],
  VerbBul - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP],
  AdjectiveBul - [ReflA2, CAdvAP],
  AdverbBul - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceBul - [EmbedVP],
  QuestionBul,
  RelativeBul,
  ConjunctionBul,
  PhraseBul - [UttAP, UttVP],
  IdiomBul,
  TenseX - [CAdv,IAdv,AdV,SC],
  ParseExtendBul,
  ConstructionBul,
  WordNetBul,
  DocumentationBul
  ** {

flags
  case_sensitive = off;

} ;
