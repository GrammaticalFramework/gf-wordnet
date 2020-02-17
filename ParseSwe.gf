--# -path=.:../scandinavian:../abstract:../common:../api
concrete ParseSwe of Parse =
  NounSwe - [PPartNP, UseN2, RelNP, DetNP],
  VerbSwe - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveSwe - [ReflA2, CAdvAP],
  AdverbSwe - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceSwe - [EmbedVP],
  QuestionSwe,
  RelativeSwe,
  ConjunctionSwe,
  PhraseSwe - [UttAP, UttVP],
  IdiomSwe,
  TenseSwe,
  ParseExtendSwe,
  WordNetSwe,
  ConstructionSwe,
  DocumentationSwe
  ** {

flags
  case_sensitive = off;

} ;
