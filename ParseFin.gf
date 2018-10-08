--# -path=.:../abstract:../common:../api:lib/src/finnish
concrete ParseFin of Parse =
  NounFin - [PPartNP, UseN2, RelNP, DetNP],
  VerbFin - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP],
  AdjectiveFin - [ReflA2,CAdvAP],
  AdverbFin - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceFin - [EmbedVP],
  QuestionFin,
  RelativeFin - [IdRP],
  ConjunctionFin,
  PhraseFin - [UttAP, UttVP],
  TextX - [SC],
  IdiomFin,
  TenseX - [SC],
  ParseExtendFin,
  WordNetFin,
  ConstructionFin,
  DocumentationFin
  ** {

-- INJECT

} ;
