--# -path=.:../scandinavian:../abstract:../common:../api
concrete ParseIce of Parse =
  NounIce - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbIce - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveIce - [ReflA2, CAdvAP, AdjOrd],
  AdverbIce - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceIce - [EmbedVP],
  QuestionIce,
  RelativeIce,
  ConjunctionIce,
  PhraseIce - [UttAP, UttVP],
  IdiomIce,
  TenseX,
  ParseExtendIce,
  WordNetIce,
  DocumentationIce
  ** {

flags
  case_sensitive = off;

} ;
