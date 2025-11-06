--# -path=.:../abstract:../common:../api
concrete ParseGla of Parse =

  NounGla - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbGla - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveGla - [ReflA2,CAdvAP,AdjOrd],
  AdverbGla - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceGla - [UseCl, EmbedVP],
  QuestionGla,
  RelativeGla - [IdRP],
  ConjunctionGla,
  PhraseGla - [UttAP, UttVP],
  IdiomGla,
  TenseX - [Pol,PPos,PNeg,SC,CAdv],
  NamesGla,
  WordNetGla,
  DocumentationGla ** {

flags
  case_sensitive = off;

} ;
