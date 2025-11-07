--# -path=.:../abstract:../common:../api
concrete ParseJpn of Parse =
  NounJpn - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbJpn - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveJpn - [ReflA2,CAdvAP,AdjOrd],
  AdverbJpn - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceJpn - [EmbedVP],
  QuestionJpn,
  RelativeJpn,
  ConjunctionJpn,
  PhraseJpn - [UttAP, UttVP],
  IdiomJpn,
  TenseX,
  ParseExtendJpn,
  WordNetJpn,
  DocumentationJpn ** {

flags
  case_sensitive = off;

} ;
