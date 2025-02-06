--# -path=.:../abstract:../common:../api
concrete ParseUrd of Parse =
  NounUrd - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbUrd - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveUrd - [ReflA2,CAdvAP,AdjOrd],
  AdverbUrd - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceUrd - [EmbedVP],
  QuestionUrd,
  RelativeUrd,
  ConjunctionUrd,
  PhraseUrd - [UttAP, UttVP],
  IdiomUrd,
  TenseX - [Adv, AdN, SC],
  ParseExtendUrd,
  WordNetUrd,
  DocumentationUrd ** {

flags
  case_sensitive = off;

} ;
