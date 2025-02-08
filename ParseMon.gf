--# -path=.:../abstract:../common:../api
concrete ParseMon of Parse =
  NounMon - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbMon - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveMon - [ReflA2,CAdvAP,AdjOrd],
  AdverbMon - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceMon - [EmbedVP],
  QuestionMon,
  RelativeMon,
  ConjunctionMon,
  PhraseMon - [UttAP, UttVP],
  IdiomMon,
  TenseX - [Adv,CAdv,Temp,Tense],
  ParseExtendMon,
  WordNetMon,
  DocumentationMon ** {

flags
  case_sensitive = off;

} ;
