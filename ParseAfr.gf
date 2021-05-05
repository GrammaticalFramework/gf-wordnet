concrete ParseAfr of Parse =
  NounAfr - [PPartNP, UseN2, RelNP, DetNP],
  VerbAfr - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveAfr - [ReflA2, CAdvAP],
  AdverbAfr - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceAfr - [EmbedVP],
  QuestionAfr,
  RelativeAfr,
  PhraseAfr - [UttAP, UttVP],
  IdiomAfr,
  TenseX - [CAdv,IAdv,AdV,SC,Adv],
  ParseExtendAfr,
  WordNetAfr
  ** {

flags
  case_sensitive = off;

} ;
