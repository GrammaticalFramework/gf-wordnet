concrete ParseSwa of Parse =
  NounSwa - [PPartNP, UseN2, RelNP, DetNP],
  VerbSwa - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveSwa - [ReflA2, CAdvAP],
  AdverbSwa - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceSwa - [EmbedVP],
  QuestionSwa,
  RelativeSwa,
  PhraseSwa - [UttAP, UttVP],
  IdiomSwa,
  TenseX - [CAdv,IAdv,AdV,SC,Adv],
  ParseExtendSwa,
  WordNetSwa
  ** {

flags
  case_sensitive = off;

} ;
