concrete ParseSwa of Parse =
  NounSwa - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbSwa - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveSwa - [ReflA2, CAdvAP, AdjOrd],
  AdverbSwa - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceSwa - [EmbedVP],
  QuestionSwa,
  RelativeSwa,
  PhraseSwa - [UttAP, UttVP],
  IdiomSwa,
  TenseX - [CAdv,IAdv,AdV,SC,Adv],
  NamesSwa,
  ParseExtendSwa,
  WordNetSwa
  ** {

flags
  case_sensitive = off;

} ;
