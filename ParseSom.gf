concrete ParseSom of Parse =
  NounSom - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbSom - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveSom - [ReflA2, CAdvAP, AdjOrd],
  AdverbSom - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceSom - [EmbedVP],
  QuestionSom,
  RelativeSom,
  ConjunctionSom,
  PhraseSom - [UttAP, UttVP],
  IdiomSom,
  TenseX - [CAdv,IAdv,AdV,Adv,SC],
  NamesSom,
  ParseExtendSom,
  WordNetSom
  ** {

flags
  case_sensitive = off;

} ;
